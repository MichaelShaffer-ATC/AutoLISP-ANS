;;; ===========================================================================
;;; ECHO:Log.lsp -- ship structured logs from AutoLISP into Echo's OpenSearch
;;; ---------------------------------------------------------------------------
;;; Buffers log records and POSTs them as a JSON batch to Echo's /lisp-logs
;;; endpoint. Works on full AutoCAD for Windows (uses an MSXML/WinHttp COM HTTP
;;; object -- see ECHO:HTTP-CreateObject); it does NOT require Echo installed.
;;;
;;; This file is intentionally pure ASCII: AutoCAD's LISP reader aborts
;;; silently on non-ASCII bytes.
;;;
;;; -------------------------------- SETUP ------------------------------------
;;; 1. Set the endpoint URL below (or set *ECHO:Log-URL* before loading).
;;;    Internal API host example: "https://api.echo.ans-team.com/lisp-logs"
;;; 2. If the server requires a key, set *ECHO:Log-Key* to the shared key.
;;; 3. (load "ECHO:Log.lsp")
;;; 4. (ECHO:SetScriptName "annotate.lsp")   ; optional, labels the source
;;;
;;; -------------------------------- USAGE ------------------------------------
;;;   (ECHO:Log "info"  "started run")
;;;   (ECHO:Log "error" "no target layer found")
;;;   ;; with a duration (ms) and extra attributes:
;;;   (ECHO:Log-Verbose "info" "annotated sheets" 142.5
;;;                (list (cons "sheets" 12) (cons "layer" "S-ANNO")))
;;;   (ECHO:Log-Flush)          ; force-send now (also runs at buffer full /
;;;                             ; on error+fatal automatically)
;;;
;;; Records buffer in memory and flush when the buffer fills, when an
;;; error/fatal is logged, or when you call (ECHO:Log-Flush). Flushing is
;;; fire-and-forget: the POST is sent asynchronously and never waited on, so a
;;; down/slow service can't stall the drawing -- but there is NO delivery
;;; confirmation and NO retry (the buffer is dropped once fired). Nothing here
;;; ever throws into your drawing script -- every network call is guarded.
;;; ===========================================================================

; (vl-load-com) ;; NOT TECHNICALLY NEEDED SINCE AUTOLOADER HANDLES THIS

;; ---- Configuration (override before or after load as needed) ---------------
; (if (not (boundp '*ECHO:Log-URL*))			(setq *ECHO:Log-URL*			"https://api.echo.ans-team.com/lisp-logs"))
; (if (not (boundp '*ECHO:Log-Key*))			(setq *ECHO:Log-Key*			nil))	;; STRING, OR NIL IF THE SERVER USES AN IP ALLOWLIST ONLY
; (if (not (boundp '*ECHO:Log-Script*))			(setq *ECHO:Log-Script*			"lisp"))
; (if (not (boundp '*ECHO:Log-Command*))		(setq *ECHO:Log-Command*		nil))	;; LABEL; FALLS BACK TO 'CMDNAMES'
; (if (not (boundp '*ECHO:Log-MaxBuffer*))		(setq *ECHO:Log-MaxBuffer*		25))	;; AUTO-FLUSH WHEN THE BUFFER REACHES THIS MANY RECORDS
; (if (not (boundp '*ECHO:Log-MaxCapacity*))	(setq *ECHO:Log-MaxCapacity*	1000))	;; HARD CAP; OLDEST RECORDS ARE DROPPED PAST THIS
; (if (not (boundp '*ECHO:Log-Buffer*))			(setq *ECHO:Log-Buffer*			nil))
; (if (not (boundp '*ECHO:Log-InFlight*))		(setq *ECHO:Log-InFlight*		nil))	;; ASYNC REQUESTS AWAITING REAP
;; THESE ARE ALL SET WITHIN THE COMPILED CONFIG FILE

;; ---- Public: label the originating script ----------------------------------
(defun ECHO:SetScriptName ( scr )
	(setq *ECHO:Log-Script* (if (null scr) "lisp" scr))
	(princ)
)

;; ---- Public: label the emitting command (else the active command is used) --
(defun ECHO:SetCommandName ( cmd )
	(setq *ECHO:Log-Command* cmd)
	(princ)
)

;; ---- Public: log with just a level + message -------------------------------
(defun ECHO:Log ( lvl msg )
	(ECHO:Log-Verbose lvl msg nil nil)
)

;; ---- Public: log with an optional duration (ms) and extra attributes -------
;; dur	: a number of milliseconds, or nil.
;; atrs	: an association list of extra string attributes, either dotted
;;            pairs  (("host" . "CAD-07") ("run" . 42))  or 2-element lists
;;            (("host" "CAD-07") ("run" 42)) ; values are coerced to strings.
;; Example:
;;   (ECHO:Log-Verbose "info" "annotated sheets" 142.5
;;                (list (cons "sheets" 12) (cons "layer" "S-ANNO")))
(defun ECHO:Log-Verbose ( lvl msg dur atrs )
	(setq lvl (if (null lvl) "info" lvl))
	(setq *ECHO:Log-Buffer*
		(cons (ECHO:Log-Record lvl msg dur atrs) *ECHO:Log-Buffer*)
	)
	(ECHO:DropOlderRecords)
	(if (or
			(>= (length *ECHO:Log-Buffer*) *ECHO:Log-MaxBuffer*)
			(member (strcase lvl) '("ERROR" "FATAL"))
		)
		(ECHO:Log-Flush)
	)
	(princ)
)

;; ---- Internal: create the first available HTTP COM object ------------------
;; WinHttp.WinHttpRequest is NOT registered on every machine ("Class not
;; registered"); MSXML6 ServerXMLHTTP ships with Windows and almost always is,
;; so it is tried first. Returns (progid . object) or nil. The method/property
;; names used below (open, send, status, setRequestHeader, setTimeouts) are
;; common to these objects and IDispatch is case-insensitive, so one code path
;; drives whichever provider is available.
(defun ECHO:HTTP-CreateObject ( / ids obj hit )
	(setq ids
		(list
			"MSXML2.ServerXMLHTTP.6.0"
			"MSXML2.ServerXMLHTTP"
			"WinHttp.WinHttpRequest.5.1"
			"MSXML2.XMLHTTP.6.0"
			"MSXML2.XMLHTTP"
		)
	)
	(while (and ids (not hit))
		(setq obj (vl-catch-all-apply 'vlax-create-object (list (car ids))))
		(if (vl-catch-all-error-p obj)
			(setq ids (cdr ids))
			(setq hit (cons (car ids) obj))
		)
	)
	hit
)

;; ---- Public: fire buffered records asynchronously (non-blocking) ------------
;; Fire-and-forget: opens the POST in ASYNC mode and returns immediately without
;; waiting for or reading the response, so a down or slow service can NEVER stall
;; the drawing. Trade-off: there is no delivery confirmation and no retry -- the
;; buffer is dropped once fired. The COM object is kept referenced (so the
;; background request can finish) and released later by ECHO:Log-Reap.
(defun ECHO:Log-Flush ( / body http sent )
	(ECHO:Log-Reap)   ; release any earlier async requests that have finished
	(if (and *ECHO:Log-Buffer* *ECHO:Log-URL*)
		(progn
			(setq body (strcat "[" (ECHO:StringJoin (reverse *ECHO:Log-Buffer*) ",") "]"))
			(setq http (cdr (ECHO:HTTP-CreateObject)))
			(if http
				(progn
					(setq sent
						(vl-catch-all-apply
							'(lambda ( )
								;; async = true -> send returns immediately; we never wait/read.
								(vlax-invoke-method http 'open "POST" *ECHO:Log-URL* :vlax-true)
								;; Bounds the background request; optional (plain XMLHTTP lacks it).
								(vl-catch-all-apply '(lambda ( ) (vlax-invoke-method http 'setTimeouts 1000 1000 2000 3000)))
								(vlax-invoke-method http 'setRequestHeader "Content-Type" "application/json")
								(if *ECHO:Log-Key*
									(vlax-invoke-method http 'setRequestHeader "X-Echo-Lisp-Key" *ECHO:Log-Key*)
								)
								(vlax-invoke-method http 'send body)
								t
							)
						)
					)
					;; Keep the object alive so the async POST can complete; releasing it
					;; now could abort the request. Reaped on the next flush.
					(if (vl-catch-all-error-p sent)
						(vl-catch-all-apply '(lambda ( ) (vlax-release-object http)))
						(setq *ECHO:Log-InFlight* (cons http *ECHO:Log-InFlight*))
					)
				)
			)
			;; Fire-and-forget: drop the buffer whether or not the send launched.
			(setq *ECHO:Log-Buffer* nil)
		)
	)
	(princ)
)

;; ---- Internal: release async requests that have finished (or pile up) -------
(defun ECHO:Log-Reap ( / kept rs )
	(setq kept nil)
	(foreach h *ECHO:Log-InFlight*
		(setq rs (vl-catch-all-apply '(lambda ( ) (vlax-get-property h 'readyState))))
		(if (and (not (vl-catch-all-error-p rs)) (numberp rs) (>= rs 4))
			(vl-catch-all-apply '(lambda ( ) (vlax-release-object h)))   ; completed -> free it
			(setq kept (cons h kept))
		)
	) ; still running / no readyState
	;; Safety cap: never let requests accumulate; force-release the oldest.
	(while (> (length kept) 8)
		(vl-catch-all-apply '(lambda ( ) (vlax-release-object (last kept))))
		(setq kept (reverse (cdr (reverse kept))))
	)
	(setq *ECHO:Log-InFlight* kept)
	(princ)
)

;; ---- Internal: build one JSON object for a record --------------------------
(defun ECHO:Log-Record ( lvl msg dur atrs / json k v aprs cmd )
	(setq json
		(strcat
			"{\"level\":\""		(ECHO:StringEscape lvl)
			"\",\"message\":\""	(ECHO:StringEscape msg)
			"\",\"script\":\""	(ECHO:StringEscape *ECHO:Log-Script*)
			"\",\"drawing\":\""	(ECHO:StringEscape (getvar "DWGNAME"))
			"\",\"user\":\""	(ECHO:StringEscape (getvar "ONLINEUSERNAME"))
			"\""
		)
	)
	;; command: explicit *ECHO:Log-Command* wins; else the active command (CMDNAMES)
	(setq cmd (if (null *ECHO:Log-Command*) (getvar "CMDNAMES") *ECHO:Log-Command*))
	(if (and cmd (> (strlen cmd) 0))
		(setq json (strcat json ",\"command\":\"" (ECHO:StringEscape cmd) "\""))
	)
	;; Optional numeric duration_ms (unquoted JSON number).
	(if (and dur (numberp dur))
		(setq json (strcat json ",\"duration_ms\":" (vl-princ-to-string dur)))
	)
	;; atrs object: caller-supplied pairs only. No client timestamp -- the server
	;; stamps the authoritative receive time.
	(foreach pair atrs
		(if (and pair (listp pair) (car pair))
			(progn
				(setq k (car pair) v (cdr pair))
				;; Accept both ("k" . "v") and ("k" "v") shapes.
				(if (listp v) (setq v (car v)))
				(setq aprs
					(cons
						(strcat
							"\""
							(ECHO:StringEscape (if (null k) "" (vl-princ-to-string k)))
							"\":\"" (ECHO:StringEscape (if (null v) "" (vl-princ-to-string v)))
							"\""
						)
						aprs
					) ;; CREATE ATTRIBUTE PAIRS (KEY . VALUE)
				)
			)
		)
	)
	(strcat json ",\"attrs\":{" (ECHO:StringJoin (reverse aprs) ",") "}}")
)

;; ---- Internal: JSON-escape and force ASCII ---------------------------------
;; Escapes backslash and quote, blanks control chars, and replaces any byte
;; above 126 with '?' so the payload is guaranteed valid ASCII JSON.
(defun ECHO:StringEscape ( s / i c code out )
	(if (not s) (setq s ""))
	(setq out "" i 1)
	(while (<= i (strlen s))
		(setq c (substr s i 1) code (ascii c))
		(cond
			((= c "\\") (setq out (strcat out "\\\\")))
			((= c "\"") (setq out (strcat out "\\\"")))
			((< code 32) (setq out (strcat out " ")))
			((> code 126) (setq out (strcat out "?")))
			( t (setq out (strcat out c)))
		)
		(setq i (1+ i))
	)
	out
)

;; ---- Internal: join a list of strings with a separator ---------------------
(defun ECHO:StringJoin ( lst sep / out first )
	(setq out "" first t)
	(foreach x lst
		(setq out (strcat out (if first "" sep) x) first nil)
	)
	out
)

;; ---- Internal: drop oldest records past the hard cap -----------------------
(defun ECHO:DropOlderRecords ( )
	(while (> (length *ECHO:Log-Buffer*) *ECHO:Log-MaxCapacity*)
		;; buffer is newest-first, so the oldest is the last element.
		(setq *ECHO:Log-Buffer* (reverse (cdr (reverse *ECHO:Log-Buffer*))))
	)
)

;; ---- Convenience test command ----------------------------------------------
(defun c:ECHOLOGTEST ( )
	(ECHO:Log "info" "ECHO:Log test message")
	(ECHO:Log-Flush)
	(princ "\nSent test log to Echo.")
	(princ)
)

; (princ "\nECHO:Log.lsp loaded. Use (ECHO:Log \"info\" \"message\"), or ECHOLOGTEST.")
;; FUNCTION LOADING HANDLED IN AUTOLOADER

(princ)
