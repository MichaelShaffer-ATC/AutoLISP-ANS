;; URL AND PRIVATE KEY FOR DATA METRICS API ACCESS

;;; TESTING LOCALLY ;;;
; (setq *ECHO:Log-URL* "https://localhost:5001/lisp-logs")	;; TESTING LOCAL PORT
; (setq *ECHO:Log-Key* nil) 								;; API KEY LEFT UN-SET

(setq *ECHO:Log-URL* "https://localhost:5001/lisp-logs")	;; PRODUCTION URL
(setq *ECHO:Log-Key* "PRODUCTION-API-KEY-STRING") 			;; PRODUCTION API KEY

(if (not (boundp '*ECHO:Log-Script*))		(setq *ECHO:Log-Script*			"lisp"))
(if (not (boundp '*ECHO:Log-Command*))		(setq *ECHO:Log-Command*		nil))	; LABEL; FALLS BACK TO 'CMDNAMES'
(if (not (boundp '*ECHO:Log-MaxBuffer*))	(setq *ECHO:Log-MaxBuffer*		25))	; AUTO-FLUSH WHEN THE BUFFER REACHES THIS MANY RECORDS
(if (not (boundp '*ECHO:Log-MaxCapacity*))	(setq *ECHO:Log-MaxCapacity*	1000))	; HARD CAP; OLDEST RECORDS ARE DROPPED PAST THIS
(if (not (boundp '*ECHO:Log-Buffer*))		(setq *ECHO:Log-Buffer*			nil))
(if (not (boundp '*ECHO:Log-InFlight*))		(setq *ECHO:Log-InFlight*		nil))	; ASYNC REQUESTS AWAITING REAP