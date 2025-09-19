;; FUNCTION USED TO INTERACT WITH BOD EXCEL OUTPUT FILE AND INSERT NECESSARY DATA INFORMATION INTO AUTOCAD DRAWING
;; DATA IS EXCTRACTED BASED ON USER SELECTION OF BOD, TEXT IS FORMATTED TO MATCH TAGS CREATED FROM BOD DATA

;; FOR ANY UN-DEFINED FUNCTIONS IN THIS FILE:
;; PLEASE REFER TO "LIB - MSXL Library Functions.lsp" FOR EXCEL RELATED FUNCTIONS (MSXL:)
;; OR "LIB - STD Sub Functions.lsp" FOR ALL OTHER UN-DEFINED FUNCTIONS WITHIN THIS SCOPE

(defun c:BODT ( / )
	(c:BODTRANSFER)
)

(defun c:BODTRANSFER
	(
		/
		;Functions
		*error*
		ConvertNumberToString CreateDataMatrix
		FindText ReplaceTextInString GenerateKeyPattern
		;Variables
		dir fl nms shts txt
		;Excel Based
		xl wb wss ws
	)
	
	(defun *error* ( msg )
		(if (not (member msg (list "Function cancelled" "quit / exit abort")))
			(vl-bt)
		)
		;; !!MAKE SURE TO RELEASE ALL EXCEL RELATED OBJECTS!!
		(ObjectRelease (list rgx cell wss ws wb))
		(if (and (= (type xl) 'VLA-OBJECT) (not (vl-object-released-p xl)))
			(msxl:CloseExcel xl)
		)
		(gc)
		(princ (strcat "\nAn error occurred: " msg))
	)
	;; ERROR CATCHING FUNCTION
	
	
	
	(defun ConvertNumberToString ( num )
		(cond
			((= (type num) 'REAL)
				(rtos num 2 (- (strlen (vl-string-right-trim "0" (rtos (abs (- num (fix num))) 2 8))) 2))
			)
			((= (type num) 'INT)
				(itoa num)
			)
			( t
				num
			) ;; VALUE IS NOT NUMERIC
		)
	)
	;; CONVERTS NUMERIC VALUES TO STRINGS
	;; MATHEMATICALLY FIGURES OUT PRECISION FOR REAL NUMBERS BASED ON ENDING DIGITS, DOES NOT INCLUDE TRAILING 0'S:
	;; SUBRACTS 2 FROM THE TOTAL LENGTH OF THE FIXED STRING
	;; THE LEADING ZERO "0" AND DECIMAL "." COUNT AS TWO CHARACTERS FOR ALL FIXED FLOATING POINT VALUES, HENCE WHY 2 IS SUBTRACTED FROM THE TOTAL STRING LENGTH
	;; [ num ]	== ANY NUMERIC VALUE, IF NUM IS NOT NUMERIC, NUM IS RETURNED UNCHANGED
	
	
	(defun CreateDataMatrix ( ws / row col mxr key subk subv vals bgn end cell mtx )
		(setq row 1 col 1 mxr (msxl-get-count (msxl-get-rows (vlax-get-property ws 'UsedRange))))
		(while (< row mxr)
			(if (and (setq key (msxl:GetCellVal ws row col)) (wcmatch key "*:"))
				(progn
					(setq vals nil) ;; INITIALIZE VALS FOR EACH NEW KEY
					(while (and (setq subk (msxl:GetCellVal ws (setq row (1+ row)) col)) (not (wcmatch subk "*:")))
						(setq bgn (msxl:ReturnCellAddress (setq cell (msxl:ReturnCellObject ws row (1+ col)))))
						(setq end (msxl:ReturnCellAddress (msxl:LastNonEmptyCell cell)))
						(if (setq subv (msxl:RangeItems->List (msxl-get-range ws (if (and bgn end) (strcat bgn ":" end) bgn))))
							(setq vals (cons (cons subk (vl-string-right-trim " " (apply 'strcat (mapcar '(lambda ( s ) (if s (progn (setq s (ConvertNumberToString s)) (strcat s " ")) "")) subv)))) vals))
						)
					)
					(setq mtx (cons (cons key (reverse vals)) mtx)) ;; ADD THE KEY AND ITS COLLECTED SUB-ITEMS
				)
				(setq row (1+ row)) ;; INCREMENT ROW IF NO KEY FOUND IN THIS ROW
			)
		)
		(ObjectRelease (list ws cell))
		(reverse mtx) ;; REVERSE THE LIST TO GET THE ORIGINAL ORDER
	)
	;; RETURNS MATRIX OF KEY VALUE PAIRS REPRESENTING DATA FROM A PASSED EXCEL WORKSHEET
	;; [ ws ]	== EXCEL WORKSHEET OBJECT
	;; SEE LIBRARY FILE "LIB - MSXL Library Functions.lsp" FOR MSXL FUNCTIONS LISTED
	
	
	(defun FindText ( ptn / i txt obj )
		(if (setq ss (ssget "X" (list (cons 0 "*TEXT"))))
			(progn
				(setq i 0)
				(while (and (< i (sslength ss)) (not obj))
					(setq txt (vla-get-textstring (vlax-ename->vla-object (ssname ss i))))
					(if (vl-string-search (strcase ptn) (strcase txt))
						(setq obj (vlax-ename->vla-object (ssname ss i)))
					)
					(setq i (1+ i))
				)
			)
		)
		obj
	)
	;; RETURNS A TEXT OBJECT THAT CONTAINS THE PASSED PATTERN IF FOUND ; ELSE NIL
	;; [ ptn ]	== STRING VALUE REPRESENTING THE PATTERN TO SEARCH THE CURRENT AUTOCAD DRAWING DATABASE FOR
	
	
	(defun ReplaceTextInString ( str ptn rpl / rgx )
		(if (vl-every '(lambda ( var ) (= (type var) 'STR)) (list str ptn rpl))
			(progn
				(setq rgx (vlax-create-object "VBScript.RegExp"))
				(vlax-put-property rgx 'Pattern ptn)
				(vlax-put-property rgx 'IgnoreCase :vlax-true)
				(vlax-put-property rgx 'Global :vlax-true)
				(setq str (vlax-invoke rgx 'Replace str rpl))
				(ObjectRelease (list rgx))
				str
			)
			"N/A"
		)
	)
	;; RETURNS A STRING WITH THE PATTERN REPLACED, SIMILAR TO SUBSTR EXCEPT THIS REPLACES ALL FOUND PATTERNS IN A SINGLE STRING
	;; IF ANY PASSED VARIABLE IS NOT A STRING, FUNCTION RETURNS "N/A"
	;; [ str ]	== STRING VALUE REPRESENTING THE FULL STRING TO SEARCH THROUGH
	;; [ ptn ]	== STRING VALUE REPRESENTING THE PATTERN TO SEARCH FOR WITHIN THE STRING
	;; [ rpl ]	== THE STRING VALUE THAT WILL REPLACE THE PATTERN VALUE IF IT IS FOUND
	
	
	(defun GenerateKeyPattern ( key str )
		(strcat
			"%"
			(vl-string-trim " " key)
			(if (and str (> (strlen str) 0))
				(vl-string-right-trim " -?:" (vl-string-trim " " str))
				"N/A"
			)
			"%"
		)
	)
	;; CREATES A WILDCARD PATTERN STRING THAT CAN BE USED FOR SEARCHING AND REPLACING IN THE DOCUMENT
	;; [ key ]	== STRING VALUE THAT REPRESENTS THE MAIN CATEGORY TO SEARCH FOR
	;; [ str ]	== THE TAIL OF THE PATTERN
	;; EXAMPLE: KEY = "STRUCTURAL:", STR = "PROJECT DATE -" --> "%STRUCTURAL:PROJECT DATE%"
	;; THIS STRING VALUE CAN NOW BE SEARCHED FOR IN THE DOCUMENT AND REPLACED WITH A NEW VALUE
	
	;;; MAIN ;;;
	
	(setq dir (vl-filename-directory (getvar "DWGPREFIX")))
	(setq fl (getfiled "Select a File" (strcat "C:\\Users\\" (getvar "LOGINNAME") "\\") "xlsx;csv" 0))	;; FOR TESTING ONLY
	;(setq fl (getfiled "Select a File" (strcat dir "\\") "xlsx;csv" 0))								;; FOR LIVE FUNCTIONALITY USE
	
	(if fl
		(progn
			(setq xl (msxl:OpenExcel fl t nil)) ;; OPEN EXCEL SESSION AS BACKGROUND PROCESS
			(setq wb (msxl:ReturnActiveWorkBook xl))
			(setq wss (vlax-get-property wb 'WorkSheets))
			(if (and wss (= (type wss) 'VLA-OBJECT))
				(progn
					(vlax-for ws wss
						(if (not (= (msxl-get-name ws) "Data")) (setq nms (cons (msxl-get-name ws) nms))) ;; IGNORE "DATA" WORKSHEET
					)
					;(setq shts (MultiSelectListBox "Select BOD Type(s)" nms t))	;; FUNCTION DEFINED IN "LIB - STD Sub Functions.lsp"
					(setq shts (DynamicToggleBox "Select BOD Type(s)" nms))			;; FUNCTION DEFINED IN "LIB - STD Sub Functions.lsp"
				) ;; USE DYNAMIC TOGGLE BOX OR MULTI-SELECTION BOX FOR CHOOSING "BOD" TYPE
			)
			(foreach sht shts
				(setq mtx (CreateDataMatrix (msxl-get-item wss sht)))	;; TRAVERSES EXCEL SHEET AND STORES DATA AS KEY VALUE PAIRS
				(foreach itm mtx
					(vl-remove 'nil
						(mapcar 
							'(lambda ( s v ) (if (setq txt (FindText s)) (vla-put-textstring txt (ReplaceTextInString (vla-get-textstring txt) s (strcase v)))))
							(mapcar
								'(lambda ( v ) (GenerateKeyPattern (car itm) (car v))) (cdr itm)
							)
							(mapcar 'cdr (cdr itm))
						)
					)
				)
			)
		)
	)
	
	;;; CREATE PATTERNS FOR DRAWING TEMPLATE BASED ON KEY VALUES FROM EXCEL FILE
	
	(ObjectRelease (list wss ws wb))
	(msxl:CloseExcel xl)
)