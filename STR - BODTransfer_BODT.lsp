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
		ConvertValueToString SafeConcatenate CreateDataMatrix
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
		(msxl:CloseExcel xl)
		(gc)
		(princ (strcat "\nAn error occurred: " msg))
	)
	;; ERROR CATCHING FUNCTION
	
	
	
	(defun ConvertValueToString ( num )
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
	
	
	(defun SafeConcatenate ( stra strb )
		(cond
			((not strb)
				stra
			)
			((and stra strb)
				(strcat stra " " strb)
			)
			( t
				""
			)
		)
	)
	;; COMBINES TWO STRINGS TOGETHER IF NEITHER ARE NIL
	;; OTHERWISE WILL RETURN STRING "A" IF STRING "B" IS NIL OR "N/A"
	;; [ stra ]	== STRING "A" AS THE PREFIX STRING TO STRING "B"
	;; [ strb ]	== STRING "B" AS THE SUFFIX STRING TO STRING "A"
	
	 
	(defun CreateDataMatrix ( ws row col / mxr hdr vals key val cell mtx )
		(setq mxr (msxl-get-count (msxl-get-rows (vlax-get-property ws 'UsedRange))))
		(while (< row mxr)
			(if (and (msxl:IsMergedCell (setq cell (msxl:GetCell ws row col))) (setq hdr (msxl:GetCellValue cell)))
				(progn
					(setq vals nil) ;; INITIALIZE VALS FOR EACH NEW KEY
					(while (and (< row mxr) (not (msxl:IsMergedCell (msxl:GetCell ws (setq row (1+ row)) col))))
						(if (setq key (msxl:GetCellValue (msxl:GetCell ws row col)))
							(progn
								(setq val
									(SafeConcatenate
										(ConvertValueToString (msxl:GetCellValue (msxl:GetCell ws row (1+ col))))
										(msxl:GetCellValue (msxl:GetCell ws row (+ col 2)))
									) ;; SAFELY VERIFY THAT THE STRINGS CAN BE COMBINED
								)
								(setq vals (cons (cons key val) vals))
							)
						)
					)
					(setq mtx (cons (cons hdr (reverse vals)) mtx)) ;; ADD HEADER TO MATRIX LIST, REVERSE VALS AS TO KEEP THE CORRECT ORDER
				)
				(setq row (1+ row))
			)
		)
		(ObjectRelease (list ws cell))
		(reverse mtx) ;; REVERSE THE LIST TO GET THE ORIGINAL ORDER
	)
	;; RETURNS MATRIX OF KEY VALUE PAIRS REPRESENTING DATA FROM A PASSED EXCEL WORKSHEET
	;; [ ws ]	== EXCEL WORKSHEET OBJECT
	;; [ row ]	== STARTING ROW ON WORKSHEET OBJECT
	;; [ col ]	== STARTING COLUMN ON WORKSHEET OBJECT
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
	
	
	(defun GenerateKeyPattern ( key val )
		(strcat
			"%"
			(strcat (vl-string-trim " -?:=" key) ":")
			(if (and val (> (strlen val) 0))
				(vl-string-trim " -?:=" val)
				"N/A"
			)
			"%"
		)
	)
	;; CREATES A WILDCARD PATTERN STRING THAT CAN BE USED FOR SEARCHING AND REPLACING IN THE DOCUMENT
	;; [ key ]	== STRING VALUE THAT REPRESENTS THE MAIN CATEGORY TO SEARCH FOR
	;; [ val ]	== THE TAIL OF THE PATTERN
	;; EXAMPLE: KEY = "STRUCTURAL:", VAL = "PROJECT DATE -" --> "%STRUCTURAL:PROJECT DATE%"
	;; THIS STRING VALUE CAN NOW BE SEARCHED FOR IN THE DOCUMENT AND REPLACED WITH A NEW VALUE
	
	;;; MAIN ;;;
	
	(setq dir (vl-filename-directory (getvar "DWGPREFIX")))
	;; C:\Users\MichaelShaffer\OneDrive - ANS Geo\File Transfer\AutoLISP\BOD Excel Data Transfer\References\Notes & Updated BOD
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
						(if (wcmatch (strcase (msxl-get-name ws)) "* - STRUCT") (setq nms (cons (msxl-get-name ws) nms))) ;; REMOVE ALL INSTANCES OF SHEETS THAT DO NOT CONTAIN "- STRUCT"
						;; [BESS - STRUCT, PV - STRUCT, HV - STRUCT]
					)
					;(setq shts (MultiSelectListBox "Select BOD Type(s)" nms t))	;; FUNCTION DEFINED IN "LIB - STD Sub Functions.lsp"
					(setq shts (DynamicToggleBox "Select BOD Type(s)" nms))			;; FUNCTION DEFINED IN "LIB - STD Sub Functions.lsp"
				) ;; USE DYNAMIC TOGGLE BOX OR MULTI-SELECTION BOX FOR CHOOSING "BOD" TYPE
			)
			(foreach sht shts
				(setq mtx (CreateDataMatrix (msxl-get-item wss sht) 5 2)) ;; TRAVERSES EXCEL SHEET AND STORES DATA AS KEY VALUE PAIRS
				(foreach itm mtx
					(vl-remove 'nil
						(mapcar 
							'(lambda ( s v ) (if (setq txt (FindText s)) (vla-put-textstring txt (ReplaceTextInString (vla-get-textstring txt) s (strcase v)))))
							(mapcar
								'(lambda ( v ) (GenerateKeyPattern (strcat (vl-string-subst "" " - STRUCT" (strcase sht)) ":" (car itm)) (car v))) (cdr itm) ;;; BOOKMARK ;;; ??
							)
							(mapcar 'cdr (cdr itm))
						)
					)
				)
			)
		)
	)
	
	;;; CREATE PATTERNS FOR DRAWING TEMPLATE BASED ON KEY VALUES FROM EXCEL FILE
	
	(ObjectRelease (list cell wss ws wb))
	(msxl:CloseExcel xl)
	(gc)
)
	