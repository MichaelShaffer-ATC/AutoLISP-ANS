;; THIS FILE CONTAINS JSON FUNCTIONS THAT ARE GLOBALLY AVAILABLE TO ALL OTHER LISP FUNCTIONS
;; FUNCTIONS DEFINED WITHIN THIS FILE READ, WRITE, AND ACCESS JSON DATA (EX. PPP PRESETS, PVCASE .PVsettings FILES)
;; THIS FILE IS INTENTIONALLY PURE ASCII: AUTOCAD'S LISP READER ABORTS SILENTLY ON NON-ASCII BYTES

;; JSON TO AUTOLISP DATA MAPPING:
;;	JSON OBJECT		{"a": 1, "b": "x"}		(OBJECT ("a" . 1) ("b" . "x"))
;;	JSON ARRAY		[1, 2, 3]				(ARRAY 1 2 3)
;;	JSON STRING		"text"					"text"
;;	JSON INTEGER	84						84		(VALUES BEYOND 32-BIT INTEGER RANGE ARE READ AS REALS)
;;	JSON REAL		6.804					6.804
;;	JSON TRUE		true					:vlax-true
;;	JSON FALSE		false					:vlax-false
;;	JSON NULL		null					nil
;;
;; OBJECTS AND ARRAYS ARE TAGGED WITH THE SYMBOLS 'OBJECT' AND 'ARRAY' SO AN EMPTY OBJECT, AN EMPTY ARRAY, AND NULL STAY DISTINCT
;; TRUE AND FALSE USE :vlax-true / :vlax-false SO FALSE IS NOT LOST AS NIL WHEN A FILE IS READ AND WRITTEN BACK
;; !IMPORTANT! :vlax-false IS NOT NIL, TEST BOOLEANS WITH (json:True-p val) RATHER THAN (if val ...)
;; OBJECT KEYS ARE CASE-SENSITIVE, MATCHING THE JSON SPECIFICATION
;;
;; WHEN WRITING, UNTAGGED LISTS ARE ALSO ACCEPTED FOR CONVENIENCE:
;;	A LIST WHERE EVERY ITEM IS A ( "STRING" . VALUE ) PAIR IS WRITTEN AS AN OBJECT, ANY OTHER LIST IS WRITTEN AS AN ARRAY
;;	T IS WRITTEN AS true, NIL AS null
;;	USE (json:Object nil) AND (json:Array nil) FOR EMPTY OBJECTS AND ARRAYS
;;
;; USE:
; (setq dat (json:ReadFile "C:\\Temp\\Frame presets.PVsettings"))
; (json:GetPath dat '("BlockTemplatesContainer" "Templates" 0 "Name"))					;; -> "INT-28M56"
; (json:Items (json:GetPath dat '("BlockTemplatesContainer" "Templates" 0 "PilingData" "FullTemplate" "HorizontalDistances")))
; (setq dat (json:SetPath dat '("Units" "CurrentUnit") "Foot"))
; (json:WriteFile "C:\\Temp\\Copy.PVsettings" dat "  ")

(if (not (boundp '$json-LastError)) (setq $json-LastError nil))
;; !IMPORTANT! THIS VARIABLE MUST REMAIN GLOBAL, HOLDS THE LAST PARSE OR WRITE ERROR MESSAGE (NIL IF THE LAST CALL SUCCEEDED)



;;; STRING UTILITIES ;;;

(defun json:StringJoin ( lst sep / out )
	(setq out "")
	(foreach str lst
		(setq out (if (= out "") str (strcat out sep str)))
	)
	out
)
;; JOINS A LIST OF STRINGS WITH A SEPARATOR STRING
;; RETURNS "" FOR AN EMPTY LIST
;; [ lst ]	== LIST OF STRINGS
;; [ sep ]	== SEPARATOR STRING (EX. ",")


(defun json:StringSplit ( str sep / pos out )
	(while (setq pos (vl-string-search sep str))
		(setq
			out (cons (substr str 1 pos) out)
			str (substr str (+ pos 1 (strlen sep)))
		)
	)
	(reverse (cons str out))
)
;; SPLITS A STRING INTO A LIST OF STRINGS AT EACH OCCURENCE OF THE SEPARATOR
;; [ str ]	== STRING TO SPLIT
;; [ sep ]	== SEPARATOR STRING (EX. "\n")



;;; PARSING (JSON TEXT -> AUTOLISP DATA) ;;;

(defun json:Parse ( str / Fail SkipWS HexValue MatchLiteral ParseValue ParseObject ParseArray ParseString ParseNumber err tot lst res )
	
	(defun Fail ( msg rst )
		(if (not err)
			(setq err (strcat msg " at character " (itoa (1+ (- tot (length rst))))))
		)
		nil
	)
	;; RECORDS THE FIRST ERROR AND ITS CHARACTER POSITION, LATER ERRORS ARE IGNORED SO THE ORIGINAL CAUSE IS REPORTED
	;; RETURNS NIL SO CALLERS CAN RETURN THE RESULT DIRECTLY
	
	(defun SkipWS ( rst )
		(while (member (car rst) '(9 10 13 32)) ;; TAB, LINE FEED, CARRIAGE RETURN, SPACE
			(setq rst (cdr rst))
		)
		rst
	)
	;; RETURNS THE CHARACTER CODE LIST WITH LEADING WHITESPACE REMOVED
	
	(defun HexValue ( cds / val ok )
		(setq val 0 ok t)
		(foreach cde cds
			(cond
				((null cde) (setq ok nil))
				((<= 48 cde 57) (setq val (+ (* val 16) (- cde 48))))	;; 0-9
				((<= 65 cde 70) (setq val (+ (* val 16) (- cde 55))))	;; A-F
				((<= 97 cde 102) (setq val (+ (* val 16) (- cde 87))))	;; a-f
				( t (setq ok nil))
			)
		)
		(if ok val)
	)
	;; CONVERTS A LIST OF HEXADECIMAL CHARACTER CODES TO AN INTEGER
	;; RETURNS NIL IF ANY CODE IS MISSING OR NOT A HEXADECIMAL DIGIT
	
	(defun MatchLiteral ( rst lit val / cds )
		(setq cds (vl-string->list lit))
		(while (and cds (eq (car cds) (car rst)))
			(setq cds (cdr cds) rst (cdr rst))
		)
		(if cds
			(Fail (strcat "Invalid literal, expected '" lit "'") rst)
			(cons val rst)
		)
	)
	;; MATCHES A FIXED LITERAL (true, false, null)
	;; RETURNS ( VALUE . REMAINING-CODES ) IF MATCHED, ELSE NIL
	
	(defun ParseValue ( rst / cde )
		(setq rst (SkipWS rst) cde (car rst))
		(cond
			((null cde)						(Fail "Unexpected end of input" rst))
			((= cde 123)					(ParseObject (cdr rst)))					;; {
			((= cde 91)						(ParseArray (cdr rst)))						;; [
			((= cde 34)						(ParseString (cdr rst)))					;; "
			((= cde 116)					(MatchLiteral rst "true" :vlax-true))
			((= cde 102)					(MatchLiteral rst "false" :vlax-false))
			((= cde 110)					(MatchLiteral rst "null" nil))
			((or (= cde 45) (<= 48 cde 57))	(ParseNumber rst))							;; - OR 0-9
			( t								(Fail (strcat "Unexpected character '" (chr cde) "'") rst))
		)
	)
	;; PARSES ANY JSON VALUE FROM THE FRONT OF THE CHARACTER CODE LIST
	;; RETURNS ( VALUE . REMAINING-CODES ) IF SUCCESSFUL, ELSE NIL
	;; THE RESULT IS ALWAYS A CONS CELL ON SUCCESS, SO A NULL VALUE (NIL) IS STILL DISTINGUISHABLE FROM FAILURE
	
	(defun ParseObject ( rst / itm key prs done )
		(setq rst (SkipWS rst))
		(if (= (car rst) 125) ;; }
			(cons (list 'OBJECT) (cdr rst))
			(progn
				(while (and (not err) (not done))
					(setq rst (SkipWS rst))
					(cond
						((/= (car rst) 34)
							(Fail "Expected string key" rst)
						)
						((null (setq itm (ParseString (cdr rst)))))
						( t
							(setq key (car itm) rst (SkipWS (cdr itm)))
							(cond
								((/= (car rst) 58) ;; :
									(Fail "Expected ':'" rst)
								)
								((null (setq itm (ParseValue (cdr rst)))))
								( t
									(setq
										prs (cons (cons key (car itm)) prs)
										rst (SkipWS (cdr itm))
									)
									(cond
										((= (car rst) 44) (setq rst (cdr rst)))				;; , NEXT PAIR
										((= (car rst) 125) (setq rst (cdr rst) done t))		;; } END OF OBJECT
										( t (Fail "Expected ',' or '}'" rst))
									)
								)
							)
						)
					)
				)
				(if (not err)
					(cons (cons 'OBJECT (reverse prs)) rst)
				)
			)
		)
	)
	;; PARSES A JSON OBJECT, THE OPENING BRACE HAS ALREADY BEEN CONSUMED
	;; RETURNS ( (OBJECT ("KEY" . VALUE) ...) . REMAINING-CODES ) IF SUCCESSFUL, ELSE NIL
	
	(defun ParseArray ( rst / itm lst done )
		(setq rst (SkipWS rst))
		(if (= (car rst) 93) ;; ]
			(cons (list 'ARRAY) (cdr rst))
			(progn
				(while (and (not err) (not done))
					(if (setq itm (ParseValue rst))
						(progn
							(setq
								lst (cons (car itm) lst)
								rst (SkipWS (cdr itm))
							)
							(cond
								((= (car rst) 44) (setq rst (cdr rst)))				;; , NEXT ITEM
								((= (car rst) 93) (setq rst (cdr rst) done t))		;; ] END OF ARRAY
								( t (Fail "Expected ',' or ']'" rst))
							)
						)
					)
				)
				(if (not err)
					(cons (cons 'ARRAY (reverse lst)) rst)
				)
			)
		)
	)
	;; PARSES A JSON ARRAY, THE OPENING BRACKET HAS ALREADY BEEN CONSUMED
	;; RETURNS ( (ARRAY VALUE ...) . REMAINING-CODES ) IF SUCCESSFUL, ELSE NIL
	
	(defun ParseString ( rst / out cde hex )
		(while (and rst (/= (setq cde (car rst)) 34) (not err))
			(cond
				((= cde 92) ;; BACKSLASH ESCAPE
					(setq rst (cdr rst) cde (car rst))
					(cond
						((= cde 34)		(setq out (cons 34 out)))	;; \"
						((= cde 92)		(setq out (cons 92 out)))	;; \\
						((= cde 47)		(setq out (cons 47 out)))	;; \/
						((= cde 98)		(setq out (cons 8 out)))	;; \b
						((= cde 102)	(setq out (cons 12 out)))	;; \f
						((= cde 110)	(setq out (cons 10 out)))	;; \n
						((= cde 114)	(setq out (cons 13 out)))	;; \r
						((= cde 116)	(setq out (cons 9 out)))	;; \t
						((= cde 117)								;; \uXXXX
							(if (setq hex (HexValue (list (cadr rst) (caddr rst) (cadddr rst) (car (cddddr rst)))))
								(setq
									out (cons (if (< 0 hex 256) hex 63) out) ;; CHARACTERS OUTSIDE THE SINGLE BYTE RANGE BECOME "?"
									rst (cddddr rst) ;; POINT AT THE LAST HEX DIGIT, THE LOOP STEP BELOW MOVES PAST IT
								)
								(Fail "Invalid unicode escape" rst)
							)
						)
						( t (Fail "Invalid escape character" rst))
					)
				)
				((< cde 32)
					(Fail "Unescaped control character in string" rst)
				)
				( t
					(setq out (cons cde out))
				)
			)
			(setq rst (cdr rst))
		)
		(cond
			(err nil)
			((null rst) (Fail "Unterminated string" rst))
			( t (cons (vl-list->string (reverse out)) (cdr rst)))
		)
	)
	;; PARSES A JSON STRING, THE OPENING QUOTE HAS ALREADY BEEN CONSUMED
	;; RETURNS ( "STRING" . REMAINING-CODES ) IF SUCCESSFUL, ELSE NIL
	
	(defun ParseNumber ( rst / out str )
		(while (and rst (or (<= 48 (car rst) 57) (member (car rst) '(43 45 46 69 101)))) ;; 0-9 + - . E e
			(setq out (cons (car rst) out) rst (cdr rst))
		)
		(setq str (vl-list->string (reverse out)))
		(cond
			((not (vl-some '(lambda ( cde ) (<= 48 cde 57)) out))
				(Fail "Invalid number" rst)
			)
			((vl-some '(lambda ( cde ) (member cde '(46 69 101))) out) ;; DECIMAL POINT OR EXPONENT
				(cons (atof str) rst)
			)
			((< (abs (atof str)) 2147483647.0)
				(cons (atoi str) rst)
			)
			( t
				(cons (atof str) rst) ;; TOO LARGE FOR A 32-BIT INTEGER
			)
		)
	)
	;; PARSES A JSON NUMBER
	;; RETURNS ( NUMBER . REMAINING-CODES ) IF SUCCESSFUL, ELSE NIL
	
	(setq err nil)
	(cond
		((= (type str) 'STR)
			(setq lst (vl-string->list str))
		)
		((and (listp str) (vl-every '(lambda ( cde ) (= (type cde) 'INT)) str))
			(setq lst str) ;; CHARACTER CODE LIST FROM json:ReadFile
		)
		( t
			(setq err "Input is not a string")
		)
	)
	(if (not err)
		(progn
			(if (and (= (car lst) 239) (= (cadr lst) 187) (= (caddr lst) 191))
				(setq lst (cdddr lst))
			) ;; SKIP UTF-8 BYTE ORDER MARK
			(setq tot (length lst))
			(if (and (setq res (ParseValue lst)) (SkipWS (cdr res)))
				(Fail "Unexpected data after JSON value" (SkipWS (cdr res)))
			)
		)
	)
	(setq $json-LastError err)
	(if err
		(progn
			(princ (strcat "\nJSON Error: " err))
			nil
		)
		(car res)
	)
)
;; PARSES A JSON STRING INTO AUTOLISP DATA (SEE MAPPING AT THE TOP OF THIS FILE)
;; RETURNS THE PARSED VALUE, OR NIL AND SETS $json-LastError IF THE TEXT IS NOT VALID JSON
;; A VALID DOCUMENT OF 'null' ALSO RETURNS NIL, CHECK (json:LastError) TO TELL THE TWO APART
;; [ str ]	== JSON STRING (A LIST OF CHARACTER CODES IS ALSO ACCEPTED, USED BY json:ReadFile)


(defun json:ReadFile ( pth / des lin cds )
	(setq $json-LastError nil)
	(cond
		((or (/= (type pth) 'STR) (null (setq pth (findfile pth))))
			(setq $json-LastError "File not found")
			(princ "\nJSON Error: File not found.")
			nil
		)
		((null (setq des (open pth "r")))
			(setq $json-LastError "File could not be opened")
			(princ (strcat "\nJSON Error: File could not be opened: " pth))
			nil
		)
		( t
			(while (setq lin (read-line des))
				(foreach cde (vl-string->list lin)
					(setq cds (cons cde cds))
				)
				(setq cds (cons 10 cds)) ;; KEEP LINE BREAKS SO ERROR POSITIONS MATCH THE FILE
			)
			(close des)
			(json:Parse (reverse cds))
		)
	)
)
;; READS AND PARSES A JSON FILE
;; CHARACTERS ARE COLLECTED AS A CODE LIST RATHER THAN BY JOINING STRINGS, WHICH KEEPS LARGE FILES FAST
;; RETURNS THE PARSED VALUE, OR NIL AND SETS $json-LastError IF THE FILE IS MISSING OR NOT VALID JSON
;; [ pth ]	== FILE PATH STRING



;;; WRITING (AUTOLISP DATA -> JSON TEXT) ;;;

(defun json:Write ( val ind / Hex4 Escape FormatNumber IsPairList WriteValue WriteObject WriteArray nl err dz out )
	
	(defun Hex4 ( n / hx )
		(setq hx "0123456789ABCDEF")
		(strcat
			(substr hx (1+ (logand (lsh n -12) 15)) 1)
			(substr hx (1+ (logand (lsh n -8) 15)) 1)
			(substr hx (1+ (logand (lsh n -4) 15)) 1)
			(substr hx (1+ (logand n 15)) 1)
		)
	)
	;; RETURNS A 4 DIGIT HEXADECIMAL STRING FOR AN INTEGER (EX. 176 -> "00B0")
	
	(defun Escape ( str / )
		(vl-list->string
			(apply 'append
				(mapcar
					'(lambda ( cde )
						(cond
							((= cde 34)		'(92 34))	;; \"
							((= cde 92)		'(92 92))	;; \\
							((= cde 8)		'(92 98))	;; \b
							((= cde 12)		'(92 102))	;; \f
							((= cde 10)		'(92 110))	;; \n
							((= cde 13)		'(92 114))	;; \r
							((= cde 9)		'(92 116))	;; \t
							((or (< cde 32) (> cde 126))
								(append '(92 117) (vl-string->list (Hex4 cde))) ;; \uXXXX, KEEPS THE OUTPUT PURE ASCII
							)
							( t (list cde))
						)
					)
					(vl-string->list str)
				)
			)
		)
	)
	;; RETURNS THE STRING WITH JSON ESCAPES APPLIED (WITHOUT SURROUNDING QUOTES)
	
	(defun FormatNumber ( n / str )
		(setq str (rtos n 2 8)) ;; DIMZIN IS SET TO 0 FOR THE DURATION OF json:Write SO LEADING ZEROS ARE KEPT (".5" IS NOT VALID JSON)
		(if (vl-string-search "." str)
			(setq str (vl-string-right-trim "0" str))
		)
		(if (= "." (substr str (strlen str)))
			(setq str (strcat str "0"))
		) ;; KEEP ONE DECIMAL PLACE SO WHOLE REALS (EX. 60.0) ARE READ BACK AS REALS, NOT INTEGERS
		(if (= str "-0.0") "0.0" str)
	)
	;; RETURNS A REAL AS A JSON NUMBER STRING WITH UP TO 8 DECIMAL PLACES AND NO TRAILING ZEROS BEYOND THE FIRST (EX. 60.0, 6.804)
	;; USES RTOS RATHER THAN VL-PRINC-TO-STRING, WHICH ROUNDS LARGE COORDINATES TO 6 SIGNIFICANT DIGITS (EX. 2.64068e+06)
	
	(defun IsPairList ( lst / )
		(and
			lst
			(vl-list-length lst)
			(vl-every '(lambda ( itm ) (and (listp itm) itm (= (type (car itm)) 'STR))) lst)
		)
	)
	;; RETURNS T IF EVERY ITEM IN THE LIST IS A ( "STRING" . VALUE ) PAIR, USED TO DETECT UNTAGGED OBJECTS
	
	(defun WriteValue ( val pad / )
		(cond
			((null val)										"null")
			((or (eq val t) (eq val :vlax-true))			"true")
			((eq val :vlax-false)							"false")
			((= (type val) 'INT)							(itoa val))
			((= (type val) 'REAL)							(FormatNumber val))
			((= (type val) 'STR)							(strcat "\"" (Escape val) "\""))
			((not (listp val))
				(setq err (strcat "Unsupported data type " (vl-princ-to-string (type val)) ": " (vl-princ-to-string val)))
				"null"
			) ;; SYMBOLS, ENAMES, VLA-OBJECTS, FILES, ETC.
			((null (vl-list-length val))
				(setq err "Dotted pair found where a value was expected")
				"null"
			)
			((eq (car val) 'OBJECT)							(WriteObject (cdr val) pad))
			((eq (car val) 'ARRAY)							(WriteArray (cdr val) pad))
			((IsPairList val)								(WriteObject val pad))
			( t												(WriteArray val pad))
		)
	)
	;; RETURNS THE JSON TEXT FOR ANY SUPPORTED VALUE
	;; [ pad ]	== CURRENT INDENT STRING FOR THE LINE THE VALUE STARTS ON
	
	(defun WriteObject ( prs pad / nxt )
		(setq nxt (strcat pad ind))
		(if (null prs)
			"{}"
			(strcat
				"{" nl
				(json:StringJoin
					(mapcar
						'(lambda ( pair )
							(if (and (listp pair) pair)
								(strcat
									nxt "\""
									(Escape (if (= (type (car pair)) 'STR) (car pair) (vl-princ-to-string (car pair))))
									"\":" (if (= ind "") "" " ")
									(WriteValue (cdr pair) nxt)
								)
								(progn
									(setq err "Object item is not a ( KEY . VALUE ) pair")
									(strcat nxt "\"\":null")
								)
							)
						)
						prs
					)
					(strcat "," nl)
				)
				nl pad "}"
			)
		)
	)
	;; RETURNS THE JSON TEXT FOR A LIST OF ( "KEY" . VALUE ) PAIRS
	
	(defun WriteArray ( lst pad / nxt )
		(setq nxt (strcat pad ind))
		(if (null lst)
			"[]"
			(strcat
				"[" nl
				(json:StringJoin
					(mapcar '(lambda ( itm ) (strcat nxt (WriteValue itm nxt))) lst)
					(strcat "," nl)
				)
				nl pad "]"
			)
		)
	)
	;; RETURNS THE JSON TEXT FOR A LIST OF VALUES
	
	(setq
		ind (if (= (type ind) 'STR) ind "")
		nl (if (= ind "") "" "\n")
		err nil
		dz (getvar "DIMZIN")
	)
	(setvar "DIMZIN" 0)
	(setq out (vl-catch-all-apply 'WriteValue (list val "")))
	(setvar "DIMZIN" dz) ;; ALWAYS RESTORED, EVEN IF WRITING FAILED
	(if (vl-catch-all-error-p out)
		(setq err (vl-catch-all-error-message out))
	)
	(setq $json-LastError err)
	(if err
		(progn
			(princ (strcat "\nJSON Error: " err))
			nil
		)
		out
	)
)
;; CONVERTS AUTOLISP DATA INTO A JSON STRING (SEE MAPPING AT THE TOP OF THIS FILE)
;; RETURNS THE JSON STRING, OR NIL AND SETS $json-LastError IF THE DATA CONTAINS AN UNSUPPORTED VALUE
;; REALS ARE WRITTEN WITH UP TO 8 DECIMAL PLACES
;; [ val ]	== VALUE TO CONVERT
;; [ ind ]	== INDENT STRING FOR PRETTY PRINTING (EX. "  " OR "\t"), NIL FOR COMPACT SINGLE LINE OUTPUT


(defun json:WriteFile ( pth val ind / str des )
	(cond
		((null (setq str (json:Write val ind)))
			nil
		)
		((null (setq des (open pth "w")))
			(setq $json-LastError "File could not be opened for writing")
			(princ (strcat "\nJSON Error: File could not be opened for writing: " pth))
			nil
		)
		( t
			(foreach lin (json:StringSplit str "\n")
				(write-line lin des) ;; WRITE-LINE HANDLES THE PLATFORM LINE ENDING
			)
			(close des)
			t
		)
	)
)
;; WRITES AUTOLISP DATA TO A JSON FILE
;; RETURNS T IF SUCCESSFUL, ELSE NIL (SEE $json-LastError)
;; [ pth ]	== FILE PATH STRING, EXISTING FILES ARE OVERWRITTEN
;; [ val ]	== VALUE TO WRITE
;; [ ind ]	== INDENT STRING FOR PRETTY PRINTING (EX. "  "), NIL FOR COMPACT OUTPUT



;;; CONSTRUCTORS AND ACCESSORS ;;;

(defun json:Object ( prs )
	(cons 'OBJECT prs)
)
;; RETURNS A TAGGED JSON OBJECT FROM A LIST OF ( "KEY" . VALUE ) PAIRS
;; [ prs ]	== ASSOCIATION LIST, NIL FOR AN EMPTY OBJECT


(defun json:Array ( lst )
	(cons 'ARRAY lst)
)
;; RETURNS A TAGGED JSON ARRAY FROM A LIST OF VALUES
;; [ lst ]	== LIST OF VALUES, NIL FOR AN EMPTY ARRAY


(defun json:Pairs ( obj )
	(if (and (listp obj) (eq (car obj) 'OBJECT))
		(cdr obj)
		obj
	)
)
;; RETURNS THE ( "KEY" . VALUE ) PAIRS OF AN OBJECT WITHOUT THE 'OBJECT' TAG
;; [ obj ]	== TAGGED OR UNTAGGED OBJECT


(defun json:Items ( arr )
	(if (and (listp arr) (eq (car arr) 'ARRAY))
		(cdr arr)
		arr
	)
)
;; RETURNS THE VALUES OF AN ARRAY WITHOUT THE 'ARRAY' TAG
;; [ arr ]	== TAGGED OR UNTAGGED ARRAY


(defun json:Keys ( obj )
	(mapcar 'car (json:Pairs obj))
)
;; RETURNS A LIST OF THE KEY STRINGS IN AN OBJECT
;; [ obj ]	== TAGGED OR UNTAGGED OBJECT


(defun json:Has ( obj key )
	(and (listp obj) (assoc key (json:Pairs obj)) t)
)
;; RETURNS T IF THE OBJECT CONTAINS THE KEY, EVEN IF ITS VALUE IS NULL, ELSE NIL
;; [ obj ]	== TAGGED OR UNTAGGED OBJECT
;; [ key ]	== KEY STRING (CASE-SENSITIVE)


(defun json:Get ( obj key )
	(if (listp obj)
		(cdr (assoc key (json:Pairs obj)))
	)
)
;; RETURNS THE VALUE FOR A KEY IN AN OBJECT, NIL IF THE KEY IS MISSING OR NULL (SEE json:Has)
;; [ obj ]	== TAGGED OR UNTAGGED OBJECT
;; [ key ]	== KEY STRING (CASE-SENSITIVE)


(defun json:GetPath ( val pth )
	(foreach key pth
		(setq val
			(cond
				((and (= (type key) 'INT) (>= key 0))	(nth key (json:Items val)))
				((= (type key) 'STR)					(json:Get val key))
			)
		)
	)
	val
)
;; RETURNS A NESTED VALUE BY FOLLOWING A PATH OF OBJECT KEYS AND ARRAY INDEXES
;; RETURNS NIL IF ANY STEP OF THE PATH DOES NOT EXIST
;; [ val ]	== ROOT VALUE
;; [ pth ]	== LIST OF KEY STRINGS AND ZERO-BASED INTEGER INDEXES (EX. '("Templates" 0 "Name"))


(defun json:Set ( obj key val / tag prs )
	(setq
		tag (or (null obj) (eq (car obj) 'OBJECT))
		prs (json:Pairs obj)
	)
	(setq prs
		(if (assoc key prs)
			(mapcar '(lambda ( pair ) (if (and (listp pair) (equal (car pair) key)) (cons key val) pair)) prs)
			(append prs (list (cons key val)))
		)
	)
	(if tag (cons 'OBJECT prs) prs)
)
;; RETURNS A COPY OF THE OBJECT WITH THE KEY SET TO THE VALUE, EXISTING KEYS KEEP THEIR POSITION, NEW KEYS ARE ADDED AT THE END
;; THE PASSED OBJECT IS NOT MODIFIED, SET THE RESULT BACK (EX. (setq obj (json:Set obj "Name" "INT-28M56")))
;; [ obj ]	== TAGGED OR UNTAGGED OBJECT, NIL CREATES A NEW OBJECT
;; [ key ]	== KEY STRING (CASE-SENSITIVE)
;; [ val ]	== NEW VALUE


(defun json:SetPath ( val pth new / key idx )
	(if (null pth)
		new
		(progn
			(setq key (car pth))
			(if (= (type key) 'INT)
				(progn
					(setq idx -1)
					(json:Array
						(mapcar
							'(lambda ( itm )
								(if (= (setq idx (1+ idx)) key)
									(json:SetPath itm (cdr pth) new)
									itm
								)
							)
							(json:Items val)
						)
					)
				)
				(json:Set val key (json:SetPath (json:Get val key) (cdr pth) new))
			)
		)
	)
)
;; RETURNS A COPY OF THE ROOT VALUE WITH THE NESTED VALUE AT THE PATH REPLACED
;; MISSING OBJECT KEYS ALONG THE PATH ARE CREATED, ARRAY INDEXES OUT OF RANGE LEAVE THE ARRAY UNCHANGED
;; [ val ]	== ROOT VALUE
;; [ pth ]	== LIST OF KEY STRINGS AND ZERO-BASED INTEGER INDEXES
;; [ new ]	== NEW VALUE


(defun json:True-p ( val )
	(or (eq val :vlax-true) (eq val t))
)
;; RETURNS T IF THE VALUE IS JSON TRUE, ELSE NIL
;; USE THIS INSTEAD OF (if val ...) SINCE :vlax-false IS NOT NIL


(defun json:LastError ( )
	$json-LastError
)
;; RETURNS THE LAST PARSE OR WRITE ERROR MESSAGE, NIL IF THE LAST CALL SUCCEEDED


