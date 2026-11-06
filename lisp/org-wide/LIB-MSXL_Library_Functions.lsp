;; GLOBALLY AVAILABLE FUNCTIONS USED TO INTERACT WITH MICROSOFT EXCEL VIA AUTOLISP / AUTOCAD
;; SEE 'LIB - STD Sub-Functions.lsp' FOR GLOBALLY AVAILABLE FUNCTIONS NOT DEFINED IN THIS SCOPE
;; OjectRelease, ReturnCollectionItem, etc.

;; ALL CREATED EXCEL OBJECTS MUST BE RELEASED AFTER FUNCTION EXECUTION BUT PRIOR TO EXCELCLOSE BEING CALLED:
;; (ObjectRelease (list wb ws cell rng))
;; (msxl:CloseExcel xl)
;; THIS RE-ALLOCATES MEMORY TO THE COMPUTERS BACKGROUND PROCESSES

(defun msxl:OpenExcel ( fl rd vs / LoadExcelLibrary exs rt xl )
	
	(defun LoadExcelLibrary ( rt )
		(if (not $msxl-ExcelLib)
			(setq $msxl-ExcelLib
				(vlax-import-type-library
					:tlb-filename rt
					:methods-prefix "msxl-"
					:properties-prefix "msxl-"
					:constants-prefix "msxl-"
				)
			)
			;; !IMPORTANT! THIS VARIABLE MUST REMAIN GLOBAL
			;; FOR EVERY ACAD SESSION DO NOT LOCALIZE
		)
		$msxl-ExcelLib
	)
	;; LOADS THE EXCEL COMPONENT OBJECT MODEL (COM) TYPE LIBRARY FOR USE IN AUTOLISP
	;; THIS FUNCTION ENSURES THE LIBRARY IS IMPORTED ONLY ONCE PER AUTOCAD SESSION
	;; RETURNS T IF SUCCESSFUL, NIL OTHERWISE ; WILL ALSO RETURN T IF $msxl-ExcelLib WAS SET IN A PREVIOUS SESSION
	
	(setq exs (list ".csv" ".xls" ".xlsx" ".xlsm"))
	
	(if fl
		(cond
			((not (setq rt (vl-registry-read "HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\excel.exe")))
				(princ "\nMicrosoft Excel application was not found.")
				;; FIND EXCEL ROOT PATH [ rt ]
			)
			((not (findfile fl))
				(princ (strcat "\nProvided file '" fl "' was not found."))
			)
			((not (member (vl-filename-extension fl) exs))
				(princ (strcat "\nFile is not a supported file type: [" (vl-string-right-trim " " (apply 'strcat (mapcar '(lambda ( s ) (strcat s " ")) exs))) "]"))
			)
			((null (LoadExcelLibrary rt))
				(princ "\nLoading Excel Component Object Model (COM) type library was unsuccessful.")
			)
			( t
				(setq xl (vlax-create-object "Excel.Application"))
				;; CREATE NEW INSTANCE OF EXCEL SO AS TO NOT INTERUPT EXISTING INSTANCES OF EXCEL
				(msxl-open (vlax-get-property xl "Workbooks") fl nil (if rd :vlax-true :vlax-false))
				(msxl-put-visible xl (if vs :vlax-true :vlax-false))
				;; VISIBILITY CAN ONLY BE APPLIED TO THE APPLICATION
			)
		)
	)
	xl
)
;; OPENS A PASSED EXCEL FILE AND RETURNS THE EXCEL APPLICATION OBJECT [ xl ]
;; APPLICATION OBJECT MUST BE CLOSED WITH THE msxl:CloseExcel FUNCTION
;; [ fl ]	=	FULL FILE PATH STRING VALUE INCLUDING FILE NAME AND EXTENSION
;; [ rd ]	=	READONLY BOOLEAN VALUE - IF T, EXCEL FILE IS OPENED READ ONLY ; ELSE FILE IS OPENED WITH WRITE ACCESS
;; [ vs ]	=	VISIBILITY BOOLEAN VALUE - IF T, EXCEL SESSION IS VISIBLE TO THE SCREEN ; ELSE EXCEL IS OPENED AS A BACKGROUND PROCESS


(defun msxl:CloseExcel ( xl )
	(if (and (= (type xl) 'VLA-OBJECT) (not (vlax-object-released-p xl)))
		(progn
			(vlax-invoke-method xl "Quit")
			(vlax-release-object xl)
		)
	)
	(gc)
	(princ)
)
;; CLOSES AN OPEN EXCEL OBJECT AND RELEASES IT FROM SYSTEM MEMORY
;; THIS FUNCTION REQUIRES AN EXISTING EXCEL APPLICATION OBJECT
;; EITHER 'msxl:OpenExcel' OR 'msxl:GetOrCreateExcelApp' MUST BE CALLED FIRST
;; THIS FUNCTION USES GARBAGE COLLECTION TO REMOVE REFERENCES FROM BACKGROUND PROCESSES



;; COLUMNS ;;

(defun msxl:ColumnLetter ( num / str )
	(setq str "")
	(while (> num 0)
		(setq
			str (strcat (chr (+ 65 (rem (1- num) 26))) str)
			num (/ (1- num) 26)
		)
	)
	str
)
;; RETURNS THE EXCEL COLUMN LETTER FOR A 1-BASED COLUMN NUMBER (EX. 1 -> "A", 24 -> "X", 27 -> "AA")



;; IMPORT FUNCTIONS ;;

(defun msxl:ReadTables ( lst shn hdr / CellValue FindSheet ReadBlock xl wbs wb shs ws rng val rws hrw idx out res err nme r0 c0 nr nc sub cols len )
	(defun CellValue ( var / )
		(cond
			((= 'VARIANT (type var)) (vlax-variant-value var))
			( t var )
		)
	)
	;; RETURNS THE LISP VALUE OF A CELL (NUMBER, STRING, OR NIL FOR AN EMPTY CELL)
	
	(defun FindSheet ( wb / shs ws hit )
		(setq shs (vlax-get-property wb 'Worksheets))
		(if (and (= 'STR (type shn)) (not (vl-catch-all-error-p (setq ws (vl-catch-all-apply 'vlax-get-property (list shs 'Item shn))))))
			ws
			(progn
				(vlax-for sht shs
					(if (and (null hit) (wcmatch (strcase (vlax-get-property sht 'Name)) "*STR*,*STRUCT*"))
						(setq hit sht)
					)
				)
				hit
			)
		)
	)
	;; RETURNS THE WORKSHEET NAMED [ shn ], ELSE THE FIRST SHEET WITH "STR" OR "STRUCT" IN ITS NAME, ELSE NIL
	
	(defun ReadBlock ( r1 c1 r2 c2 / rg v )
		(setq
			rg (vlax-get-property ws 'Range (strcat (msxl:ColumnLetter c1) (itoa r1) ":" (msxl:ColumnLetter c2) (itoa r2)))
			v (vlax-variant-value (vlax-get-property rg 'Value2)) ;; ONE CALL FOR THE WHOLE BLOCK
		)
		(vlax-release-object rg)
		(if (= 'SAFEARRAY (type v))
			(mapcar '(lambda ( r ) (mapcar 'CellValue r)) (vlax-safearray->list v))
			(list (list (CellValue v))) ;; A ONE CELL RANGE RETURNS THE VALUE ITSELF
		)
	)
	;; RETURNS THE CELL VALUES OF ROWS [ r1 ] TO [ r2 ], COLUMNS [ c1 ] TO [ c2 ] (1 BASED, SHEET NUMBERS) OF 'ws' AS A LIST OF ROWS
	
	(if (or (vl-catch-all-error-p (setq xl (vl-catch-all-apply 'vlax-create-object (list "Excel.Application")))) (null xl))
		(setq out (mapcar '(lambda ( pth ) (list pth "Microsoft Excel was not found")) lst))
		(progn
			(vlax-put-property xl 'DisplayAlerts :vlax-false)
			(setq wbs (vlax-get-property xl 'Workbooks))
			(foreach pth lst
				(setq wb nil ws nil res nil)
				(setq err
					(vl-catch-all-apply
						'(lambda ( / hit )
							(setq wb (vlax-invoke-method wbs 'Open pth 0 :vlax-true)) ;; READ ONLY, LINKS NOT UPDATED. !REVISIT! CONFIRM LATE BINDING ACCEPTS THESE 3 ARGUMENTS (ADD / SAVEAS NEEDED EXTRA ONES)
							(cond
								((null (setq ws (FindSheet wb)))
									(setq res (strcat "no \"" (if (= 'STR (type shn)) shn "Structural") "\" sheet"))
								)
								( t
									(setq
										nme (vlax-get-property ws 'Name)
										rng (vlax-get-property ws 'UsedRange)
										r0 (vlax-get-property rng 'Row)
										c0 (vlax-get-property rng 'Column)
										sub (vlax-get-property rng 'Rows)
										nr (vlax-get-property sub 'Count)
									)
									(vlax-release-object sub)
									(setq
										sub (vlax-get-property rng 'Columns)
										nc (vlax-get-property sub 'Count)
									)
									(vlax-release-object sub)
									(vlax-release-object rng)
									(setq rws (ReadBlock r0 c0 (+ r0 (min 100 nr) -1) (+ c0 nc -1))) ;; ONLY THE FIRST 100 ROWS ARE READ WHOLE, TO FIND THE HEADER ROW
									(setq hrw 0)
									(while (and rws (< hrw 100) (null hit))
										(if (vl-some '(lambda ( h ) (member (strcase h) (mapcar '(lambda ( c ) (if (= 'STR (type c)) (strcase (vl-string-trim " " c)) "")) (car rws)))) hdr)
											(setq hit (car rws))
											(setq rws (cdr rws) hrw (1+ hrw))
										)
									) ;; HEADER ROW == FIRST ROW (OF THE FIRST 100) WITH ANY REQUESTED HEADER
									(if (null hit)
										(setq res "no header row with the expected column names")
										(progn
											(setq hit (mapcar '(lambda ( c ) (if (= 'STR (type c)) (strcase (vl-string-trim " " c)) "")) hit))
											(setq idx (mapcar '(lambda ( h ) (vl-position (strcase h) hit)) hdr)) ;; COLUMN OF EACH REQUESTED HEADER, NIL IF MISSING
											(setq cols
												(mapcar
													'(lambda ( i )
														(if (and i (< (1+ hrw) nr))
															(mapcar 'car (ReadBlock (+ r0 hrw 1) (+ c0 i) (+ r0 nr -1) (+ c0 i)))
														)
													)
													idx
												)
											) ;; ONLY THE REQUESTED COLUMNS ARE READ, ONE CALL EACH, FROM BELOW THE HEADER ROW TO THE LAST USED ROW
											(setq len (apply 'max (cons 0 (mapcar 'length cols))))
											(setq cols (mapcar '(lambda ( col ) (cond (col) ((repeat len (setq col (cons nil col))) col))) cols)) ;; A MISSING COLUMN READS AS EMPTY CELLS
											(setq res
												(list
													nme
													(mapcar '(lambda ( h i ) (if i h)) hdr idx)
													(if (< 0 len) (apply 'mapcar (cons 'list cols)))
												)
											)
										)
									)
								)
							)
						)
					)
				)
				(setq out (cons (list pth (if (vl-catch-all-error-p err) (vl-catch-all-error-message err) res)) out))
				(if (and ws (= 'VLA-OBJECT (type ws)) (not (vlax-object-released-p ws)))
					(vlax-release-object ws)
				)
				(if (and wb (= 'VLA-OBJECT (type wb)) (not (vlax-object-released-p wb)))
					(progn
						(vl-catch-all-apply 'vlax-invoke-method (list wb 'Close :vlax-false))
						(vlax-release-object wb)
					)
				)
			)
			(if (and wbs (not (vlax-object-released-p wbs)))
				(vlax-release-object wbs)
			)
			(vl-catch-all-apply 'vlax-invoke-method (list xl 'Quit))
			(vlax-release-object xl)
			(gc) ;; RELEASES EXCEL FROM MEMORY
			(setq out (reverse out))
		)
	)
	out
)
;; READS A TABLE FROM EACH WORKBOOK IN [ lst ] WITH ONE EXCEL INSTANCE, COLUMNS ARE FOUND BY HEADER NAME SO THEIR ORDER DOES NOT MATTER
;; RETURNS ONE ITEM PER FILE: ( PATH ( SHEET-NAME FOUND-HEADERS ROWS ) ) IF READ, ELSE ( PATH "REASON" )
;; FOUND-HEADERS == [ hdr ] WITH NIL FOR EACH HEADER THAT WAS NOT IN THE SHEET, ROWS == ONE LIST PER ROW IN [ hdr ] ORDER (NIL FOR A MISSING COLUMN OR EMPTY CELL)
;; HEADERS ARE COMPARED WITHOUT CASE OR SURROUNDING SPACES, THE HEADER ROW IS THE FIRST OF THE FIRST 100 ROWS WITH ANY REQUESTED HEADER
;; ONLY THE FIRST 100 ROWS AND THE REQUESTED COLUMNS ARE READ (ONE CALL EACH), SO WIDE SHEETS WITH MANY UNUSED COLUMNS READ QUICKLY
;; EXCEL IS ALWAYS CLOSED AND RELEASED, WORKBOOKS ARE OPENED READ ONLY AND NEVER SAVED
;; [ lst ]	== LIST OF FULL WORKBOOK PATHS
;; [ shn ]	== WORKSHEET NAME, OR NIL TO USE THE FIRST SHEET WITH "STR" / "STRUCT" IN ITS NAME
;; [ hdr ]	== LIST OF HEADER NAMES TO READ



;; EXPORT FUNCTIONS ;;

(defun msxl:ExportTable ( pth shn row hdr rows fmt / xl wbs wb ws sht rng fnt sa lst nrw ncl err out )
	(setq
		ncl (length hdr)
		nrw (length rows)
	)
	(if (or (vl-catch-all-error-p (setq xl (vl-catch-all-apply 'vlax-create-object (list "Excel.Application")))) (null xl))
		(setq
			xl nil
			err "Microsoft Excel was not found"
		) ;; A NEW EXCEL INSTANCE, WORKBOOKS THE USER HAS OPEN ARE NOT TOUCHED
		(setq err
			(vl-catch-all-apply
				'(lambda ( )
					(setq
						wbs (vlax-get-property xl 'Workbooks)
						wb (vlax-invoke-method wbs 'Add -4167) ;; -4167 == ONE BLANK WORKSHEET, LATE BINDING NEEDS THE ARGUMENT
						sht (vlax-get-property wb 'Worksheets)
						ws (vlax-get-property sht 'Item 1)
					)
					(vlax-put-property xl 'DisplayAlerts :vlax-false) ;; SAVE OVER AN EXISTING FILE WITHOUT ASKING
					(if (= 'STR (type shn))
						(vlax-put-property ws 'Name (substr shn 1 31)) ;; EXCEL SHEET NAMES ARE LIMITED TO 31 CHARACTERS
					)
					(foreach itm fmt
						(setq rng (vlax-get-property ws 'Range (car itm)))
						(vlax-put-property rng 'NumberFormat (cdr itm))
						(vlax-release-object rng)
					) ;; FORMATS ARE SET BEFORE THE VALUES, SO TEXT COLUMNS ("@") KEEP VALUES LIKE "1-1" AS TEXT, NOT DATES
					(setq
						lst (cons hdr rows)
						sa (vlax-make-safearray vlax-vbVariant (cons 0 nrw) (cons 0 (1- ncl)))
					)
					(vlax-safearray-fill sa lst)
					(setq rng (vlax-get-property ws 'Range (strcat "A" (itoa row) ":" (msxl:ColumnLetter ncl) (itoa (+ row nrw)))))
					(vlax-put-property rng 'Value2 (vlax-make-variant sa)) ;; ONE CALL FOR THE WHOLE TABLE, CELL BY CELL WOULD TAKE MINUTES
					(vlax-release-object rng)
					(setq rng (vlax-get-property ws 'Range (strcat "A" (itoa row) ":" (msxl:ColumnLetter ncl) (itoa row))))
					(setq fnt (vlax-get-property rng 'Font))
					(vlax-put-property fnt 'Bold :vlax-true)
					(vlax-release-object fnt)
					(vlax-invoke-method (vlax-get-property rng 'EntireColumn) 'AutoFit)
					(vlax-release-object rng)
					(vlax-invoke-method wb 'SaveAs pth 51 "" "" :vlax-false :vlax-false 1 2) ;; 51 == .xlsx, LATE BINDING NEEDS EVERY ARGUMENT: NO PASSWORDS, NO BACKUP, 1 == NO ACCESS CHANGE, 2 == KEEP LOCAL CHANGES
				)
			)
		)
	)
	(setq out
		(cond
			((= 'STR (type err)) err)
			((vl-catch-all-error-p err) (vl-catch-all-error-message err))
			( t t )
		)
	)
	(if (and wb (= 'VLA-OBJECT (type wb)) (not (vlax-object-released-p wb)))
		(vl-catch-all-apply 'vlax-invoke-method (list wb 'Close :vlax-false))
	)
	(foreach obj (list ws sht wb wbs)
		(if (and obj (= 'VLA-OBJECT (type obj)) (not (vlax-object-released-p obj)))
			(vlax-release-object obj)
		)
	)
	(if (and xl (= 'VLA-OBJECT (type xl)) (not (vlax-object-released-p xl)))
		(progn
			(vl-catch-all-apply 'vlax-invoke-method (list xl 'Quit))
			(vlax-release-object xl)
		)
	)
	(gc) ;; RELEASES EXCEL FROM MEMORY, OTHERWISE EXCEL.EXE CAN STAY OPEN IN THE BACKGROUND
	out
)
;; WRITES A TABLE TO A NEW EXCEL WORKBOOK AND SAVES IT AS .xlsx, EXCEL IS ALWAYS CLOSED AND RELEASED, EVEN WHEN THE EXPORT FAILS
;; RETURNS T IF SUCCESSFUL, ELSE THE ERROR MESSAGE (EX. THE FILE IS OPEN, EXCEL IS NOT INSTALLED)
;; DOES NOT NEED THE EXCEL TYPE LIBRARY (msxl:OpenExcel), IT USES LATE BINDING
;; [ pth ]	== FULL FILE PATH INCLUDING ".xlsx"
;; [ shn ]	== WORKSHEET NAME (CUT TO 31 CHARACTERS), NIL TO KEEP EXCEL'S DEFAULT
;; [ row ]	== ROW NUMBER FOR THE HEADER, DATA STARTS ON THE NEXT ROW (EX. 20 TO LEAVE ROOM FOR A SUMMARY ABOVE THE TABLE)
;; [ hdr ]	== LIST OF HEADER STRINGS, ONE PER COLUMN
;; [ rows ]	== LIST OF ROWS, EACH A LIST OF VALUES (STRING, INTEGER OR REAL) THE SAME LENGTH AS [ hdr ], USE "" FOR AN EMPTY CELL
;; [ fmt ]	== LIST OF ( "RANGE" . "NUMBER FORMAT" ) PAIRS APPLIED BEFORE WRITING, EX. (("A:A" . "@") ("G:H" . "0.00"))



;; WORKBOOK FUNCTIONS ;;

(defun msxl:ReturnActiveWorkBook ( xl )
	(if (and (eq (type xl) 'VLA-OBJECT) (not (vlax-object-released-p xl)))
		(vlax-get-property xl "ActiveWorkbook")
	)
)
;; RETURNS THE ACTIVE WORKBOOK OBJECT FROM AN EXCEL OBJECT
;; [ xl ]	== CURRENTLY ACTIVE EXCEL APPLICATION OBJECT


(defun msxl:ReturnWorkBookName ( wb )
	(if (and (eq (type wb) 'VLA-OBJECT) (not (vlax-object-released-p wb)))
		(vlax-get-property wb "Name")
	)
)
;; RETURNS THE ACTIVE WORKBOOKS NAME PASSED FROM THE MSXL:OPENEXCEL FUNCTION
;; THE MSXL:OPENEXCEL FUNCTION MUST BE CALLED FIRST WITH A VALID EXCEL OBJECT RETURNED
;; [ wb ]	== WORKBOOK OBJECT FROM EXCEL APPLICATION



;; WORKSHEET FUNCTIONS ;;

(defun msxl:ReturnWorkSheet ( wb wsn / ws )
	(if 
		(not
			(vl-catch-all-error-p 
				(setq ws (vl-catch-all-apply 'msxl-get-item (list (vlax-get-property wb 'WorkSheets) wsn)))
			)
		)
		ws
	)
)
;; RETURNS EXCEL WORKSHEET OBJECT BASED ON COLLENCTION ITEM IN WORKBOOKS
;; REQUIRES THE RETURNCOLLECTIONITEM FUNCTION TO OPPERATE
;; [ wb ]	== WORKBOOK OBJECT FROM EXCEL APPLICATION
;; [ wsn ]	== WORKSHEET NAME TO SEARCH COLLECTION FOR
;; WORKSHEET OBJECT IS RETURNED IF STRING NAME IS FOUND IN COLLECTION, ELSE NIL


(defun msxl:GetCell ( ws row col )
	(vlax-variant-value
		(msxl-get-item
			(msxl-get-cells ws)
			(vlax-make-variant row)
			(vlax-make-variant col)
		)
	)
)
;; [ ws ]	== EXCEL WORKSHEET OBJECT
;; [ row ]	== INTEGER VALUE REPRESENTING THE ROW
;; [ col ]	== INTEGER VALUE REPRESENTING THE COLUMN
;; RETURNS A CELL OBJECT BASED ON THE GIVEN ROW AND COLUMN ON THE WORKSHEET


(defun msxl:GetCellValue ( cell )
	(vlax-variant-value (msxl-get-value cell))
)
;; [ cell ]	== RANGE OBJECT REPRESENTING A SINGLE CELL
;; RETURNS TEXT VALUE OF WHATEVER IS FOUND IN THE CELL


(defun msxl:IsMergedCell ( cell )
	(= :vlax-true
		(vlax-variant-value
			(msxl-get-mergecells cell)
		)
	)
)
;; [ cell ]	== RANGE OBJECT REPRESENTING A SINGLE CELL
;; RETURNS T IF CELL IS MERGED WITH ADDITIONAL CELLS, ELSE NIL


(defun msxl:LastNonEmptyCell ( cell )
	(if (vlax-variant-value (msxl-get-value cell))
		(while (vlax-variant-value (msxl-get-value (msxl-get-next cell)))
			(setq cell (msxl-get-next cell))
		)
	)
)
;; [ cell ]	== RANGE OBJECT REPRESENTING A SINGLE CELL
;; RETURNS AN ADDRESS OF THE LAST NON-NIL CELL FOUND IN THE ROW
;; RETURNS NIL IF STARTING CELL IS EMPTY


(defun msxl:RangeItems->List ( rng / ret )
	(vlax-for cell rng
		(setq ret (cons (vlax-variant-value (msxl-get-value cell)) ret))
	)
	(reverse ret)
)
;; [ rng ]	== RANGE OBJECT OF MULTIPLE OR SINGLE CELL OBJECT
;; RETURNS A LIST OF VALUES WITHIN THE CELL OBJECTS IN THE PASSED RANGE


(defun msxl:ReturnColumnLetter ( addrs )
	(vl-list->string
		(vl-remove-if-not
			'(lambda ( x )
				(<= (ascii "A") x (ascii "Z"))
			)
			(vl-string->list addrs)
		)
	)
)
;; [ addrs ]	== CELL ADDRESS "$A1" "$CG$120"
;; RETURNS THE COLUMN LETTER ASSOCIATED WITH A GIVEN CELLS ADDRESS: "A" "CG"


(defun msxl:FindMatchingCell ( ws str )
	(msxl-find
		(msxl-get-cells ws)
		(vlax-make-variant str vlax-vbString)
		nil
		msxl-xlValues
		msxl-xlWhole
		msxl-xlByColumns
		msxl-xlNext
		:vlax-true
		:vlax-false
		:vlax-false
	)
)
;; https://learn.microsoft.com/en-gb/office/vba/api/excel.range.find
;; VBA RANGE METHOD DOCUMENTATION
;; [ ws ]	== EXCEL WORKSHEET OBJECT
;; [ str ]	== TARGET STRING TO LOOK FOR (MUST BE A STRING VALUE FOR THIS FUNCTION)
;; RETURNS A RANGE OBJECT OF SINGLE CELL THAT IS THE FIRST FOUND MATCH
;; THIS FUNCTION IS NOT DYNAMIC


(defun msxl:ReturnCellAddress ( cell )
	(if cell
		(msxl-get-address
			cell
			:vlax-true
			:vlax-true
			msxl-xlA1 ; xlR1C1 or xlA1
			:vlax-false
			:vlax-false
		)
	)
)
;; https://learn.microsoft.com/en-us/office/vba/api/excel.range.address
;; [cell]	== RANGE OBJECT REPRESENTING A SINGLE CELL
;; RETURNS AN ADDRESS OF A GIVEN CELL: "$A$1" "$CA$12"





