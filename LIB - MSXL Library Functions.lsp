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

(defun msxl:ReturnWorkSheet ( wb wsn )
	(ReturnCollectionItem (vlax-get-property wb 'WorkSheets) wsn)
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





