;; FUNCTION TO HELP STANDARDIZE THE CONTACT INFORMATION ON THE COVER SHEET BASED ON INFORMATION STORED IN THE BOD FOR A PROJECT
;; USES OUTSIDE LIBRARY LISP FILES:
;; - "LIB - STD Sub-Functions.lsp"
;; - "LIB - MSXL Library Functions.lsp"


;; ASK USER TO SELECT BOD
;; PARSE DATA FROM A SPECIFIC EXCEL TAB
;; EDIT / CREATE MTEXT OBJECTS FOR THE CONTACT DATA

(defun c:UCONI ( / )
	(c:UPDATECONTACTINFO)
)


(defun c:UPDATECONTACTINFO
	(
		/
		;Functions
		*error*
		BODContactsParser ClearExistingTextObjects StringBuilder CreateContactTextObjects
		;Variables
		dir fl cts dat osm
		;Excel Based
		xl wb wss ws
	)
	
	(defun *error* ( msg )
		(if (not (member msg (list "Function cancelled" "quit / exit abort")))
			(vl-bt)
		)
		(if osm (setvar "OSMODE" osm))
		;; !!MAKE SURE TO RELEASE ALL EXCEL RELATED OBJECTS!!
		(std:ObjectRelease (list cel ws wb))
		(msxl:CloseExcel xl)
		(gc)
		(princ (strcat "\nAn error occurred: " msg))
	)
	;; ERROR CATCHING FUNCTION
	
	(defun BODContactsParser ( xl cts / wb ws cel row col key val sbl res )
		(princ "\nGathering Excel information... This may take a minute.")
		(if (and (setq wb (msxl:ReturnActiveWorkBook xl)) (setq ws (msxl:ReturnWorkSheet wb "Contacts")))
			(foreach ct cts
				;; FIND HEADERS THAT MATCH THE 'CTS' VALUES
				(if (setq cel (msxl:FindMatchingCell ws ct)) ;; RETURNS THE HEADER CELL, IF IT EXISTS
					(progn
						(setq sbl nil)
						(setq cel (msxl:GetCell ws (setq row (1+ (msxl-get-row cel))) (setq col (msxl-get-column cel))))
						(while (and (not (msxl:IsMergedCell cel)) (not (null (setq key (msxl:GetCellValue cel)))))
							(if (not (null (setq val (msxl:GetCellValue (msxl:GetCell ws row (1+ col))))))
								(setq sbl (cons (cons key (std:ConvertToString val)) sbl))
							)
							(setq cel (msxl:GetCell ws (setq row (1+ row)) col)) ;; CONTINUE TO NEXT CELL
						)
						(setq res (cons (cons ct (reverse sbl)) res))
					)
				)
			)
			(alert "Workbook was not returned or 'Contacts' worksheet does not exist in workbook.")
		)
		(std:ObjectRelease (list cel ws wb))
		(reverse res)
	)
	;; RETURNS KEY VALUE PAIRS OF CONTACT INFORMATION FOUND IN BOD BASED ON USER SPECIFIED INFORMATION 'cts'
	;; MAY NEED TO MAP SELECTED CONTACTS FROM LISP CODE TO CONTACT NAMES IN BOD EXCEL FILE
	;; MAY UPDATE TO REMOVE ANY "" VALUES FROM SUBLIST 'sbl'
	
	(defun ClearExistingTextObjects ( ptl / ss i )
		(if (setq ss (ssget "_W" (car ptl) (cadr ptl) '((0 . "MTEXT"))))
			(progn
				(setq i 0)
				(repeat (sslength ss)
					(entdel (ssname ss i))
					(setq i (1+ i))
				)
			)
		)
	)
	;; DELETES ALL TEXT OBJECTS IN A SPECIFIED WINDOW
	
	(defun StringBuilder ( inf / PrependingHelper hdr cmp adr cit sta zip nme eml )
		(defun PrependingHelper ( dat key str )
			(if (assoc key dat)
				(subst (cons key (strcat str (cdr (assoc key dat)))) (assoc key dat) dat)
				dat
			)
		)
		;; ADJUST DATA TO INCLUDE PREPENDED TEXT FOR CERTAIN ITEMS (CONTACT AND EMAIL)
		
		;; SET HEADER (UNDERLINED)
		;; TEXT FORMATTING SEQUENCE:
		;; \\pxsm1,ql;{H1.06667X;\\LPROJECT OWNER\\H0.50001x;\\l\\P\\psm1.5;COMPANY NAME\\PADDRESS LINE 1\\PCITY, STATE 00000\\PCONTACT NAME\\PEMAIL}
		(setq hdr (strcase (car inf)) inf (cdr inf))
		(setq inf (PrependingHelper inf "Contact Name" "CONTACT: "))
		(setq inf (PrependingHelper inf "Contact Email" "EMAIL: "))
		
		(setq 
			cmp (if (setq cmp (assoc "Company Name" inf)) (strcase (strcat (cdr cmp))) "")
			adr (if
					(and
						(setq adr (assoc "Street Address" inf))
						(setq cit (assoc "City" inf))
						(setq sta (assoc "State" inf))
						(setq zip (assoc "Zip Code" inf))
					)
					(setq adr (strcase (strcat "\\P" (cdr adr) "\\P" (cdr cit) ", " (cdr sta) " " (cdr zip))))
					""
				)
			nme (if (setq nme (assoc "Contact Name" inf)) (strcase (strcat "\\P" (cdr nme))) "")
			eml (if (setq eml (assoc "Contact Email" inf)) (strcase (strcat "\\P" (cdr eml))) "")
		)
		(strcat 
			"\\pxsm1,ql;{\\H1.0X;\\L"
			hdr
			"\\H0.5X;\\l\\P\\psm1.5;" 
			cmp
			adr
			nme
			eml
			"}"
		)
	)
	;; RETURNS FORMATTED STRING FROM PASSED DATA
	
	(defun CreateContactTextObjects ( dat / CreateMTextObject ptl str )
		
		(defun CreateMTextObject ( str pt )
			(entmake
				(list
					(cons 0 "MTEXT")			; Entity Type
					(cons 100 "AcDbEntity")		; Subclass Marker
					(cons 100 "AcDbMText")		; Subclass Marker
					(cons 10 pt)				; Insertion Point (e.g., '(0 0 0))
					(cons 8 "G-ANNO-TEXT")		; Entity Layer Name
					(cons 40 0.2000)			; Default Text Height (Reference height)
					(cons 71 1)					; Attachment Point: 1 = Top Left
					(cons 1 str)				; Your Formatting String
				)
			)
		)
		;; CREATE TEXT ENTITY OBJECT

		;; CALCULATE POINT LIST BASED ON LENGTH OF DATA
		(cond
			((<= (length dat) 6)
				(setq ptl
					(list
						'(7.7471 16.0836 0.0)	'(12.0624 16.0836 0.0)
						'(7.7471 13.3836 0.0)	'(12.0624 13.3836 0.0)
						'(7.7471 10.6836 0.0)	'(12.0624 10.6836 0.0)
					)
				)
				;; POINT LIST FOR 1 - 6 CONTACTS
			)
			((<= (length dat) 8)
				(setq ptl
					(list
						'(7.7471 16.0836 0.0)	'(12.0624 16.0836 0.0)
						'(7.7471 14.2836 0.0)	'(12.0624 14.2836 0.0)
						'(7.7471 12.4836 0.0)	'(12.0624 12.4836 0.0)
						'(7.7471 10.6836 0.0)	'(12.0624 10.6836 0.0)
					)
				)
				;; POINT LIST FOR 7 - 8 CONTACTS (MAXIMUM ALLOWED)
			)
		)
		(if ptl
			(foreach dt dat
				(setq str (StringBuilder dt))
				;; CREATE FORMATTED STRING
				(CreateMTextObject str (car ptl))
				;; CREATE MTEXT OBJECT USING STRING AND POINT LIST
				(setq ptl (cdr ptl)) ;; SET POINT LIST TO NEXT VALUES
			)
		) ;; RETURNS NIL IF POINT LIST IS 0 OR GREATER THAN 8
	)
	;; CREATES MULTI-TEXT OBJECTS BASED ON DATA
	;; BUILDS HEADER AND CATEGORIES BASED ON DATA
	
	(setq osm (getvar "OSMODE"))
	(setvar "OSMODE" 0)
	
	(setq dir (vl-filename-directory (getvar "DWGPREFIX")))
	(setq fl (getfiled "Select BOD file with contact information" (strcat dir "\\") "xlsx;csv" 0))
	(setq cts
		(list
			"Project Owner"
			"EPC Contact"
			"AHJ Contact"
			"Utility Contact"
			"Civil Engineer"
			"Electrical Engineer"
			"Geotechnical Engineer"
			"Racking Engineer"
			"Structural Engineer"
		)
	)
	
	(cond
		((null fl)
			nil
		) ;; EXIT QUIETLY
		((null (vl-every 'boundp '(msxl:OpenExcel msxl:CloseExcel msxl:ReturnActiveWorkBook msxl:ReturnWorkSheet msxl:FindMatchingCell))) ;; CHECK THAT MSXL LIBRARY FUNCTIONS ARE LOADED
			(alert "MSXL -> LISP file not loaded or functions missing. Please load the 'LIB - MSXL Library Functions.lsp' file before using this function or check the file.")
		)
		((null (vl-every 'boundp '(std:DynamicToggleBox std:ObjectRelease))) ;; CHECK THAT STANDARD LIBRARY FUNCTIONS ARE LOADED, PROVES THAT THE LIBRARY FILE WAS LOADED AND ACTIVE
			(alert "Standard library functions not loaded. Please load the 'LIB - STD Sub-functions.lsp' file before using this function.")
		)
		((null (setq xl (msxl:OpenExcel fl t nil))) ;; OPEN A NEW INSTANCE OF EXCEL, AS TO AVOID FILE CLASHING
			(alert (strcat "Unable to open Excel file: " fl))
		)
		(	(null
				(progn
					(while
						(and
							(setq cts (std:DynamicToggleBox "Select contacts to include (8 max.)" cts nil))
							(> (length cts) 8)
						)
						(alert "Please select 8 contacts max.") ;; CONSTRAIN TO 8 CONTACTS MAXIMUM
					)
					cts
				)
			)
			nil ;; EXIT QUIETLY, NO ITEMS SELECTED
		)
		((null (setq dat (BODContactsParser xl cts))) ;; KEY VALUE PAIRS -> (cons "Project Owner" . (list (cons "Company Name" "Place") (cons "Contact Name" "Bob Doe") ... ) ... )
			nil ;; EXIT QUIETLY, ERROR HANDLED BY PARSER FUNCTION
		)
		( t ;; COMPLETED CHECKS AND BALANCES
			(ClearExistingTextObjects (list '(7.4971 16.3961 0.0) '(16.1058 8.7779 0.0)))
			;; UPPER LEFT CORNER	-> (7.4971 16.3961 0.0)
			;; LOWER RIGHT CORNER	-> (16.1058 8.7779 0.0)
			;; REMOVE EXISTING TEXT OBJECTS IN THE WINDOW LOCATION
			
			(CreateContactTextObjects dat)
			;; CREATE AUTOCAD TEXT OBJECTS WITH CONTACT INFORMATION
		)
	)
	
	(if osm (setvar "OSMODE" osm))
	
	(std:ObjectRelease (list cel ws wb))
	(msxl:CloseExcel xl)
	(gc)
	
	(princ)
)


(Load:DescriptionLog "UPDATECONTACTINFO / UCONI" "Update contact information on cover sheet")