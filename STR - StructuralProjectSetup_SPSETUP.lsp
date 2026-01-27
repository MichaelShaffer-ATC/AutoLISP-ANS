;; PARSE DATA FROM SELECTED EXCEL BOD DOCUMENT AND USE THE DATA TO FILL OUT THE STRUCTURAL PROJECT COVER SHEET / DRAWING

;; FOR FUNCTIONS DEFINED OUTSIDE OF THIS SCOPE:
;; FOR MSXL SPECIFIC FUNCTIONS, REFER TO THE "LIB - MSXL Library Functions.lsp" FILE -> (msxl:) 
;; FOR STD SPECIFIC FUNCTIONS, REFER TO THE "LIB - STD Sub-Functions.lsp" FILE -> (std:)


(defun c:SPSETUP ( / )
	(c:StructuralProjectSetup)
)

(defun c:StructuralProjectSetup
	(
		/
		;Functions
		StructuralProjectSetup_Main
		ParseDataFromBOD
		ReturnProjectFolderById SwapDataKeys DMS_Conversion SetTableSuppression
		ReplaceDwgProp ReplaceTblText ReplaceCvrText
		;Variables
		dir id fdr fl
		dwp tbl cvr
		nts
		;Global
		kmap
	)
	
	;;; BOD FUNCTIONS ;;;
	
	(defun ParseDataFromBOD ( ws rng / res k v )
		(setq res '())
		(vlax-for c (msxl-get-cells rng)
			(if (and (not (msxl:IsMergedCell c)) (setq k (msxl:GetCellValue c)) (setq v (msxl:GetCellValue (msxl-get-offset c 0 1))))
				(setq res (cons (cons k v) res))
			)
		)
		res
	)
	;; RETURNS THE DATA PARSED FROM PASSED EXCEL WORKSHEET OBJECT AS A LIST
	;; DEPENDING ON HOW THE EXCEL CELLS WERE FORMATTED, A "BLANK" CELL MAY RETURN NIL OR ""; FUNCTION NEEDS TO CHECK FOR BOTH
	;; [  ws ]	== EXCEL WORKSHEET OBJECT
	;; [ rng ]	== RANGE OBJECT TO PARSE DATA FROM (Ex. (msxl-get-range ws "A1:C20"))
	;; (msxl-get-offset c 0 1) ;; GET CELL IN SAME ROW AND ONE COLUMN TO THE RIGHT
	;; (msxl-get-offset c 2 -1) ;; GET CELL 2 ROWS DOWN AND ONE COLUMN TO THE LEFT
	
	
	;;; MISC. FUNCTIONS ;;;
	
	(defun ReturnProjectFolderById ( dir id / res )
		(setq res
			(vl-remove-if-not
				'(lambda ( d )
					(= (substr (strcase d) 1 (vl-string-search " - " d)) (strcase id))
				)
				(vl-directory-files dir nil -1)
			)
		)
		(if (> (length res) 1)
			(car (std:MultiSelectListBox "Select Project Folder" res nil))
			(car res) ;; IF 'res' IS NIL THIS WILL NOT THROW AN ERROR AND RETURNS NIL
		)
	)
	;; CASE INSENSITIVE ID MATCHING FOR PROJECT FOLDERS WITHIN A PASSED DIRECTORY
	;; IF MULTIPLE DIRECTORIES ARE A MATCH TO THE 'ID', THE USER IS PROMPTED TO SELECT THE FOLDER FROM A LIST OF MATCHING FOLDER ID'S
	;; FUNCTION UTILIZES THE STD:MULTISELECTLISTBOX FUNCTION FROM THE STANDARD LIBRARY LISP FILE
	;; RETURNS THE MATCHING PROJECT FOLDER STRING IF A MATCH IS FOUND, ELSE NIL
	;; [ dir ]	== DIRECTORY (LOCAL) TO SEARCH WITHIN
	;; [ id  ]	== ID NUMBER/NAME ASSOCIATED WITH PREOJECT TO SEARCH FOR
	
	
	(defun SwapDataKeys ( dat map )
		(mapcar
			'(lambda ( p )
				(if (vl-member-if '(lambda ( m ) (= (car m) (car p))) map)
					(cons (cdr (assoc (car p) map)) (cdr p))
					p
				)
			)
			dat
		)
	)
	;; IF THE KEY IS FOUND IN THE DATA LIST FROM THE MAP, THE KEY IS SWAPPED WITH THE VALUE FROM THE MAP
	;; A RECREATED KEY VALUE PAIR WITH SWAPPED KEYS IS RETURNED, ELSE NIL IF NO MATCHES ARE FOUND BETWEEN THE DATA LIST AND THE MAP
	;; [ dat ]	== KEY VALUE PAIRS OF DATA
	;; [ map ]	== KEY VALUE PAIR MAP OF KEY VALUES TO SWAP WITH FROM PASSED DATA PAIRS
	
	
	(defun DMS_Conversion ( ltlg / ZeroPad Convert )
		(defun ZeroPad ( val len )
			(while (< (strlen val) len)
				(setq val (strcat "0" val))
			)
			val
		) ;; ADD 0 PADDING TO BEGINNING OF STRING UNTIL 'LEN' IS MET
		(defun Convert ( val ltf / sw dg mn )
			(if (minusp val) (setq sw t))
			(strcat
				(ZeroPad (itoa (setq dg (fix (abs val)))) 2) (chr 176) " "		;; DEGREES "°"
				(ZeroPad (itoa (fix (setq mn (* (- (abs val) dg) 60)))) 2) "' "	;; MINUTES "'"
				(ZeroPad (rtos (* (abs (- (fix mn) mn)) 60) 2 2) 5) "\" "		;; SECONDS "\""
				(if ltf
					(if (not sw) "N" "S")
					(if (not sw) "E" "W")
				)
			)
		)
		(if (and (listp ltlg) (car ltlg) (cdr ltlg))
			(progn
				(if (= (type (car ltlg)) 'STR)
					(setq ltlg (cons (atof (car ltlg)) (cdr ltlg)))
				)
				(if (= (type (cdr ltlg)) 'STR)
					(setq ltlg (cons (car ltlg) (atof (cdr ltlg))))
				)
				(cons (Convert (car ltlg) t) (Convert (cdr ltlg) nil))
			)
		)
	)
	;; CONVERTS A DECIMAL LATITUDE AND LONGITUDE PAIR TO DMS VERSION (DEGREES MINUTES SECONDS) PAIR
	;; RETURNS THE CONVERTED PAIR VALUE IF SUCCESSFUL, ELSE RETURNS NIL
	;; [ ltlg ]	== LATITUDE / LONGITUDE PAIR AS STRINGS OR NUMERIC FLOATING POINT VALUE
	;; EX: ( ##.##### . ##.##### ) -> ( ##° ##' ##.##" DRCT . ##° ##' ##.##" DRCT ) (DRCT -> N, S, E, W)
	
	
	(defun SetTableSuppression ( tbl flg )
		(if (and tbl (vlax-property-available-p tbl 'regeneratetablesuppressed t))
			(progn
				(vla-put-regeneratetablesuppressed tbl (if flg :vlax-true :vlax-false))
				(vla-update tbl)
			)
		)
	)
	;; SETS TABLE REGENERATION FOR PASSED TABLE BASED ON PASSED FLAG VALUE
	;; [ tbl ]	== AUTOCAD TABLE OBJECT
	;; [ flg ]	== BOOLEAN FLAG TO SET REGENERATION SUPPRESSION EITHER ON OR OFF
	
	
	;; FIND AND REPLACING FUNCTIONS ;;
	
	(defun ReplaceDwgProp ( dwp kvp / res )
		(if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-getcustombykey (list dwp (car kvp) 'res))))
			(vla-setcustombykey dwp (car kvp) (strcase (std:ConvertToString (cdr kvp))))
		)
		(not (null res))
	)
	;; RETURNS T AND REPLACES DRAWING PROPERTY VALUE IF KEY IS FOUND, ELSE RETURNS NIL
	;; [ dwp ]	== DRAWING PROPERTY SUMMARY INFO
	;; [ kvp ]	== KEY VALUE PAIR TO SEARCH KEY VALUE FOR WITHIN DRAWING PROPERTY, PASSED VALUE TO REPLACE EXISING DRAWING PROPERTY VALUE IF FOUND
	
	
	(defun ReplaceTblText ( tbl kvp / row col res )
		(setq row 0 col 0)
		(while (and (null res) (not (> row (vla-get-rows tbl))) (not (> col (vla-get-columns tbl))))
			(if (= (std:LM:UnFormat (strcase (vla-gettext tbl row col)) nil) (strcase (car kvp)))
				(progn (setq res t) (vla-settext tbl row (1+ col) (strcase (std:ConvertToString (cdr kvp)))))
			)
			(cond
				((= col (vla-get-columns tbl))
					(setq row (1+ row) col 0)
				)
				( t
					(setq col (1+ col))
					(if (> col (vla-get-columns tbl))
						(setq row (1+ row) col 0)
					)
				)
			)
		)
		res
	)
	;; RETURNS T AND REPLACES TABLE TEXT VALUE IF KEY TEXT IS FOUND, ELSE RETURNS NIL
	;; [ tbl ]	== AUTOCAD TABLE OBJECT TO SEARCH KEY VALUE IN
	;; [ kvp ]	== KEY VALUE PAIR TO SEARCH KEY VALUE FOR WITHIN AUTOCAD TABLE, PASSED VALUE TO REPLACE EXISING AUTOCAD TABLE TEXT IF FOUND
	
	
	(defun ReplaceCvrText ( cvr kvp / res )
		(vlax-for obj (vla-get-block cvr)
			(if (and (null res) (= (vla-get-objectname obj) "AcDbMText"))
				(if (vl-string-search (strcase (car kvp)) (strcase (vla-get-textstring obj)))
					(progn (setq res t) (vla-put-textstring obj (vl-string-subst (strcase (std:ConvertToString (cdr kvp))) (strcase (car kvp)) (vla-get-textstring obj))))
				)
			)
		)
		res
	)
	;; RETURNS T AND REPLACES TEXT VALUE WITHIN COVER SHEET IF KEY IS FOUND, ELSE RETURNS NIL
	;; [ cvr ]	== COVER SHEET OBJECT (FIRST LAYOUT) TO SEARCH KEY IN
	;; [ kvp ]	== KEY VALUE PAIR TO SEARCH KEY VALUE FOR WITHIN COVER SHEET, PASSED VALUE TO REPLACE EXISING COVER SHEET TEXT IF FOUND
	;; TEXT FROM DRAWING MUST BE SET TO UPPERCASE FOR FUNCTION TO WORK PROPERLY DURING COMPARISON
	
	;; HELPER FUNCTION ;;
	(defun StructuralProjectSetup_Main ( fl / *error* xl wb ws dat ltlg dwp tbl cvr )
		
		(defun *error* ( msg )
			(if (not (member msg (list "Function cancelled" "quit / exit abort")))
				(vl-bt)
			)
			(SetTableSuppression tbl nil) ;; RESET TABLE SUPPRESSION IF PREVIOUSLY SET
			;; !!MAKE SURE TO RELEASE ALL EXCEL RELATED OBJECTS!!
			(std:ObjectRelease (list ws wb))
			(msxl:CloseExcel xl)
			(gc)
			(princ (strcat "\nAn error occurred: " msg))
		)
		;; ERROR HANDLING
		
		(if (and (setq xl (msxl:OpenExcel fl t nil)) (setq wb (msxl:ReturnActiveWorkBook xl)) (setq ws (msxl:ReturnWorkSheet wb "General"))) ;; OPEN EXCEL FILE READ ONLY AND NOT VISIBLE, RETURN ACTIVE WORKBOOK
			(progn
				;; PARSE DATA FROM "General" WORKSHEET
				(if (null (setq dat (ParseDataFromBOD ws (msxl-get-range ws "B6:B23")))) ;; RETURN COLUMN RANGE ONLY
					(princ "\nNo data was returned from specified address within the Excel file.")
					(progn
						(setq ltlg (DMS_Conversion (cons (cdr (assoc "Project Site Latitude" dat)) (cdr (assoc "Project Site Longitude" dat)))))
						(setq dat (append dat (list (cons "Latitude:" (car ltlg)) (cons "Longitude:" (cdr ltlg)))))
						;; ADD LATITUDE AND LONGITUDE DMS VALUES FOR AUTOCAD TABLE EDIT, VALUES DO NOT EXIST IN BOD
						(setq dwp (vla-get-summaryinfo (vla-get-activedocument (vlax-get-acad-object))))
						(setq tbl (if (setq tbl (ssget "_X" (list (cons 0 "ACAD_TABLE") (cons 1 "SITE INFORMATION")))) (vlax-ename->vla-object (cadar (ssnamex tbl)))))
						(if (vl-catch-all-error-p (setq cvr (vl-catch-all-apply 'vla-item (list (vla-get-layouts (vla-get-activedocument (vlax-get-acad-object))) "S-0001")))) (setq cvr nil))
						;;; BOOKMARK ;;;
						;; GOING TO NEED SOME ERROR CATCHING HERE
						;; WILL NEED A WAY TO CONSISTENTLY RETURN THE FIRST LAYOUT IN A DRAWING FILE WITHOUT SUPPLYING THE SHEET NAME
						(SetTableSuppression tbl t)
						(foreach itm (SwapDataKeys dat kmap)
							(cond
								( (ReplaceDwgProp dwp itm) )
								( (ReplaceTblText tbl itm) )
								( (ReplaceCvrText cvr itm) )
							)
							;; TEST AND REPLACE TEXT IN MULTIPLE LOCATIONS, ONLY ONE LOCATION (DWG PROPS / TABLE / MULTILINE TEXT) WILL BE UPDATED FOR EACH ITEM IN DATA
						)
						(SetTableSuppression tbl nil)
						(vl-cmdf "._REGEN")
						
						;;; BOOKMARK ;;;
						
						;; FILL OUT INITIALS BASED ON USER NAME?
						;; ISSUE HERE IS THAT THE LOGINNAME HAS EXTRA CHARACTERS AT THE END FOR MOST PEOPLE
						;; WILL NEED TO FIGURE OUT A USEFUL ALTERNATIVE HERE
						
						;; UPDATE CLIENT LOGO VISIBILITY
						(UpdateClientLogo (cdr (assoc "Client Name" dat)))
						
						;; UPDATE MAP VISIBILITY
						(UpdateStateMap (cdr (assoc "Project State" dat)))
						
						;; ADD .DLL CALL TO MAP IMAGE HERE, MAYBE? NEED TO MAKE SURE THE .DLL WILL RUN WHILE EXCEL IS OPEN IN THE BACKGROUND
						
						;; UPDATE GENERAL NOTES
						;(if (boundp 'c:GENERALNOTES) (setq nts (c:GENERALNOTES)))
						
					)
				)
			)
		)
		
		(std:ObjectRelease (list ws wb))
		(msxl:CloseExcel xl)
		(gc)
	)
	;; INTERMEDIATE LOGIC FOR PROJECT SETUP (GET BOD DOCUMENT, PARSE DATA, FILL DATA)
	;; [ fl ]	== FILE NAME OF EXCEL BOD
	
	
	(setq kmap
		(list
			(cons "Client Corporate Home Base"	"Client Address")			;DWG PROPERTY ("CLIENT ADDRESS") 	<- EPC INFORMATION
			(cons "Client Name"					"Client Name")				;DWG PROPERTY ("CLIENT NAME") 		<- EPC INFORMATION
			(cons "Project Number"				"Project No.")				;DWG PROPERTY ("PROJECT NO.")
			(cons "Owner Corporate Home Base"	"Owner Company Address")	;MULTITEXT OBJECT ("PROJECT OWNER" -> "OWNER COMPANY NAME") 	<- OWNER INFORMATION
			(cons "Owner Name"					"Owner Company Name")		;MULTITEXT OBJECT ("PROJECT OWNER" -> "OWNER COMPANY ADDRESS")	<- OWNER INFORMATION
			(cons "Project Name"				"Project Name")				;DWG PROPERTY ("PROJECT NAME")
			;; "Special Purpose Entity", "Target COD"
			(cons "Project City"				"Town:")					;TABLE TEXT OBJECT
			(cons "Project Coordinate System"	"Coord. System:")			;TABLE TEXT OBJECT
			(cons "Project County"				"County")					;DWG PROPERTY
			;(cons "Project Site Address"		"$XYZ")						;N/A
			(cons "Project Site Latitude"		"N:")						;TABLE TEXT OBJECT -> CONVERT TO DMS FOR LATITUDE TEXT IN TABLE
			(cons "Project Site Longitude"		"E:")						;TABLE TEXT OBJECT -> CONVERT TO DMS FOR LINGITUDE TEXT IN TABLE
			;(cons "Latitude:"					(DMS_Conversion "Latitde"))	
			;(cons "Longitude:"					(DMS_Conversion "Longitude"))	
			(cons "Project State"				"State")					;DWG PROPERTY
			;(cons "$XYZ"						"Project ZIP Code")			;N/A
		)
	)
	;; GLOBAL KEY MAP FROM BOD DATA TO AUTOCAD TEMPLATE DATA ; GLOBAL VARIABLE
	
	
	;; FUNCTION ENTRY POINT
	(setq dir "C:\\Users\\MichaelShaffer\\OneDrive - ANS Geo\\File Transfer\\AutoLISP\\BOD Excel Data Transfer\\BOD References\\")	;; TESTING FOR BOD PULL ONLY
	;(setq dir "C:\\_ACC\\ACCDocs\\FastGrid\\")
	
	(cond
		((zerop (strlen (setq id (vl-string-trim " " (getstring "Enter the ACC project id number: ")))))
			(alert "Project id is invalid.")
			(princ "Function cancelled.")
		)
		((null (setq fdr (ReturnProjectFolderById dir id)))
			(alert "Project not found in local environment.\nMake sure you've selected the required project using Autodesk Desktop Connector first.")
			(princ "Function cancelled.")
		)
		((null (setq fl (getfiled "Select the project BOD Excel file" (strcat dir fdr "\\") "xlsx;csv" 0)))
			(princ "Function cancelled.")
		)
		( t
			(StructuralProjectSetup_Main fl)
		)
	)
	;; MAY NEED TO SELECT CLIENT NAME AS WELL SINCE SOME PROJECT ID'S CAN AND WILL BE DUPLICATES
	;; IF THERE ARE MULTIPLE PROJECTS UNDER THE SAME ID HAVE A SELECTION BOX DISPLAY THE OPTIONS
	
	(princ)
)


;;; NOTES ;;;

;;  (zerop (getvar "DWGTITLED")) -> CHECKS IF DRAWING IS SAVED AND IS NOT A NEWLY CREATED DRAWING FILE
;; THIS FUNCTION SHOULD ONLY BE RUN WITHIN NEWLY CREATED DRAWING FILES

;; SPSV SHOULD BE USED TO SAVEAS OVER EXISTING PROJECTS IF A COPY NEEDS TO BE MADE... WILL NEED TO VERIFY HOW THIS SHOULD WORK


;;; BOOKMARK ;;;
;; ADD LEADING ZEROS TO DMS FORMATTING FOR NUMBERS LESS THAN 2 DIGITS IE. "9" -> "09"
;;	--> (if (< (strlen str) pad) (DCL:LeadingZeros pad (strcat "0" str)) str)
;; FIND A WAY TO SKIP MERGED CELLS OR "" / NIL ITEMS FROM THE EXCEL DOCUMENT BETTER
;;	--> CURRENT ERROR IS CAUSED BY EXCEL DOCUMENT CONTAINING A MERGED CELL (HEADER) THAT IS TREATED AS A VALUE WITHIN THE LIST
;;		(strcase (car kvp)) ENDS UP BREAKING WITHIN THE REPLACEMENT FUNCTIONS