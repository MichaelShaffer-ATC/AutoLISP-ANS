;; FUNCTION TO SET UP THE REVISION VALUES WITHIN THE TITLEBLOCKS ON VARIOUS SHEETS
;; USER IS ABLE TO SELECT SPECIFIC SHEETS FOR INSTANCES WHERE REVISIONS ONLY APPLY TO SPECIFIC SHEETS AND NOT THE WHOLE PROJECT
;; UPDATED CODE: IF PREVIOUS REVISION WAS ALPHABETIC AND NEW REVISION IS NUMERIC, CLEAR ALL PREVIOUS REVISIONS
;; ADDED ADDITIONAL REVISION VALUES AND CONSOLIDATED EXISTING CODE
;; UPDATED: 09/30/25 REMOVED 'APR' VARIABLE AND 'APPROVED_' REFERENCES DUE TO TITLE BLOCK UPDATE NO LONGER NEEDING THESE VALUES
;; CHANGED ATTRIBUTES TO SHOW '_REV' IN ADDITION TO 'REV' WITHIN THE TITLE BLOCK. CODE HAS BEEN UPDATED TO CHANGE BOTH LOCATIONS.

(defun c:REVX
	(
		/
		;Functions
		*error*
		ReturnLastAttributeValue ReturnNextAttribute ClearAttributeValues ReplaceAttributeValue
		FilterAttributesByTagString GetRevisionType
		UpdateTitleBlockAttributes
		;DCL Functions
		DCL:RunDialogChecks DCL:LayoutList DCL:Cancel DCL:SelectAll
		;DCL (Global Variables)
		ttl dcl tmp des dch lyts res rev date desc clr
		;Variables
		tblks
	)
	
	(defun *error* ( msg )
		(if (< 0 dch)
			(unload_dialog dch)
		)
		(if (eq (type des) 'FILE)
			(close des)
		)
		(if (and (eq (type tmp) 'STR) (setq tmp (findfile tmp)))
			(vl-file-delete tmp)
		)
		(if (not (member msg (list "Function cancelled" "quit / exit abort")))
			(vl-bt)
		) ;; FOR DEBUGGING
		(strcat "\nError: " msg)
	)
	
	(defun ReturnLastAttributeValue ( blk tag )
		(car
			(vl-remove 'nil
				(mapcar
					'(lambda ( att )
						(if (/= (vla-get-textstring att) "")
							(vla-get-textstring att)
						)
					)
					(FilterAttributesByTagString tag blk)
				)
			)
		)
	)
	;; RETURNS ATTRIBUTE VALUE OF PASSED ATTRIBUTE TAG WITHIN PASSED BLOCK OBJECT IF IT EXISTS
	
	(defun ReturnNextAttribute ( blk tag )
		(car
			(vl-remove 'nil
				(mapcar
					'(lambda ( att )
						(if (= (vla-get-textstring att) "")
							att
						)
					)
					(FilterAttributesByTagString tag blk)
				)
			)
		)
	)
	;; SEARCHES A PASSED BLOCK FOR A MATCHING ATTRIBUTE TAG PATTERN AND RETURNS THE LATEST ATTRIBUTE TAG THAT IS NULL IF FOUND
	
	(defun ClearAttributeValues ( blks tag )
		(mapcar
			'(lambda ( blk )
				(mapcar
					'(lambda ( att )
						(vla-put-textstring att "")
					)
					(FilterAttributesByTagString tag blk)
				)
			)
			blks
		)
	)
	;; CLEARS ALL ATTRIBUTE VALUES THAT MATCH THE TAG STRING AND DO NOT INCLUDE A "0" IN THE TAG
	
	(defun ReplaceAttributeValue ( att val blk )
		(mapcar
			'(lambda ( x )
				(if (= att (vla-get-tagstring x)) (vla-put-textstring x val))
			)
			(vlax-safearray->list (vlax-variant-value (vla-getattributes blk)))
		)
	)
	;; REPLACES EXISTING ATTRIBUTE VALUE WITH PASSED VALUE 'VAL'
	
	(defun FilterAttributesByTagString ( tag blk / ptn )
		(setq ptn (strcat tag "*"))
		(vl-remove-if-not
			'(lambda ( att )
				(wcmatch (vla-get-tagstring att) ptn)
			)
			(vlax-safearray->list (vlax-variant-value (vla-getattributes blk)))
		)
	)
	;; FILTERS OUT ATTRIBUTES THAT DO NOT MATCH TAG STRING
	
	(defun GetRevisionType ( blk / val )
		(type
			(read
				(if (null (setq val (ReturnLastAttributeValue blk "REV")))
					rev ;; GLOBAL, SET BY DCL SELECTION
					val
				)
			)
		)
	)
	;; COMPARES REVISION TYPES (SYM VS INT) OF PREVIOUS REVISIONS (IF ANY) AND NEW REVISION SELECTED FROM DCL
	
	(defun UpdateTitleBlockAttributes ( blks map )
		(mapcar
			'(lambda ( blk )
				(mapcar
					'(lambda ( pair )
						(	(lambda ( tag val nxt )
								(if nxt
									(vla-put-textstring nxt val)
								) ;; REPLACE EXISTING ATTRIBUTE VALUE
							)
							(car pair)
							(cdr pair)
							(ReturnNextAttribute blk (car pair))
						)
					)
					map
				)
			)
			blks
		)
	)
	;; RECURSIVE FUNCTION TO UPDATE TITLEBLOCK INFORMATION, MAKES CALLS TO SEVERAL FUNCTIONS, USED AS A HELPER TO UPDATE DATA
	
	
	(vl-load-com)
	
	(setq ttl "Revision")
	(setq dcl
		(list
			"// Temporary DCL file;"
			(strcat ttl " : dialog {")
			"	label = \"Revision Setup Dialog Control\";"
			"	: row {"
			"		: column {"
			"			children_alignment = \"left\";"
			"			: list_box {"
			"				label = \"Existing layout names:\";"
			"				multiple_select = true;"
			"				height = 18;"
			"				width = 24;"
			"				key = \"lyts\";"
			"			}"
			"			: toggle {"
			"				label = \"Select all layouts\";"
			"				value = \"1\";"
			"				key = \"all\";"
			"			}"
			"			: toggle {"
			"				label = \"Clear existing rev's\";"
			"				value = \"0\";"
			"				key = \"clr\";"
			"			}"
			"		}"
			"		: column {"
			"			: row {"
			"				label = \"Revision Value:\";"
			"				: radio_column {"
			"					key = \"prlm\";"
			"					children_alignment = \"left\";"
			"					: radio_button {"
			"						label = \"Revision A\";"
			"						key = \"revA\";"
			"						value = \"1\";"
			"					}"
			"					: radio_button {"
			"						label = \"Revision B\";"
			"						key = \"revB\";"
			"					}"
			"					: radio_button {"
			"						label = \"Revision C\";"
			"						key = \"revC\";"
			"					}"
			"					: radio_button {"
			"						label = \"Revision D\";"
			"						key = \"revD\";"
			"					}"
			"					: radio_button {"
			"						label = \"Revision E\";"
			"						key = \"revE\";"
			"					}"
			"					: radio_button {"
			"						label = \"Revision F\";"
			"						key = \"revF\";"
			"					}"
			"				}"
			"				: radio_column {"
			"					key = \"finl\";"
			"					children_alignment = \"left\";"
			"					: radio_button {"
			"						label = \"Revision 0\";"
			"						key = \"rev0\";"
			"					}"
			"					: radio_button {"
			"						label = \"Revision 1\";"
			"						key = \"rev1\";"
			"					}"
			"					: radio_button {"
			"						label = \"Revision 2\";"
			"						key = \"rev2\";"
			"					}"
			"					: radio_button {"
			"						label = \"Revision 3\";"
			"						key = \"rev3\";"
			"					}"
			"					: radio_button {"
			"						label = \"Revision 4\";"
			"						key = \"rev4\";"
			"					}"
			"					: radio_button {"
			"						label = \"Revision 5\";"
			"						key = \"rev5\";"
			"					}"
			"				}"
			"			}"
			"			spacer_1;"
			"			: boxed_column {"
			"				label = \"Revision Details:\";"
			"				: edit_box {"
			"					key = \"desc\";"
			"					label = \"Description:\";"
			"					edit_limit = 32;"
			"					value = \"32 CHARACTERS MAX.\";"
			"				}"
			"				: edit_box {"
			"					key = \"date\";"
			"					label = \"Revision Date:\";"
			"					value = \"mm/dd/yy\";"
			"				}"
			"			}"
			"		}"
			"	}"
			"	spacer_1;"
			"	errtile;"
			"	ok_cancel;"
			"}"
		)
	)
	
	(if
		(and
			dcl
			(setq tmp (vl-filename-mktemp "REVX" nil ".dcl"))
			(setq des (open tmp "w"))
			(foreach line dcl (write-line line des))
			(not (close des))
			(> (setq dch (load_dialog tmp)) 0)
			(new_dialog ttl dch)
		)
		(progn
			(defun DCL:RunDialogChecks ( / )
				(cond
					;; ADD CONDITIONAL LOGIC TO CHECK FOR HERE
					((null res)
						(set_tile "error" "Error: No layouts are selected.")
					)
					((or (= (get_tile "desc") "") (= (get_tile "date") "") (= (get_tile "apr") ""))
						(set_tile "error" "Error: Description, date, or approved by values cannot be blank.")
					)
					( t
						(setq 
							date (get_tile "date")
							desc (strcase (get_tile "desc"))
							rev (substr rev (strlen rev) 1) ;; RETURNS THE LAST CHARACTER VALUE OF KEY ("revA" -> "A") ;; BASED ON HARDCODED VALUE IN DCL CODE
							clr (if (= (get_tile "clr") "1") t nil)
						)
						(done_dialog 1)
						(setq res (mapcar '(lambda ( x ) (nth x lyts)) (read (strcat "(" res ")")))) ;; RETURN SELECTED LAYOUT OBJECTS FROM LIST
						
						(if (setq tblks (ssget "_X" (append (list (cons 0 "INSERT") (cons 2 "*Title Block*") (cons -4  "<OR")) (mapcar '(lambda ( x ) (cons 410 x)) (mapcar 'vla-get-name res)) (list (cons -4 "OR>")))))
							(progn
								(setq
									tblks (mapcar 'vlax-ename->vla-object (mapcar 'cadr (ssnamex tblks)))
									tblks (vl-remove-if-not '(lambda ( blk ) (eq :vlax-true (vla-get-hasattributes blk))) tblks)
								) ;; REMOVES ALL TITLEBLOCKS THAT DO NOT HAVE ATTRIBUTED DATA
								(if tblks
									(progn
										(if (or clr (not (apply '= (append (list (type (read rev))) (mapcar 'GetRevisionType tblks))))) ;; CHECKS AGAINST PREVIOUS REVISIONS OR MISALIGNED REVISION VALUES ON ALL LAYOUTS
											(mapcar '(lambda ( tag ) (ClearAttributeValues tblks tag)) (list "REV_" "DESCRIPTION_" "DATE_"))
										) ;; CLEAR ALL OTHER REVISION VALUES IF "CLR" IS TOGGLED OR IF NEW REVISION TYPE DOES NOT MATCH THE LATEST EXISTING REVISION TYPE ("A" 'SYM -> "0" 'INT)
										(mapcar '(lambda ( blk ) (ReplaceAttributeValue "REV" rev blk)) tblks)
										;; QUICK FIX TO ALLOW FOR REVISION UPDATE IN OLDER TITLE BLOCK
										(UpdateTitleBlockAttributes
											tblks
											(list
												(cons "REV_" rev)
												(cons "DESCRIPTION_" desc)
												(cons "DATE_" date)
											)
										) ;; CONSOLIDATES FREQUENT CALLS TO OTHER FUNCTIONS
										(if (boundp 'c:UPDATESHEETINDEX) (c:UPDATESHEETINDEX)) ;; FUNCTION DEFINED ELSWHERE, NEED TO VERIFY IT EXISTS FIRST THEN RUN
									)
									(princ "\nNo attribute data was found within title blocks.")
								)
							)
							(princ "\nNo valid title blocks found in drawing.")
						)
						(if (findfile tmp)
							(vl-file-delete tmp)
						)
					)
				)
			)
			;; RUNS INNER FUNCTION DIALOG BOX CHECKS FOR ERRORS
			
			(defun DCL:LayoutList ( / )
				(start_list "lyts")
				(foreach lyt lyts (add_list (vla-get-name lyt)))
				(end_list)
			)
			;; ADDS LAYOUT NAMES TO LIST BOX FOR USER SELECTION
			
			(defun DCL:Cancel ( / )
				(done_dialog 0)
				(if (findfile tmp)
					(vl-file-delete tmp)
				)
			)
			;; CLEANS UP BACKEND IF PROGRAM IS CANCELED
			
			(defun DCL:SelectAll ( / i )
				(setq res nil)
				(if (and lyts (= (get_tile "all") "1"))
					(set_tile "lyts"
						(repeat (setq i (length lyts))
							(setq res (strcat (itoa (setq i (1- i))) (if res (strcat " " res) "")))
						)
					)
					(set_tile "lyts" "")
				)
			)
			;; ADD ALL AVAILABLE LAYOUT TABS TO LIST
			
			(defun DCL:ClearRadioColumn ( rev )
				(mapcar
					'(lambda ( b )
						(if (= (get_tile b) "1")
							(set_tile b "0")
						)
					)
					(if (member rev (list "revA" "revB" "revC" "revD" "revE" "revF"))
						(list "rev0" "rev1" "rev2" "rev3" "rev4" "rev5")
						(list "revA" "revB" "revC" "revD" "revE" "revF")
					)
				)
			)
			;; SETS OPPOSITE RADIO COUMN VALUES TO 0 WHENEVER A RADIO BUTTON IS SELECTED IN BOTH COLUMNS
			
			;;;MAIN;;;
			(vlax-for lyt (vla-get-layouts (vla-get-activedocument (vlax-get-acad-object)))
				(if (eq (vla-get-modeltype lyt) :vlax-false) ;; IGNORES MODEL TAB
					(setq lyts (cons lyt lyts))
				)
			)
			(setq lyts (vl-sort lyts '(lambda ( a b ) (< (vla-get-taborder a) (vla-get-taborder b)))))
			
			(setq rev (get_tile "prlm")) ;; INITIALIZE REVISION VALUE TO "revA"
			(DCL:LayoutList)
			(DCL:SelectAll) ;; TOGGLE FOR ALL LAYOUTS IS SELECTED BY DEFAULT
			
			(action_tile "accept" "(DCL:RunDialogChecks)")
			(action_tile "cancel" "(DCL:Cancel)")
			
			(action_tile "lyts" "(progn (setq res $value) (set_tile \"all\" \"0\"))")
			(action_tile "all" "(DCL:SelectAll)")
			
			(action_tile "prlm" "(DCL:ClearRadioColumn (setq rev $value))")
			(action_tile "finl" "(DCL:ClearRadioColumn (setq rev $value))")
			
			(start_dialog)
			(unload_dialog dch)
		)
		(prompt "\nError loading dialog box.")
	)
	(princ)
)
