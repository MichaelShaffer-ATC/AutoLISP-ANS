;; FUNCTION TO RENAME ALL LAYOUT TABS BASED ON THE ORDER THAT THEY ARE IN
;; USER SPECIFIES PREFIX FOR ALL SELECTED SHEETS AND SHEETS GET NUMBERED BASED ON THE TAB ORDER WITH A DEFINED PREFIX APPENDED TO THEM

(defun c:RNSL ( / )
	(c:RENAMESELECTEDLAYOUTS)
)

(defun c:RENAMESELECTEDLAYOUTS
	(
		/
		;Functions
		*error* RenameSelectedLayouts
		;DCL Functions
		DCL:RunDialogChecks DCL:Cancel
		DCL:LayoutList DCL:SelectAll
		DCL:UseExistingPrefix DCL:UpdatePreviewText
		DCL:TotalDigits DCL:LeadingZeros
		;DCL (Global Variables)
		lyts res
		;Variables
		ttl dcl tmp des dch pfx pad sht
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
	
	(defun RenameSelectedLayouts ( lyts sel pfx pad int / lyns new )
		(setq lyns (mapcar '(lambda ( s ) (strcase s)) (mapcar 'vla-get-name lyts)))
		(foreach lyt sel
			(while (member (setq new (strcase (strcat pfx (DCL:LeadingZeros pad (itoa int))))) lyns)
				(setq int (1+ int))
			)
			(vla-put-name lyt new)
			(setq int (1+ int))
		)
	)
	;; REBNAMES A PASSED LIST OF LAYOUT OBJECTS (SEL) COMPARED TO THE ORDERED LAYOUT LIST (LYTS)
	;; COMBINES THE PREFIX VALUE (PFX) AND AN INTEGER VALUE (INT) FOR SHEET NUMBER
	
	(vl-load-com)
	
	(setq ttl "Rename_Layouts")
	(setq dcl
		(list
			"// Temporary DCL file;"
			(strcat ttl " : dialog {")
			"	label = \"Layout Rename Dialog Control\";"
			"	: text {"
			"		value = \"**Layouts will be renumbered based on their current tab order.**\";"
			"	}"
			"	: row {"
			"		: column {"
			"			children_alignment = \"left\";"
			"			: list_box {"
			"				label = \"Existing layout names: \";"
			"				multiple_select = true;"
			"				height = 18;"
			"				width = 24;"
			"				key = \"lyts\";"
			"			}"
			"			: toggle {"
			"				label = \"Rename all layouts\";"
			"				value = \"0\";"
			"				key = \"all\";"
			"			}"
			"		}"
			"		: column {"
			"			: boxed_column {"
			"				label = \"Prefix details: \";"
			"				: toggle {"
			"					label = \"Use selected layout prefix\";"
			"					value = \"0\";"
			"					key = \"exst\";"
			"				}"
			"				: edit_box {"
			"					label = \"Layout prefix: \";"
			"					edit_width = 25;"
			"					is_enabled = true;"
			"					key = \"pfx\";"
			"				}"
			"			}"
			"			spacer_1;"
			"			: boxed_row {"
			"				label = \"Total digits: \";"
			"				children_alignment = \"top\";"
			"				: radio_button {"
			"					label = \"1 (#)\";"
			"					value = \"1\";"
			"					key = \"dgts_1\";"
			"				}"
			"				: radio_button {"
			"					label = \"2 (##)\";"
			"					value = \"0\";"
			"					key = \"dgts_2\";"
			"				}"
			"				: radio_button {"
			"					label = \"3 (###)\";"
			"					value = \"0\";"
			"					key = \"dgts_3\";"
			"				}"
			"				: radio_button {"
			"					label = \"4 (####)\";"
			"					value = \"0\";"
			"					key = \"dgts_4\";"
			"				}"
			"			}"
			"			: boxed_row {"
			"				label = \"Sheet number: \";"
			"					: edit_box {"
			"					label = \"Starting sheet number value: \";"
			"					value = \"1\";"
			"					key = \"val\";"
			"				}"
			"			}"
			"			spacer_1;"
			"			: boxed_row {"
			"				label = \"Updated layout name preview: \";"
			"				: text {"
			"					value = \"\";"
			"					key = \"prv\";"
			"				}"
			"			}"
			"			spacer_1;"
			"			errtile;"
			"			ok_cancel;"
			"		}"
			"	}"
			"}"
		)
	)
	
	(if
		(and
			dcl
			(setq tmp (vl-filename-mktemp "RNLT" nil ".dcl"))
			(setq des (open tmp "w"))
			(foreach line dcl (write-line line des))
			(not (close des))
			(> (setq dch (load_dialog tmp)) 0)
			(new_dialog ttl dch)
		)
	
		(progn
			;; ONLY LOAD FUNCTIONS FOR DIALOG CONTROLS IF DIALOG BOX CAN BE CREATED
			(defun DCL:RunDialogChecks ( / )
				(cond
					;; ADD CONDITIONAL LOGIC TO CHECK FOR HERE
					((vl-some '(lambda ( c ) (vl-string-search c (get_tile "pfx"))) (list "\\" "<" ">" "/" "?" "\"" ":" ";" "*" "|" "," "=" "`"))
						(set_tile "error" "Layout names cannot contain: \\ < > / ? \" : ; * | , = `")
					)
					((null res)
						(set_tile "error" "Error: No layouts are currently selected.")
					)
					((not (eq (type (read (get_tile "val"))) 'INT))
						(set_tile "error" "Error: Starting sheet value needs to be numeric.")
					)
					( t
						(setq
							pfx	(get_tile "pfx")
							pad (DCL:TotalDigits)
							sht (get_tile "val")
						)
						(done_dialog 1)
						(setq res (mapcar '(lambda ( x ) (nth x lyts)) (read (strcat "(" res ")")))) ;; RETURN SELECTED LAYOUT OBJECTS FROM LIST
						
						(RenameSelectedLayouts lyts res "$XYZ%" pad 0) ;; RENAME LAYOUTS TEMPORARILY
						(RenameSelectedLayouts lyts res pfx pad (atoi sht))
						;; "PREFIX-LEADINGZEROS-PAGENUMBER"
						
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
					(progn
						(set_tile "lyts"
							(repeat (setq i (length lyts))
								(setq res (strcat (itoa (setq i (1- i))) (if res (strcat " " res) "")))
							)
						)
						(DCL:UseExistingPrefix)
					)
					(set_tile "lyts" "")
				)
			)
			;; ADD ALL AVAILABLE LAYOUT TABS TO LIST
			
			(defun DCL:UseExistingPrefix ( / FindPrefixValue )
				(defun FindPrefixValue ( str )
					(while (and (> (strlen str) 0) (wcmatch (substr str (strlen str) 1) "#"))
						(setq str (substr str 1 (1- (strlen str))))
					)
					str
				) ;; CREATES PREFIX OF STRING BY REMOVING NUMERIC VALUES AT END (PAGE NUMBER)
				(if (= (get_tile "exst") "1")
					(progn
						(mode_tile "pfx" 1)
						(if (and lyts res)
							(set_tile "pfx" (FindPrefixValue (vla-get-name (nth (atoi (substr res 1 1)) lyts))))
						)
					)
					(mode_tile "pfx" 0)
				)
				(DCL:UpdatePreviewText)
			)
			;; USES EXISTING LAYOUT PREFIX BASED ON FIRST SELECTED LAYOUT
			
			(defun DCL:UpdatePreviewText ( / )
				(set_tile "prv" (strcat (get_tile "pfx") (DCL:LeadingZeros (DCL:TotalDigits) (get_tile "val"))))
			)
			;; UPDATES PREVIEW TEXT BY APPENDING PREFIX, LEADING ZEROS AND PAGE NUMBER TO CREATE STRING VALUE
			
			(defun DCL:TotalDigits ( / )
				(cond
					((= (get_tile "dgts_1") "1")
						1
					)
					((= (get_tile "dgts_2") "1")
						2
					)
					((= (get_tile "dgts_3") "1")
						3
					)
					((= (get_tile "dgts_4") "1")
						4
					)
				)
			)
			;; DETERMINES THE TOTAL NUMBER OF DIGITS TO USE BASED ON RADIO BUTTON TOGGLED FOR DIGITS (TOTAL)
			
			(defun DCL:LeadingZeros ( pad str )
				(if (< (strlen str) pad) (DCL:LeadingZeros pad (strcat "0" str)) str)
			)
			;; ADDS PADDING (LEADING ZEROS) TO MATCH THE USER DEFINED NUMBER OF DIGITS IN PAGE NUMBER
			
			;;;MAIN;;;
			(vlax-for lyt (vla-get-layouts (vla-get-activedocument (vlax-get-acad-object)))
				(if (eq (vla-get-modeltype lyt) :vlax-false) ;; IGNORES MODEL TAB
					(setq lyts (cons lyt lyts))
				)
			)
			(setq lyts (vl-sort lyts '(lambda ( a b ) (< (vla-get-taborder a) (vla-get-taborder b)))))
			(DCL:LayoutList)
			
			(action_tile "accept" "(DCL:RunDialogChecks)")
			(action_tile "cancel" "(DCL:Cancel)")
			
			(action_tile "lyts" "(setq res $value) (DCL:UseExistingPrefix)")
			(action_tile "all" "(DCL:SelectAll)")
			(action_tile "exst" "(DCL:UseExistingPrefix)")
			(action_tile "pfx" "(DCL:UpdatePreviewText)")
			(action_tile "val" "(DCL:UpdatePreviewText)")
			
			(action_tile "dgts_1" "(DCL:UpdatePreviewText)")
			(action_tile "dgts_2" "(DCL:UpdatePreviewText)")
			(action_tile "dgts_3" "(DCL:UpdatePreviewText)")
			(action_tile "dgts_4" "(DCL:UpdatePreviewText)")
			
			(start_dialog)
			(unload_dialog dch)
		)
		(prompt "\nError loading dialog box.")
	)
	
	(princ)
)