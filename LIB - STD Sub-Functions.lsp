;; THIS FILE CONTAINS SUB-FUNCTIONS THAT ARE GLOBALLY AVAILABLE TO ALL OTHER LISP FUNCTIONS

;;; GLOBAL SUBFUNCTIONS ;;;



;;; DYNAMIC DCL BOXES ;;;

(defun MultiSelectListBox ( msg lst mlt / dcl tmp des dch rtn )
	(setq dcl
		(list
			"// Temporary DCL file;"
			"	MultiSelectListBox : dialog {"
			"	: list_box {"
	(strcat	"		label = \"" msg "\";")
	(strcat	"		multiple_select = " (if mlt "true" "false") ";")
			"		height = 18;"
			"		width = 24;"
			"		key = \"items\";"
			"	}"
			"	spacer;"
			"	ok_cancel;"
			"}"
		)
	)
	(if
		(and
			dcl
			(setq tmp (vl-filename-mktemp "LBOX" nil ".dcl"))
			(setq des (open tmp "w"))
			(foreach line dcl (write-line line des))
			(not (close des))
			(> (setq dch (load_dialog tmp)) 0)
			(new_dialog "MultiSelectListBox" dch)
		)
		(progn
			(start_list "items")
			(foreach itm lst (add_list itm))
			(end_list)
			(setq rtn (set_tile "items" "0"))
			(action_tile "items" "(setq rtn $value)")
			(setq rtn
				(if (= 1 (start_dialog))
					(mapcar '(lambda ( x ) (nth x lst)) (read (strcat "(" rtn ")")))
				)
			)
		)
		(prompt "\nError loading dialog box.")
	)
	(if (< 0 dch)
		(unload_dialog dch)
	)
	(if (and tmp (setq tmp (findfile tmp)))
		(vl-file-delete tmp)
	)
	rtn
)
;; CREATES A DCL POP-UP BOX OF SELECTABLE ITEMS AND RETURNS USER SELECTED ITEMS AS A LIST
;; [ msg ]	== STRING VALUE FOR THE LIST BOX TITLE / DESCRIPTION
;; [ lst ]	== LIST OF ITEMS TO ADD TO THE LIST BOX DCL
;; [ mlt ]	== BOOLEAN VALUE, T TO ALLOW MULTIPLE SELECTIONS; NIL TO ALLOW ONLY A SINGLE SELECTION


(defun DynamicToggleBox ( msg lst / dcl tmp des dch rtn key keys rtn )
	(setq dcl
		(list
			"// Temporary DCL file;"
			"	DynamicToggleBox : dialog {"
	(strcat "	label = \"" msg "\";")
			"	: column {"
			"		alignment = left;"
			"		children_alignment = left;"
		)
	)
	;; DCL HEADER
	
    (foreach itm lst
		(setq
			key (strcat "toggle_" (vl-string-subst "_" " " itm))
			dcl (append dcl
					(list
						"		: toggle {"
				(strcat "			key = \"" key "\";")
				(strcat "			label = \"" itm "\";")
						"			value = \"0\";"
						"		}"
					)
				)
			keys (append keys (list key))
		)
	)
	;; DCL BODY (APPEND ITEMS AS TOGGLES)
	
	(setq dcl
		(append dcl
			(list
				"	}"
				"	spacer;"
				"	ok_cancel;"
				"}"
			)
		)
	)
	;; DCL FOOTER
	
	(if
		(and
			dcl
			(setq tmp (vl-filename-mktemp "DTGL" nil ".dcl"))
			(setq des (open tmp "w"))
			(foreach line dcl (write-line line des))
			(not (close des))
			(> (setq dch (load_dialog tmp)) 0)
			(new_dialog "DynamicToggleBox" dch)
		)
		(progn
			(action_tile "accept"
				(strcat
					"(progn "
					"	(setq rtn"
					"		(list "
								(apply 'strcat (mapcar '(lambda ( k ) (strcat " (get_tile \"" k "\")")) keys))
					"		)"
					"	)"
					"	(done_dialog 1)"
					")"
				)
			)
			(action_tile "cancel" "(done_dialog 0)")
			(if (= 1 (start_dialog))
				(setq rtn (vl-remove-if-not '(lambda ( x ) (= "1" (cdr x))) (mapcar 'cons lst rtn)))
			)
		)
        (prompt "\nError loading dialog box.")
    )
    (if (> dch 0)
		(unload_dialog dch)
	)
    (if (and tmp (setq tmp (findfile tmp)))
		(vl-file-delete tmp)
	)
    (mapcar 'car rtn)
)
;; CREATES A DCL POP-UP BOX OF SELECTABLE TOGGLE ITEMS AND RETURNS USER SELECTED ITEMS AS A LIST
;; [ msg ]	== STRING VALUE FOR THE TOGGLE BOX TITLE / DESCRIPTION
;; [ lst ]	== LIST OF ITEMS TO ADD TO THE TOGGLE BOX DCL



;;; OBJECTS ;;;

(defun ReturnCollectionItem ( coll key / rtn )
		(setq rtn
			(vl-catch-all-apply
				'vlax-get-property
				(list coll 'Item key)
			)
		)
	(if (vl-catch-all-error-p rtn) nil rtn)
)
;; RETRIEVES AN OBJECT FROM A COLLECTION
;; IF KEY VALUE DOES NOT EXIST IN COLLECTION OBJECT, FUNCTION RETURNS NIL
;; ELSE FUNCTION RETURNS THE OBJECT ASSOCIATED WITH THE PASSED KEY IN THE OBJECT COLLECTION
;; [ key ]	== STRING DENOTING A POSSIBLE KEY VALUE
;; [ coll ]	== COLLECTION OBJECT FROM COM (FROM ACTIVE X: BLOCKS, LAYOUTS, LAYERS, ETC. ; FROM EXCEL: WORKSHEETS, CELLS, ETC.)


(defun ObjectRelease ( objs )
	(mapcar
		'(lambda ( obj )
			(if (and (eq 'VLA-OBJECT (type obj)) (not (vlax-object-released-p obj)))
				(vlax-release-object obj)
			)
		)
		objs
	)
)
;; RELEASES PASSED COM OBJECTS WITHIN A LIST
;; [ objs ]	== LIST OF COM OBJECTS TO RELEASE FROM MEMORY




