;; THIS FILE CONTAINS SUB-FUNCTIONS THAT ARE GLOBALLY AVAILABLE TO ALL OTHER LISP FUNCTIONS

;;; GLOBAL SUBFUNCTIONS ;;;



;;; MATH FUNCTIONS ;;;

(defun std:DegreesToRadians ( n )
	(* pi (/ n 180.0))
)
;; CONVERTS A NUMERIC VALUE REPRESENTING DEGREES TO RADIANS
;; [ n ]	== NUMERIC VALUE REPRESENTING DEGREES



;;; DYNAMIC DCL BOXES ;;;

(defun std:MultiSelectListBox ( msg lst mlt / dcl tmp des dch rtn )
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


(defun std:DynamicToggleBox ( msg lst flg / dcl tmp des dch rtn key keys rtn )
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
	
	(setq flg (if flg "1" "0"))
    (foreach itm lst
		(setq
			key (strcat "toggle_" (vl-string-subst "_" " " itm))
			dcl (append dcl
					(list
						"		: toggle {"
				(strcat "			key = \"" key "\";")
				(strcat "			label = \"" itm "\";")
				(strcat	"			value = \"" flg "\";")
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
;; [ flg ]	== IF T, INITIALIZES ALL TOGGLES TO BE CHECKED; IF NIL, TOGGLES ARE INITIALIZED TO UN-CHECKED



;;; OBJECTS ;;;

;; BLOCK OBJECTS MANIPULATION ;;
(defun std:GetNestedBlockFromParent ( blk bnm / dfn ret )
	(setq dfn (vl-catch-all-apply 'vla-item (list (vla-get-blocks (vla-get-activedocument (vlax-get-acad-object))) (vla-get-effectivename blk))))
	(if (not (vl-catch-all-error-p dfn))
		(vlax-for obj dfn
			(if (and
					(= (vla-get-objectname obj) "AcDbBlockReference")
					(= (strcase (vla-get-effectivename obj)) (strcase bnm))
				)
				(setq ret obj)
			)
		)
	)
	ret
)
;; RETURNS A NESTED BLOCK WITHIN A PASSED BLOCK OBJECT BY USING THE BLOCK NAME OF THE BLOCK WITHIN THE BLOCK OBJECT
;; RETURNS NIL IF THE BLOCK NAME IS NOT FOUND WITHIN THE PASSED BLOCK OBJECTS DEFINITION
;; [ blk ]	== BLOCK OBJECT TO SEARCH BLOCK DEFINITION
;; [ bnm ]	== BLOCK NAME STRING OF NESTED BLOCK TO SEARCH FOR


(defun std:GetBlockVisibilityList ( blkDef / dct vis )
  ;; blkDef is the vla-object from the DBX 'Blocks' collection
  (if (and (= (vla-get-hasextensiondictionary blkDef) :vlax-true)
           (setq dct (vla-getextensiondictionary blkDef))
           ;; Search for the enhanced block data in the dictionary
           (setq vis (vl-some 
                       '(lambda (pair) 
                          (if (and (= (car pair) 360)
                                   (= (cdr (assoc 0 (entget (cdr pair)))) "BLOCKVISIBILITYPARAMETER"))
                              (cdr pair)))
                       (dictsearch (vlax-vla-object->ename dct) "ACAD_ENHANCEDBLOCK")))
      )
      ;; DXF 303 stores the visibility state names
      (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= (car x) 303)) (entget vis)))
  )
)
;; RETURNS A LIST OF BLOCK VISIBILITIES NAMES WITHIN A PASSED BLOCK OBJECT
;; [ blk ]	== BLOCK OBJECT


(defun std:SetBlockVisibility ( blk new / vis )
	(if (= (vla-get-isdynamicblock blk) :vlax-true)
		(foreach prp (vlax-invoke blk 'getdynamicblockproperties)
			(if (setq vis
					(vl-some 
						'(lambda ( x ) (if (= (strcase x) (strcase new)) x)) 
						(vlax-get prp 'allowedvalues)
					)
				)
				(vla-put-value prp (vlax-make-variant vis vlax-vbString))
			)
		)
	)
)
;; SET VISIBILITY OF PASSED BLOCK OBJECT TO VALUE STATE OF NEW STATE NAME
;; IF STATE NAME IS NOT FOUND IN BLOCK OBJECT, FUNCTION DOES NOT EFFECT THE BLOCK OBJECT
;; FUNCTION ALWAYS RETURNS NIL
;; FUNCTION IS NOT CASE SENSITIVE
;; [ blk ]	== BLOCK OBJECT
;; [ new ]	== STRING NAME TO SET BLOCK OBJECT VISIBILITY STATE TO IF IT EXISTS


(defun std:InsertBlockReference ( bnm ipt rot / ms arr )
	(setq ms (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object))))
	(setq arr (vlax-make-safearray vlax-vbDouble '(0 . 2))) ;; CREATE A 3 ELEMENT ARRAY TO HOLD THE INSERTION POINT
	(setq pt (vlax-safearray-fill arr ipt)) ;; FILL THE ARRAY WITH THE INSERTION POINT XYZ
	(vla-InsertBlock ms pt bnm 1.0 1.0 1.0 rot) ;; RETURNS THE BLOCK REFERENCE
)
;; INSERTS A BLOCK INTO MODELSPACE AT A SPECIFIED INSERTION POINT AND RETURNS THE REFENCE TO THE INSERTED BLOCK OBJECT
;; [ bnm ]	== BLOCK STRING NAME TO INSERT
;; [ ipt ]	== BLOCK INSERTION POINT AS A 3 POINT LIST
;; [ rot ]	== BLOCK ROTATION VALUE


;; STANDARD OBJECTS FUNCTIONS ;;
(defun std:ReturnCollectionItem ( coll key / rtn )
		(setq rtn
			(vl-catch-all-apply
				'vlax-invoke-method
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


(defun std:CreateObjectDBX ( / dbx vers )
	(setq dbx
		(vl-catch-all-apply
			'vla-getinterfaceobject
			(list
				(vlax-get-acad-object)
				(if (< (setq vers (atoi (getvar "ACADVER"))) 16)
					"ObjectDBX.AxDbDocument"
					(strcat "ObjectDBX.AxDbDocument." (itoa vers))
				)
			)
		)
	)
	(if (or (null dbx) (vl-catch-all-error-p dbx))
		(princ "\nUnable to interface with ObjectDBX.")
	)
  	dbx
)
;; CREATES A DBX OBJECT (DATABASE EXTENSION OBJECT)
;; IF SUCCESSFUL RETURNS THE DBX OBJECT THAT WAS CREATED, ELSE NIL
;; USED FOR INTERACTING WITH DRAWINGS THAT ARE UN-OPENED, BATCH PROCESSING AND HIGHER PERFORMANCE


(defun std:ObjectRelease ( objs )
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



;;; STRING MANUPULATION ;;;

(defun std:LM:UnFormat ( str mtx / _replace rx )

    (defun _replace ( new old str )
        (vlax-put-property rx 'pattern old)
        (vlax-invoke rx 'replace str new)
    )
    (if (setq rx (vlax-get-or-create-object "VBScript.RegExp"))
        (progn
            (setq str
                (vl-catch-all-apply
                    (function
                        (lambda ( )
                            (vlax-put-property rx 'global     actrue)
                            (vlax-put-property rx 'multiline  actrue)
                            (vlax-put-property rx 'ignorecase acfalse) 
                            (foreach pair
                               '(
                                    ("\032"    . "\\\\\\\\")
                                    (" "       . "\\\\P|\\n|\\t")
                                    ("$1"      . "\\\\(\\\\[ACcFfHLlOopQTW])|\\\\[ACcFfHLlOopQTW][^\\\\;]*;|\\\\[ACcFfHLlOopQTW]")
                                    ("$1$2/$3" . "([^\\\\])\\\\S([^;]*)[/#\\^]([^;]*);")
                                    ("$1$2"    . "\\\\(\\\\S)|[\\\\](})|}")
                                    ("$1"      . "[\\\\]({)|{")
                                )
                                (setq str (_replace (car pair) (cdr pair) str))
                            )
                            (if mtx
                                (_replace "\\\\" "\032" (_replace "\\$1$2$3" "(\\\\[ACcFfHLlOoPpQSTW])|({)|(})" str))
                                (_replace "\\"   "\032" str)
                            )
                        )
                    )
                )
            )
            (vlax-release-object rx)
            (if (null (vl-catch-all-error-p str))
                str
            )
        )
    )
)
;; UNFORMATS A STRING USING REGULAR EXPRESSION MANIPULATION TECHNIQUES AND RETURNS THE UNFORMATTED STRING VALUE
;; [ str ]	== STRING TO UNFORMAT
;; [ mtx ]	== BOOLEAN VALUE, IF SET; FORMATS THE STRING FOR MULTILINE TEXT


(defun std:ConvertToString ( val / typ )
	(setq typ (type val))
	(cond
		((= typ 'STR)
			val
		)
		((= typ 'REAL)
			(rtos val 2 (- (strlen (vl-string-right-trim "0" (rtos (abs (- val (fix val))) 2 8))) 2))
		)
		((= typ 'INT)
			(itoa val)
		)
	)
)
;; USED TO CHECK IF A VALUE IS A STRING OR NUMERIC; RETURNS THE PASSED VALUE AS A STRING IF IT IS NUMERIC
;; [ val ]	== NUMERIC OR STRING VALUE TO CONVERT / CHECK 



;;; LIST FUNCTIONS ;;;

(defun std:GetLayoutNameList ( / res )
  	(vlax-for lyt (vla-get-layouts (vla-get-activedocument (vlax-get-acad-object)))
		(setq res (cons (cons (vla-get-taborder lyt) (vla-get-name lyt)) res))
	)
  	(cdr (mapcar 'cdr (vl-sort res '(lambda ( a b ) (< (car a) (car b))))))
)
;; RETURNS A LIST OF THE CURRENT DRAWINGS LAYOUT NAMES IN ORDER (EXCLUDES THE MODEL TAB AUTOMATICALLY)


(defun std:Variant->List ( vrnt )
	(if (= (type vrnt) 'VARIANT)
		(vlax-safearray->list
			(vlax-variant-value vrnt)
		)
	)
)
;; RETURNS A LIST CANTAINED WITHIN A VARIANT OBJECT
;; [ vrnt ]	== VARIANT TO CONVERT INTO LIST



;;; MISC. FUNCTIONS / GLOBALS ;;;

(defun glb:StatesList ( )
	(list
		(cons "Alabama" "AL")
		(cons "Alaska" "AK")
		(cons "Arizona" "AZ")
		(cons "Arkansas" "AR")
		(cons "California" "CA")
		(cons "Colorado" "CO")
		(cons "Connecticut" "CT")
		(cons "Delaware" "DE")
		;(cons "DC" "DC")	;-------; UPDATE POSSIBLY?
		(cons "Florida" "FL")
		(cons "Georgia" "GA")
		(cons "Hawaii" "HI")
		(cons "Idaho" "ID")
		(cons "Illinois" "IL")
		(cons "Indiana" "IN")
		(cons "Iowa" "IA")
		(cons "Kansas" "KS")
		(cons "Kentucky" "KY")
		(cons "Louisiana" "LA")
		(cons "Maine" "ME")
		(cons "Maryland" "MD")
		(cons "Massachusetts" "MA")
		(cons "Michigan" "MI")
		(cons "Minnesota" "MN")
		(cons "Mississippi" "MS")
		(cons "Missouri" "MO")
		(cons "Montana" "MT")
		(cons "Nebraska" "NE")
		(cons "Nevada" "NV")
		(cons "New Hampshire" "NH")
		(cons "New Jersey" "NJ")
		(cons "New Mexico" "NM")
		(cons "New York" "NY")
		(cons "North Carolina" "NC")
		(cons "North Dakota" "ND")
		(cons "Ohio" "OH")
		(cons "Oklahoma" "OK")
		(cons "Oregon" "OR")
		(cons "Pennsylvania" "PA")
		(cons "Rhode Island" "RI")
		(cons "South Carolina" "SC")
		(cons "South Dakota" "SD")
		(cons "Tennessee" "TN")
		(cons "Texas" "TX")
		(cons "Utah" "UT")
		(cons "Vermont" "VT")
		(cons "Virginia" "VA")
		(cons "Washington" "WA")
		(cons "West Virginia" "WV")
		(cons "Wisconsin" "WI")
		(cons "Wyoming" "WY")
	)
)


