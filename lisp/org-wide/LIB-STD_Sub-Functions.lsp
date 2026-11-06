;; THIS FILE CONTAINS SUB-FUNCTIONS THAT ARE GLOBALLY AVAILABLE TO ALL OTHER LISP FUNCTIONS

;;; GLOBAL SUBFUNCTIONS ;;;


;;; TIME LOGGING ;;;

(defun std:Runtime ( st / )
	(if (not st)
		(getvar "MILLISECS")
		(- (getvar "MILLISECS") st)
	)
)
;; CREATES AN ELAPSED TIME SPEC OF A RUNNING FUNCTION
;; THIS COMMAND WILL BE USED ON ALL CALLABLE FUNCTIONS TO MEASURE RUNTIMES
;; USE:
; (defun c:TEST_FUNCTION ( / st lap )
	; (setq st (std:Runtime nil))
	; (TEST_FUNCTION) 										;; Main function call
	; (setq lap (std:Runtime st))
	; (LOG_TO_METRICS "info" "test message" lap nil)
	; (FLUSH_LOGS)
; )
;;; THIS IS THE PROPOSED SIGNATURE FORMATTING FOR ALL CALLABLE FUNCTIONS ;;;
;; [ st ]	== START TIME VALUE (FLOAT): IF NOT SET, RETURNS CURRENT TIME "MILLISECS", ELSE RETURNS DIFFERENCE FROM CURRENT TIME AND PASSED TIME ARGUMENT



;;; MATH FUNCTIONS ;;;

(defun std:DegreesToRadians ( n )
	(* pi (/ n 180.0))
)
;; CONVERTS A NUMERIC VALUE REPRESENTING DEGREES TO RADIANS
;; [ n ]	== NUMERIC VALUE REPRESENTING DEGREES


(defun std:SlopeToDegrees ( pct / )
	(* 180.0 (/ (atan (/ pct 100.0)) pi))
)
;; CONVERTS A PERCENT SLOPE TO DEGREES (EX. 8.0 -> 4.574)
;; [ pct ]	== SLOPE IN PERCENT



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
			"		width = 32;"
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


(defun std:DynamicToggleBox ( msg lst flg / dcl tmp des dch rtn key keys val )
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
			;; EVALUATE PRE-CHECKED STATE FOR THIS INDIVIDUAL ITEM:
			;; 1. If flg is a list -> check if itm exists in flg (sets individual value)
			;; 2. If flg is boolean -> use "1" for T, "0" for nil (sets all values)
			val (if (listp flg)
					(if (member itm flg) "1" "0")
					(if flg "1" "0")
				)
			key (strcat "toggle_" (vl-string-subst "_" " " itm))
			dcl (append dcl
					(list
						"		: toggle {"
				(strcat "			key = \"" key "\";")
				(strcat "			label = \"" itm "\";")
				(strcat	"			value = \"" val "\";")
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
;; [ lst ]	== LIST OF STRINGS TO DISPLAY AS TOGGLE ITEMS
;; [ flg ]	== BOOLEAN OR LIST: IF T, ALL ITEMS CHECKED; IF NIL, ALL UNCHECKED; IF A LIST, ONLY MATCHING ITEMS IN 'lst' ARE CHECKED



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


(defun std:GetBlockVisibilityList ( def / dct vis )
	;; def IS THE VLA-OBJECT FROM THE DBX 'BLOCKS' COLLECTION
	(if (and
			(= (vla-get-hasextensiondictionary def) :vlax-true)
			(setq dct (vla-getextensiondictionary def)) ;; SEARCH FOR THE ENHANCED BLOCK DATA IN THE DICTIONARY
			(setq vis
				(vl-some 
					'(lambda ( pr ) 
						(if (and
								(= (car pr) 360)
								(= (cdr (assoc 0 (entget (cdr pr)))) "BLOCKVISIBILITYPARAMETER")
							)
							(cdr pr)
						)
					)
					(dictsearch (vlax-vla-object->ename dct) "ACAD_ENHANCEDBLOCK")
				)
			)
		)
		(mapcar 'cdr (vl-remove-if-not '(lambda (x) (= (car x) 303)) (entget vis))) ;; DXF 303 STORES THE VISIBILITY STATE NAMES
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


(defun std:InsertBlockReference ( bnm ipt rot / ms arr pt )
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
(defun std:FunctionLoaded ( sym / )
	(and (boundp sym) (eval sym) t)
)
;; RETURNS T IF THE FUNCTION SYMBOL IS DEFINED (EX. AN OPTIONAL LIBRARY IS LOADED), ELSE NIL
;; USE: (if (std:FunctionLoaded 'geo:ReadGeoData) ...)


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


(defun std:CopyDBXObjects ( dbx bit lyrs scl crds / objs tmat lst out res )
	(if (or (null scl) (<= scl 0)) (setq scl 1))
	(setq tmat
		(vlax-tmatrix
			(list
				(list scl 0 0 (car crds))
				(list 0 scl 0 (cadr crds))
				(list 0 0 scl (last crds))
				(list 0 0 0 1)
			)
		)
	)
	(if (and bit (/= bit 0))
		(progn
			(if (= (logand 1 bit) 1) (setq objs (append (list "AcDbLine" "AcDbPolyline" "AcDb2dPolyline" "AcDb3dPolyline" "AcDbCircle" "AcDbArc" "AcDbEllipse" "AcDbSpline" "AcDbHatch") objs)))
			(if (= (logand 2 bit) 2) (setq objs (append (list "AcDbText" "AcDbMText" "AcDbRotatedDimension" "AcDbAlignedDimension" "AcDbArcDimension" "AcDbRadialDimension" "AcDbLeader" "AcDbMLeader") objs)))
			(if (= (logand 4 bit) 4) (setq objs (append (list "AcDbBlockReference") objs)))
			(if (= (logand 8 bit) 8) (setq objs (append (list "AcDbPoint" "AcDbRay" "AcDbXline") objs)))
		)
	)
	(vlax-for itm (vla-get-modelspace dbx)
		(if (and
				(or (null bit) (= bit 0) (member (vla-get-objectname itm) objs))
				(or (null lyrs) (wcmatch (strcase (vla-get-layer itm)) (strcase lyrs)))
			)
			(setq lst (cons itm lst))
		)
	)
	(if (and lst (setq out (vlax-invoke dbx 'CopyObjects lst (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object))))))
		(foreach itm out
			(setq res (vl-catch-all-apply 'vla-transformby (list itm tmat)))
			(if (vl-catch-all-error-p res)
				(princ (strcat "\nSkipped untransformable object: " (vla-get-objectname itm)))
			)
		)
	)
)
;; COPIES LINEWORK FROM AN EXTERNAL DRAWING BASED ON OBJECT TYPE AND/OR LAYER TO SPECIFIED COORDINATES WITHIN THE CURRENT DRAWING
;; [ dbx ]	== EXTERNAL DATABASE EXTENSION OBJECT
;; [ bit ]	== INTEGER BIT SET FOR VALID TARGET OBJECTS 0 -> 15, 0 INCLUDES ALL OBJECTS, 1 = GEOMETRY (LINES, POLYLINES INCLUDING 2D / 3D, CURVES, HATCHES),
;;			2 = ANNOTATIONS (TEXT, MTEXT, DIMENSIONS, LEADERS), 4 = BLOCKS, 8 = CONSTRUCTION LINES / POINTS
;;			TEXT / MTEXT AND 2D / 3D POLYLINES WERE ADDED 10-2026: BEFORE THAT, TEXT WAS NEVER COPIED AND "AcDbPolyline2d" (NOT A REAL OBJECT NAME) MISSED 2D POLYLINES
;; [ lyrs ]	== PATTERN STRING FOR MATCHING LAYERS IE; "Layer*,0,*New*" | INCLUDES LAYER "0", ANY LAYER THAT STARTS WITH "Layer" AND ANY LAYER THAT CONTAINS "New"
;; [ scl ]	== INTEGER VALUE FOR SCALE FOR TARGET OBJECTS INTO CURRENT DRAWING, NIL IF SCALE NOT TO BE APPLIED
;; [ crds ]	== LIST OF COORDINATES FOR OBJECTS TO BE COPIED TO IN THE CURRENT DRAWING
;; EXAMPLE USAGE: (setq dbx (std:CreateObjectDBX dir)) (vla-open dbx (strcat dir "\\" fn) :vlax-true) (std:CopyDBXObjects dbx 7 nil nil '(0 0 0))
;; NOTE: THIS FUNCTION DOES NOT PRE-CHECK IF DBX HAS BEEN SET BEFORE INVOKING COPYOBJECTS; ERROR HANDLING MUST BE DONE BEFORE THIS FUNCTION IS USED


(defun std:GetCustomPropertyValue ( key / dwp val )
	(setq dwp (vla-get-summaryinfo (vla-get-activedocument (vlax-get-acad-object))))
	(if (vl-catch-all-error-p (vl-catch-all-apply 'vla-getcustombykey (list dwp key 'val)))
		nil
		val
	)
)
;; RETURNS CUSTOM DRAWING PROPERTY VALUE OF 'key' IF KEY EXISTS, ELSE RETURNS NIL IF KEY IS NOT FOUND
;; [ key ] == STRING VALUE FOR KEY TO SEARCH CUSTOM PROPERTIES FOR


(defun std:SetCustomPropertyValue ( key val / dwp )
	(setq dwp (vla-get-summaryinfo (vla-get-activedocument (vlax-get-acad-object))))
	(not (vl-catch-all-error-p (vl-catch-all-apply 'vla-setcustombykey (list dwp key val))))
)
;; RETURNS CUSTOM DRAWING PROPERTY VALUE OF 'key' IF KEY EXISTS, ELSE RETURNS NIL IF KEY IS NOT FOUND
;; [ key ] == STRING VALUE FOR KEY TO SEARCH CUSTOM PROPERTIES FOR
;; [ val ] == STRING VALUE TO UPDATE KEY


(defun std:AddCustomProperty ( key val / dwp )
	(setq dwp (vla-get-summaryinfo (vla-get-activedocument (vlax-get-acad-object))))
	(not (vl-catch-all-error-p (vl-catch-all-apply 'vla-addcustominfo (list dwp key (if (null val) "" val)))))
)
;; CREATES NEW CUSTOM DRAWING PROPERTY
;; [ key ] == STRING VALUE FOR NEW KEY
;; [ val ] == STRING VALUE FOR NEW KEYS VALUE, DEFAULTS TO EMPTY STRING IF NIL IS PASSED AS VALUE



(defun std:CollectNewObjects ( ent / lst )
	(setq ent (if ent (entnext ent) (entnext)))
	(while ent
		(if (not (member (cdr (assoc 0 (entget ent))) (list "VERTEX" "SEQEND" "ATTRIB"))) ;; SKIP SUB-ENTITIES OF COMPLEX OBJECTS
			(setq lst (cons (vlax-ename->vla-object ent) lst))
		)
		(setq ent (entnext ent))
	)
	(reverse lst)
)
;; RETURNS A LIST OF VLA-OBJECTS ADDED TO THE DRAWING AFTER THE PASSED ENTITY
;; [ ent ]	== RESULT OF (entlast) TAKEN BEFORE THE OBJECTS WERE ADDED, NIL MEANS THE DRAWING WAS EMPTY AND ALL OBJECTS ARE RETURNED


(defun std:PolylineElevation ( obj / nam )
	(setq nam (vla-get-objectname obj))
	(cond
		((member nam (list "AcDbPolyline" "AcDb2dPolyline"))
			(vla-get-elevation obj)
		)
		((= nam "AcDb3dPolyline")
			(caddr (vlax-curve-getstartpoint obj))
		)
	)
)
;; RETURNS THE ELEVATION OF A POLYLINE (EX. A CONTOUR), A 3D POLYLINE RETURNS THE Z OF ITS START POINT
;; RETURNS NIL IF THE OBJECT IS NOT A POLYLINE
;; [ obj ]	== VLA-OBJECT


(defun std:PolylinePoints ( obj / nam crd elv idx end out )
	(setq nam (vla-get-objectname obj))
	(cond
		((= nam "AcDbPolyline")
			(setq
				crd (vlax-get obj 'Coordinates) ;; FLAT ( X Y X Y ... ) LIST, ONE CALL FOR THE WHOLE POLYLINE
				elv (vla-get-elevation obj)
			)
			(while crd
				(setq
					out (cons (list (car crd) (cadr crd) elv) out)
					crd (cddr crd)
				)
			)
		)
		((member nam (list "AcDb2dPolyline" "AcDb3dPolyline"))
			(setq
				idx 0
				end (fix (+ 0.5 (vlax-curve-getendparam obj)))
			)
			(while (<= idx end)
				(setq
					out (cons (vlax-curve-getpointatparam obj idx) out)
					idx (1+ idx)
				)
			) ;; ONE POINT PER VERTEX, A CLOSED POLYLINE'S END PARAMETER ALREADY RETURNS TO THE FIRST VERTEX
		)
	)
	(setq out (vl-remove nil (reverse out)))
	(if (and out (= nam "AcDbPolyline") (= :vlax-true (vla-get-closed obj)))
		(append out (list (car out)))
		out
	)
)
;; RETURNS THE VERTICES OF A LIGHTWEIGHT, 2D, OR 3D POLYLINE AS ( X Y Z ) POINTS IN ORDER, A CLOSED POLYLINE ENDS WITH ITS FIRST POINT AGAIN
;; ARC SEGMENTS (BULGES) ARE TAKEN AS STRAIGHT CHORDS, LIGHTWEIGHT POLYLINES ARE ASSUMED TO LIE IN THE WCS (NORMAL 0,0,1, AS CONTOURS DO)
;; RETURNS NIL IF THE OBJECT IS NOT A POLYLINE
;; [ obj ]	== VLA-OBJECT


(defun std:ObjectsExtents ( lst / pmn pmx mn mx )
	(foreach obj lst
		(if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-getboundingbox (list obj 'pmn 'pmx))))
			(setq
				pmn (vlax-safearray->list pmn)
				pmx (vlax-safearray->list pmx)
				mn (if mn (mapcar 'min mn pmn) pmn)
				mx (if mx (mapcar 'max mx pmx) pmx)
			)
		)
	)
	(if mn (list mn mx))
)
;; RETURNS THE COMBINED BOUNDING BOX ( MINPOINT MAXPOINT ) OF THE PASSED OBJECTS, NIL IF NONE HAVE A BOUNDING BOX
;; [ lst ]	== LIST OF VLA-OBJECTS


(defun std:TranslateObject ( obj dx dy dz / )
	(vla-transformby obj
		(vlax-tmatrix
			(list
				(list 1.0 0.0 0.0 (float dx))
				(list 0.0 1.0 0.0 (float dy))
				(list 0.0 0.0 1.0 (float dz))
				(list 0.0 0.0 0.0 1.0)
			)
		)
	)
)
;; MOVES AN OBJECT BY ( DX DY DZ ) IN WORLD COORDINATES
;; [ obj ]	== VLA-OBJECT, [ dx dy dz ] == DISTANCES


(defun std:RotateAboutX ( obj ang yc zc / c s )
	(setq
		c (cos ang)
		s (sin ang)
	)
	(vla-transformby obj
		(vlax-tmatrix
			(list
				(list 1.0 0.0 0.0 0.0)
				(list 0.0 c (- s) (+ (- yc (* c yc)) (* s zc)))
				(list 0.0 s c (- zc (* s yc) (* c zc)))
				(list 0.0 0.0 0.0 1.0)
			)
		)
	)
)
;; ROTATES AN OBJECT ABOUT A WORLD LINE PARALLEL TO THE X AXIS (EAST-WEST) THROUGH Y == [ yc ], Z == [ zc ]
;; POSITIVE [ ang ] (RADIANS) TURNS +Y TOWARD +Z (NORTH END UP), A POINT [ d ] SOUTH OF THE AXIS ENDS UP d x SIN(ang) LOWER AND d x COS(ang) SOUTH
;; [ obj ]	== VLA-OBJECT


(defun std:FlattenObjects ( lst / nam pnt adj skp )
	(setq adj 0 skp 0)
	(foreach obj lst
		(setq nam (vla-get-objectname obj))
		(cond
			((= nam "AcDbBlockReference")
				(setq pnt (vlax-get obj 'InsertionPoint))
				(if (not (equal (caddr pnt) 0.0 1e-8))
					(if (vl-catch-all-error-p (vl-catch-all-apply 'vlax-put (list obj 'InsertionPoint (list (car pnt) (cadr pnt) 0.0))))
						(setq skp (1+ skp)) ;; LIKELY ON A LOCKED LAYER
						(setq adj (1+ adj))
					)
				)
			)
			((= nam "AcDbPolyline")
				(if (not (equal (vla-get-elevation obj) 0.0 1e-8))
					(if (vl-catch-all-error-p (vl-catch-all-apply 'vla-put-elevation (list obj 0.0)))
						(setq skp (1+ skp))
						(setq adj (1+ adj))
					)
				)
			)
		)
	)
	(list adj skp)
)
;; FLATTENS BLOCK REFERENCES (INSERTION POINT) AND LIGHTWEIGHT POLYLINES (ELEVATION) TO Z = 0, OTHER OBJECTS ARE LEFT ALONE
;; RETURNS ( ADJUSTED SKIPPED ) OBJECT COUNTS, OBJECTS THAT COULD NOT BE CHANGED (EX. LOCKED LAYERS) ARE COUNTED AS SKIPPED
;; [ lst ]	== LIST OF VLA-OBJECTS



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
		( t
			""
		)
	)
)
;; USED TO CHECK IF A VALUE IS A STRING OR NUMERIC; RETURNS THE PASSED VALUE AS A STRING IF IT IS NUMERIC
;; [ val ]	== NUMERIC OR STRING VALUE TO CONVERT / CHECK 


(defun std:CurrentDateTime ( fmt / dt mo dy yr hr mn tim ap bas mos )
	(setq dt (rtos (getvar "CDATE") 2 4)) ;; INCLUDE TIME UP TO MINUTES
	(setq
		mo (substr dt 5 2)
		dy (substr dt 7 2)
		yr (substr dt 1 4)
		hr (substr dt 10 2)
		mn (substr dt 12 2)
	)
	;; TIME PADDING IF REQUIRED; 12:00 AM
	(if (= (strlen hr) 0) (setq hr "00"))
	(if (= (strlen hr) 1) (setq hr (strcat "0" hr)))
	(if (= (strlen mn) 0) (setq mn "00"))
	(if (= (strlen mn) 1) (setq mn (strcat "0" mn)))
	(setq tim
		(cond
			((vl-string-search "HH:MM" fmt) ;; 24 HOUR CYCLE
				(setq fmt (vl-string-subst "" "HH:MM" fmt))
				(strcat hr ":" mn)
			)
			((vl-string-search "hh:mm" fmt) ;; 12 HOUR CYCLE
				(setq fmt (vl-string-subst "" "hh:mm" fmt))
				(setq ap (if (< (atoi hr) 12) "AM" "PM"))
				(cond
					((= (atoi hr) 0) (setq hr "12"))
					((> (atoi hr) 12) (setq hr (itoa (- (atoi hr) 12))))
				)
				(if (= (strlen hr) 1) (setq hr (strcat "0" hr)))
				(strcat hr ":" mn " " ap)
			)
			( t
				"" ;; NO TIME STAMP INCLUDED
			)
		)
	)
	(setq fmt (strcase (vl-string-right-trim " " fmt)))
	(setq bas
		(cond
			((= fmt "MM/DD/YYYY")
				(strcat mo "/" dy "/" yr)
			)
			((= fmt "YYYY/MM/DD")
				(strcat yr "/" mo "/" dy)
			)
			((= fmt "MM-DD-YYYY")
				(strcat mo "-" dy "-" yr)
			)
			((= fmt "YYYY-MM-DD")
				(strcat yr "-" mo "-" dy)
			)
			((= fmt "MM/DD/YY")
				(strcat mo "/" dy "/" (substr yr 3 2))
			)
			((= fmt "MM-DD-YY")
				(strcat mo "-" dy "-" (substr yr 3 2))
			)
			((= fmt "YYYYMMDD")
				(strcat yr mo dy)
			)
			((= fmt "YYMMDD")
				(strcat (substr yr 3 2) mo dy)
			)
			((= fmt "MMDDYY")
				(strcat mo dy (substr yr 3 2))
			)
			((= fmt "DDMMMYYYY")
				(setq mos (list "JAN" "FEB" "MAR" "APR" "MAY" "JUN" "JUL" "AUG" "SEP" "OCT" "NOV" "DEC"))
				(strcat dy (nth (1- (atoi mo)) mos) yr)
			)
			( t
				"" ;; ALLOWS A TIME STAMP ONLY RETURN
			)
		)
	)
	(vl-string-left-trim " " (strcat bas (if (> (strlen tim) 0) (strcat " " tim) "")))
)
;; RETURNS STRING DATE (AND TIME IF INCLUDED) BASED ON PASSED FORMAT, ELSE EMPTY STRING
;; [ fmt ]	== FORMAT STRING THAT MATCHES INNER CODED FORMATS
;; ' fmt ' CAN INCLUDE A TIME STAMP, EITHER "HH:MM" FOR 24 HOUR CLOCK, OR "hh:mm" FOR 12 HOUR CLOCK
;; EXAMPLES: "MM-DD-YYYY" -> "01-15-2026"; "MM-DD-YYYY HH:MM" -> "01-15-2026 16:30"; "MM-DD-YYYY hh:mm" -> "01-15-2026 04:30 PM"


(defun std:NumberString ( num / str )
	(if (numberp num)
		(progn
			(setq str (rtos num 2 6))
			(if (vl-string-search "." str)
				(setq str (vl-string-right-trim "." (vl-string-right-trim "0" str)))
			)
			str
		)
		""
	)
)
;; FORMATS A NUMBER WITHOUT TRAILING ZEROS, UP TO 6 DECIMAL PLACES (EX. 6.80400 -> "6.804", 2.0 -> "2"), "" IF NOT A NUMBER
;; [ num ]	== NUMBER TO FORMAT


(defun std:ParseNumberList ( str / pos lst )
	(setq str (vl-string-translate " ;" ",," str)) ;; ALLOW SPACES OR SEMICOLONS AS SEPARATORS
	(while (setq pos (vl-string-search "," str))
		(setq
			lst (cons (substr str 1 pos) lst)
			str (substr str (+ pos 2))
		)
	)
	(setq lst (vl-remove "" (reverse (cons str lst))))
	(if (and lst (vl-every '(lambda ( s ) (distof s 2)) lst))
		(mapcar '(lambda ( s ) (distof s 2)) lst)
	)
)
;; CONVERTS A SEPARATED STRING OF NUMBERS ("6.0, 6.5, 7.0") INTO A LIST OF REALS
;; RETURNS NIL IF THE STRING IS EMPTY OR ANY VALUE IS NOT A VALID NUMBER
;; [ str ]	== NUMBERS SEPARATED BY COMMAS, SPACES, OR SEMICOLONS


(defun std:ParseLength ( str / num )
	(if (= 'STR (type str))
		(progn
			(setq str (vl-string-trim " \t" str))
			(cond
				((= "" str)
					nil
				)
				((not (or (vl-string-search "'" str) (vl-string-search "\"" str)))
					(distof str 2) ;; NO FOOT OR INCH MARK == DECIMAL FEET (EX. 29.604)
				)
				((setq num (cond ((distof str 4)) ((distof str 3))))
					(/ num 12.0) ;; ARCHITECTURAL / ENGINEERING TEXT IS READ IN INCHES, CONVERTED TO FEET
				)
			)
		)
	)
)
;; RETURNS A LENGTH IN FEET FROM DECIMAL FEET OR FEET-INCHES TEXT, NIL IF IT CANNOT BE READ
;; TEXT WITHOUT A FOOT (') OR INCH (") MARK IS DECIMAL FEET, TEXT WITH A MARK IS READ AS FEET-INCHES
;; EX. "29.604" -> 29.604, "29'-7 1/4\"" -> 29.6042, "29'-7.25\"" -> 29.6042, "7 1/4\"" -> 0.6042
;; [ str ]	== LENGTH TEXT


(defun std:ToNumber ( val / )
	(cond
		((numberp val) (float val))
		((= 'STR (type val)) (distof (vl-string-trim " \t" val) 2))
	)
)
;; RETURNS A NUMBER AS A REAL, OR THE NUMBER IN A TEXT VALUE (DECIMAL, SURROUNDING SPACES IGNORED), NIL FOR ANYTHING ELSE
;; USE FOR VALUES THAT MAY BE A NUMBER OR TEXT: EXCEL CELLS, SETTINGS FILE VALUES, DCL TILES (EX. "6.5" -> 6.5, 6 -> 6.0, "abc" -> NIL)
;; [ val ]	== NUMBER OR STRING


(defun std:ValidFileName ( str / )
	(and
		(= 'STR (type str))
		(/= "" (vl-string-trim " " str))
		(not (vl-some '(lambda ( ch ) (vl-string-search ch str)) (list "\\" "/" ":" "*" "?" "\"" "<" ">" "|")))
	)
)
;; RETURNS T IF THE STRING IS A VALID WINDOWS FILE NAME (NOT BLANK, NO \ / : * ? " < > | CHARACTERS), ELSE NIL
;; [ str ]	== FILE NAME WITHOUT A PATH



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


(defun std:FlattenList2D ( lst )
	(mapcar '(lambda ( p ) (list (car p) (cadr p))) lst)
)
; (defun std:FlattenList2D ( lst )
	; (apply 'append (mapcar '(lambda ( p ) (list (car p) (cadr p))) lst))
; )
;; RETURNS A FLATTENED LIST OF VALUES SPECIFICALLY 2D VALUES (X, Y)
;; [ lst ] == A LIST OF POINT VALUES -> (LIST '(10.0 10.0 0.0) '(10.0 20.0 0.0) '(20.0 30.0 0.0)) RETURN VALUE -> (10.0 10.0 10.0 20.0 20.0 30.0)


(defun std:ReplaceNth ( lst idx itm / pos )
	(setq pos -1)
	(mapcar '(lambda ( x ) (if (= idx (setq pos (1+ pos))) itm x)) lst)
)
;; RETURNS A COPY OF THE LIST WITH THE ITEM AT ZERO-BASED [ idx ] REPLACED BY [ itm ]


(defun std:SafeNth ( lst idx / )
	(if (and (vl-consp lst) (= 'INT (type idx)) (<= 0 idx) (< idx (length lst)))
		(nth idx lst)
	)
)
;; RETURNS THE ITEM AT ZERO-BASED [ idx ], NIL IF [ lst ] IS NOT A LIST, IS EMPTY, OR IS TOO SHORT
;; USE FOR LISTS THAT MAY BE MISSING OR SHORTER THAN EXPECTED (EX. XDATA WRITTEN BY AN OLDER VERSION), (nth) CAN STOP WITH "consp nil"


(defun std:RemoveNth ( lst idx / pos )
	(setq pos -1)
	(vl-remove-if '(lambda ( x ) (= idx (setq pos (1+ pos)))) lst)
)
;; RETURNS A COPY OF THE LIST WITHOUT THE ITEM AT ZERO-BASED [ idx ]


(defun std:InsertNth ( lst idx itm / pos out )
	(setq pos -1)
	(foreach x lst
		(if (= idx (setq pos (1+ pos)))
			(setq out (cons itm out))
		)
		(setq out (cons x out))
	)
	(if (>= idx (length lst))
		(setq out (cons itm out))
	) ;; AN INDEX PAST THE END APPENDS THE ITEM
	(reverse out)
)
;; RETURNS A COPY OF THE LIST WITH [ itm ] INSERTED AT ZERO-BASED [ idx ]


(defun std:CountList ( num / out )
	(repeat num
		(setq
			out (cons (itoa num) out)
			num (1- num)
		)
	)
	out
)
;; RETURNS ("1" "2" ... "num"), USEFUL FOR DCL POPUP LISTS
;; [ num ]	== HIGHEST NUMBER (INTEGER)


(defun std:AssocSet ( lst key val / old )
	(if (setq old (assoc key lst))
		(subst (cons key val) old lst)
		(append lst (list (cons key val)))
	)
)
;; RETURNS A COPY OF AN ASSOCIATION LIST WITH [ key ] SET TO [ val ], A MISSING KEY IS ADDED AT THE END
;; USE: (setq cfg (std:AssocSet cfg "units" "Feet"))


(defun std:AssocIncrement ( lst key / old )
	(if (setq old (assoc key lst))
		(subst (cons key (1+ (cdr old))) old lst)
		(cons (cons key 1) lst)
	)
)
;; RETURNS A COPY OF A ( KEY . COUNT ) LIST WITH [ key ] COUNTED ONE MORE, A NEW KEY STARTS AT 1 AND IS ADDED AT THE FRONT
;; USE: (setq cnt (std:AssocIncrement cnt "SAP"))


(defun std:MatchPoints ( pt1 pt2 tol / i s1 s2 hd cur bst dst out ids mat )
	(setq i -1 s1 (mapcar '(lambda ( p ) (list (car p) (cadr p) (setq i (1+ i)))) pt1))
	(setq i -1 s2 (mapcar '(lambda ( p ) (list (car p) (cadr p) (setq i (1+ i)) p)) pt2))
	(setq
		s1 (vl-sort s1 '(lambda ( a b ) (< (cadr a) (cadr b))))
		s2 (vl-sort s2 '(lambda ( a b ) (< (cadr a) (cadr b))))
		hd s2
	) ;; SWEEP ALONG Y: TRACKERS RUN NORTH-SOUTH, SO A COLUMN OF PILES SHARES ONE X AND AN X WINDOW WOULD HOLD THE WHOLE COLUMN (MCCRACKEN: 7x MORE CHECKS)
	(foreach p s1
		(while (and hd (< (cadr (car hd)) (- (cadr p) tol))) ;; BOTH LISTS RUN SOUTH TO NORTH, SO THE WINDOW ONLY MOVES FORWARD
			(setq hd (cdr hd))
		)
		(setq cur hd bst nil)
		(while (and cur (<= (cadr (car cur)) (+ (cadr p) tol)))
			(if (and
					(<= (abs (- (car (car cur)) (car p))) tol)
					(<= (setq dst (distance (list (car p) (cadr p)) (list (car (car cur)) (cadr (car cur))))) tol)
					(or (null bst) (< dst (car bst)))
				)
				(setq bst (list dst (caddr (car cur)) (cadddr (car cur))))
			)
			(setq cur (cdr cur))
		)
		(if bst
			(setq
				out (cons (cons (caddr p) (cons (car bst) (caddr bst))) out)
				ids (cons (cadr bst) ids)
			)
		)
	)
	(setq
		out (vl-sort out '(lambda ( a b ) (< (car a) (car b))))
		i -1
	)
	(foreach p pt1
		(setq i (1+ i))
		(if (and out (= i (caar out)))
			(setq
				mat (cons (cdar out) mat)
				out (cdr out)
			)
			(setq mat (cons nil mat))
		)
	)
	(setq ids (vl-sort ids '<) i nil dst 0)
	(foreach id ids
		(if (/= id i)
			(setq dst (1+ dst) i id)
		)
	) ;; COUNT OF DISTINCT [ pt2 ] ITEMS THAT WERE MATCHED
	(list (reverse mat) dst)
)
;; MATCHES EACH POINT OF [ pt1 ] TO THE NEAREST POINT OF [ pt2 ] WITHIN [ tol ] (PLAN X / Y ONLY), A SWEEP ALONG Y KEEPS IT FAST FOR LARGE LISTS
;; THE SWEEP RUNS ALONG Y BECAUSE PILE COLUMNS SHARE ONE X, POINTS THAT LINE UP ALONG Y INSTEAD WOULD BE SLOWER (RESULTS ARE THE SAME EITHER WAY)
;; RETURNS ( MATCHES COUNT ): MATCHES IS ONE ITEM PER [ pt1 ] POINT, ( DISTANCE . PT2-ITEM ) OR NIL WHEN NOTHING IS WITHIN [ tol ],
;; COUNT IS HOW MANY DISTINCT [ pt2 ] ITEMS WERE MATCHED (SO UNMATCHED [ pt2 ] ITEMS == (length pt2) - COUNT)
;; [ pt1 ]	== LIST OF ITEMS STARTING WITH X Y (EX. (X Y ...))
;; [ pt2 ]	== LIST OF ITEMS STARTING WITH X Y, THE WHOLE ITEM IS RETURNED WITH ITS MATCH
;; [ tol ]	== MATCH DISTANCE, KEEP IT WELL UNDER THE SPACING BETWEEN POINTS SO EACH POINT HAS ONE CANDIDATE


(defun std:VerticalCrossings ( lns xs rng / n ua ulo uhi k j x x1 y1 x2 y2 lo hi y prv out )
	(setq n (length xs))
	(if (and lns (< 0 n))
		(progn
			(setq
				ua (vlax-make-safearray vlax-vbDouble (cons 0 (1- n)))
				ulo (vlax-make-safearray vlax-vbDouble (cons 0 (1- n)))
				uhi (vlax-make-safearray vlax-vbDouble (cons 0 (1- n)))
			) ;; SAFEARRAYS GIVE DIRECT ACCESS BY INDEX, (nth) WOULD WALK THE LIST FOR EVERY SEGMENT
			(vlax-safearray-fill ua xs)
			(vlax-safearray-fill ulo (mapcar '(lambda ( r ) (if r (car r) -1e99)) (if rng rng (mapcar '(lambda ( x ) nil) xs))))
			(vlax-safearray-fill uhi (mapcar '(lambda ( r ) (if r (cadr r) 1e99)) (if rng rng (mapcar '(lambda ( x ) nil) xs))))
			(foreach pts lns
				(setq k 0 prv nil)
				(foreach pnt pts
					(if (and prv (/= (car prv) (car pnt))) ;; A SEGMENT PARALLEL TO THE LINES NEVER CROSSES ONE
						(progn
							(setq
								x1 (car prv) y1 (cadr prv)
								x2 (car pnt) y2 (cadr pnt)
								lo (min x1 x2)
								hi (max x1 x2)
							)
							(while (and (> k 0) (>= (vlax-safearray-get-element ua (1- k)) lo))
								(setq k (1- k))
							)
							(while (and (< k n) (< (vlax-safearray-get-element ua k) lo))
								(setq k (1+ k))
							) ;; k == FIRST LINE AT OR EAST OF THE SEGMENT START, NEIGHBORING SEGMENTS ONLY MOVE IT A LITTLE
							(setq j k)
							(while (and (< j n) (<= (setq x (vlax-safearray-get-element ua j)) hi))
								(setq y (+ y1 (* (- x x1) (/ (- y2 y1) (- x2 x1)))))
								(if (<= (vlax-safearray-get-element ulo j) y (vlax-safearray-get-element uhi j))
									(setq out (cons (list j y (* 0.5 (+ (caddr prv) (caddr pnt)))) out))
								)
								(setq j (1+ j))
							)
						)
					)
					(setq prv pnt)
				)
			)
			(mapcar 'reverse
				(std:GroupByIndex
					(mapcar '(lambda ( itm ) (cons (car itm) (cdr itm)))
						(vl-sort out '(lambda ( a b ) (if (= (car a) (car b)) (< (cadr a) (cadr b)) (< (car a) (car b)))))
					)
					n
				)
			) ;; std:GroupByIndex BUILDS EACH GROUP BACKWARDS, REVERSE PUTS EVERY LINE'S CROSSINGS BACK IN SOUTH TO NORTH ORDER
		)
	)
)
;; FINDS WHERE POLYLINES (EX. CONTOURS) CROSS THE NORTH-SOUTH LINES X == [ xs ], RETURNS ONE LIST PER LINE OF ( Y Z ) CROSSINGS SORTED SOUTH TO NORTH
;; Z AT A CROSSING IS THE SEGMENT'S AVERAGE Z (A CONTOUR'S ELEVATION), A CROSSING AT A SHARED VERTEX CAN BE LISTED TWICE WITH THE SAME Y AND Z
;; RETURNS NIL IF [ lns ] OR [ xs ] IS EMPTY, A LINE NO POLYLINE CROSSES GETS NIL
;; [ lns ]	== LIST OF POINT LISTS, ONE PER POLYLINE (SEE std:PolylinePoints)
;; [ xs ]	== X OF EACH LINE, SORTED WEST TO EAST (SMALLEST FIRST)
;; [ rng ]	== ( YMIN YMAX ) PER LINE TO KEEP ONLY THE CROSSINGS NEEDED, OR NIL FOR ALL (KEEPS THE SORT SMALL ON LARGE SITES)


(defun std:AssocMerge ( lod def / )
	(mapcar
		'(lambda ( pair / val )
			(if (and (setq val (assoc (car pair) lod)) (= 'STR (type (cdr val))))
				val
				pair
			)
		)
		def
	)
)
;; OVERLAYS LOADED STRING VALUES ONTO A DEFAULT ASSOCIATION LIST, KEYS MISSING FROM [ lod ] (OR NOT STRINGS) KEEP THEIR DEFAULT
;; RETURNS AN ASSOCIATION LIST WITH THE SAME KEYS AND ORDER AS THE DEFAULTS, KEYS ONLY IN [ lod ] ARE DROPPED
;; [ lod ]	== LOADED ( "KEY" . "VALUE" ) PAIRS (EX. FROM A SETTINGS FILE)
;; [ def ]	== DEFAULT ( "KEY" . "VALUE" ) PAIRS


(defun std:SubtractInterval ( ivs lo hi / out )
	(foreach iv ivs
		(if (or (<= hi (car iv)) (>= lo (cadr iv)))
			(setq out (cons iv out))
			(progn
				(if (< (car iv) lo)
					(setq out (cons (list (car iv) lo) out))
				)
				(if (< hi (cadr iv))
					(setq out (cons (list hi (cadr iv)) out))
				)
			)
		)
	)
	out
)
;; RETURNS THE ( START END ) INTERVALS [ ivs ] WITH THE SPAN [ lo ] TO [ hi ] REMOVED, INTERVAL ORDER IS NOT KEPT
;; EX. (std:SubtractInterval '((0 10)) 3 5) -> ((5 10) (0 3))


(defun std:GroupRuns ( lst key / grp cur out )
	(foreach itm lst
		(if (and grp (equal (apply key (list itm)) cur))
			(setq grp (cons itm grp))
			(setq
				out (if grp (cons (reverse grp) out) out)
				grp (list itm)
				cur (apply key (list itm))
			)
		)
	)
	(reverse (if grp (cons (reverse grp) out) out))
)
;; SPLITS A LIST INTO RUNS OF NEIGHBORING ITEMS WITH THE SAME KEY, ORDER IS KEPT (ITEMS WITH THE SAME KEY THAT ARE NOT NEIGHBORS START A NEW RUN)
;; EX. (std:GroupRuns '((1 a) (1 b) (2 c)) 'car) -> (((1 a) (1 b)) ((2 c)))
;; [ lst ]	== LIST, ALREADY SORTED SO EQUAL KEYS ARE NEIGHBORS
;; [ key ]	== FUNCTION RETURNING THE KEY OF AN ITEM (COMPARED WITH equal), EX. 'car OR (function (lambda ( itm ) (nth 10 itm)))


(defun std:GroupByIndex ( prs num / pos cur out )
	(setq pos 0)
	(repeat num
		(setq cur nil)
		(while (and prs (= pos (car (car prs))))
			(setq
				cur (cons (cdr (car prs)) cur)
				prs (cdr prs)
			)
		)
		(setq
			out (cons cur out)
			pos (1+ pos)
		)
	)
	(reverse out)
)
;; CONVERTS ( INDEX . VALUE ) PAIRS SORTED BY INDEX INTO A LIST OF [ num ] VALUE LISTS, ONE PER INDEX, SO (nth INDEX ...) FINDS THEM
;; EX. (std:GroupByIndex '((0 . a) (0 . b) (2 . c)) 3) -> ((b a) nil (c))




;;; FILES AND FOLDERS ;;;

(defun std:FindSubDirectory ( dir sub lvl / SubDirectoryNames que pth res tmp )
	(defun SubDirectoryNames ( pth / )
		(vl-remove-if '(lambda ( nam ) (member nam '("." ".."))) (vl-directory-files pth nil -1)) ;; -1 = DIRECTORIES ONLY
	)
	;; RETURNS THE NAMES OF EVERY SUB-DIRECTORY DIRECTLY INSIDE THE TARGET DIRECTORY
	
	(setq sub (vl-string-right-trim "\\" (vl-string-left-trim "\\" sub)))
	(setq que (list (cons (vl-string-right-trim "\\" dir) 0))) ;; QUEUE OF ( PATH . DEPTH ) PAIRS, PROCESSED BREADTH FIRST
	(while (and que (null res))
		(setq pth (car que) que (cdr que))
		(cond
			((vl-file-directory-p (setq tmp (strcat (car pth) "\\" sub))) (setq res tmp)) ;; SHALLOWEST MATCH WINS, SO THE CLOSEST BRIDGE IS RETURNED FIRST
			((< (cdr pth) lvl)
				(setq que
					(append que ;; APPENDING TO THE TAIL KEEPS THE SEARCH BREADTH FIRST RATHER THAN DIVING DOWN ONE BRANCH
						(mapcar '(lambda ( nam ) (cons (strcat (car pth) "\\" nam) (1+ (cdr pth)))) (SubDirectoryNames (car pth)))
					)
				)
			)
		)
	)
	res
)
;; SEARCHES DOWNWARD FROM THE PASSED DIRECTORY FOR THE BRANCH THAT BRIDGES INTO THE KNOWN SUB-PATH
;; RETURNS A CLEANED (NO TRAILING "\\") FULL PATH IF FOUND, ELSE NIL
;; [ dir ]	== DIRECTORY TO SEARCH FROM
;; [ sub ]	== KNOWN SUB-PATH (EX. "02 ELECTRICAL\\^DWG\\XREF\\"), WILDCARD CHARACTERS ARE TAKEN LITERALLY
;; [ lvl ]	== HOW MANY LEVELS BELOW [ dir ] THE BRIDGE MAY START, KEEPS THE SCAN BOUNDED


(defun std:FileTimeStamp ( pth / tm )
	(if (and (= 'STR (type pth)) (setq tm (vl-file-systime pth)))
		(apply '+
			(mapcar '*
				(list (nth 0 tm) (nth 1 tm) (nth 3 tm) (nth 4 tm) (nth 5 tm) (nth 6 tm)) ;; INDEX 2 IS DAY OF WEEK, NOT USED FOR SORTING
				(list 1e10 1e8 1e6 1e4 1e2 1e0)
			)
		)
	)
)
;; RETURNS A FILE'S LAST MODIFIED TIME AS ONE COMPARABLE NUMBER (YYYYMMDDHHMMSS, EX. 20261102143005.0), NIL IF THE FILE DOES NOT EXIST
;; SAVE IT AS TEXT WITH (rtos val 2 0), READ IT BACK WITH std:ToNumber
;; [ pth ]	== FULL FILE PATH


(defun std:FindNewestFile ( dir ptn / nam res tim tmp )
	(if (vl-file-directory-p (setq dir (vl-string-right-trim "\\" dir)))
		(foreach nam (vl-directory-files dir ptn 1) ;; 1 = FILES ONLY, NO SUB-DIRECTORIES
			(if (and (setq tmp (std:FileTimeStamp (strcat dir "\\" nam))) (or (null tim) (> tmp tim)))
				(setq tim tmp res nam)
			)
		)
	)
	res
)
;; SEARCHES A DIRECTORY FOR THE MOST RECENTLY MODIFIED FILE MATCHING A WILDCARD PATTERN
;; RETURNS THE FILE NAME ONLY (NO PATH) IF FOUND, ELSE NIL
;; [ dir ]	== DIRECTORY TO SEARCH
;; [ ptn ]	== WILDCARD PATTERN (EX. "*.dwg")


(defun std:CreateDirectoryPath ( pth / par )
	(setq pth (vl-string-right-trim "\\" pth))
	(cond
		((vl-file-directory-p pth)
			pth
		)
		((and
				(setq par (vl-filename-directory pth))
				(/= par "")
				(/= par pth)
				(std:CreateDirectoryPath par) ;; BUILD PARENT FOLDERS FIRST, VL-MKDIR ONLY CREATES ONE LEVEL AT A TIME
				(vl-mkdir pth)
				(vl-file-directory-p pth)
			)
			pth
		)
	)
)
;; CREATES THE PASSED DIRECTORY PATH INCLUDING ANY MISSING PARENT FOLDERS
;; RETURNS THE CLEANED (NO TRAILING "\\") PATH IF IT EXISTS OR WAS CREATED, ELSE NIL
;; [ pth ]	== FULL DIRECTORY PATH


(defun std:IsFileLocked ( pth / hnd )
	(cond
		((not (and (= 'STR (type pth)) (findfile pth)))
			nil ;; CHECKED FIRST, OPENING A MISSING FILE IN APPEND MODE WOULD CREATE IT
		)
		((setq hnd (open pth "a"))
			(close hnd)
			nil
		)
		( t )
	)
)
;; CHECKS IF AN EXISTING FILE IS LOCKED (OPEN BY ANOTHER USER OR READ-ONLY) AND CANNOT BE APPENDED TO, THE FILE IS NOT CHANGED
;; RETURNS T IF THE FILE IS LOCKED, NIL IF IT IS NOT LOCKED OR DOES NOT EXIST
;; [ pth ]	== FULL FILE PATH




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


