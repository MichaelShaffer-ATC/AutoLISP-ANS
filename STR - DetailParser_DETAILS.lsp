;; PULL OBJECTS FROM OUTSIDE DRAWING FILE

(defun c:DETAILS
	(
		/
		;Functions
		*error*
		PullObjectsByLayers ReturnSubDirectories ParseDetailName
		DetailCoordinateMap CheckForExistingDetails
		;Variables
		dir funcs dbx det typ
		; DCL
		DCL:RunDialogChecks DCL:Cancel DCL:ActionWrapper
		DCL:PopulateAvailableDetails DCL:AddSelectedDetails DCL:RemoveSelectedDetails
		ttl dcl tmp des dch
		subs subdir sel avail slctd p:crds
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
		(if (and dbx (eq (type dbx) 'VLA-OBJECT) (not (vlax-object-released-p dbx)))
			(std:ObjectRelease (list dbx)) ;; IF DBX WAS SET THIS SHOULD BE LOADED
		)
		(if (not (member msg (list "Function cancelled" "quit / exit abort")))
			(vl-bt)
		) ;; FOR DEBUGGING
		(strcat "\nError: " msg)
	)

	(defun PullObjectsByLayers ( dbx bit lyrs scl crds / objs tmat lst out res )
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
				(if (= (logand 1 bit) 1) (setq objs (append (list "AcDbLine" "AcDbPolyline" "AcDbPolyline2d" "AcDbCircle" "AcDbArc" "AcDbEllipse" "AcDbSpline" "AcDbHatch") objs)))
				(if (= (logand 2 bit) 2) (setq objs (append (list "AcDbRotatedDimension" "AcDbAlignedDimension" "AcDbArcDimension" "AcDbRadialDimension" "AcDbLeader" "AcDbMLeader") objs)))
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
	;; BIT == INTEGER FOR 0 -> 15, 0 INCLUDES ALL OBJECTS, 1 = GEOMETRY, 2 = ANNOTATIONS, 4 = BLOCKS / HATCHES, 8 = CONSTRUCTION LINES / POINTS
	;; LYRS == PATTERN STRING FOR WANTED LAYERS; "Layer*,0,*New*" (INCLUDES LAYER "0", ANY LAYER THAT STARTS WITH "Layer" AND ANY LAYER THAT CONTAINS "New"
	;; SCL 	== INTEGER VALUE FOR SCALE OR NIL
	;; CRDS == (list X Y Z) COORDINATES
	
	(defun ReturnSubDirectories ( pth )
		(if (vl-file-directory-p pth)
			(vl-remove-if
				'(lambda ( d )
					(member d (list "." "..")) ;; REMOVE PARENT ROOT FOLDERS
				)
				(vl-directory-files pth nil -1)
			)
		)
	)
	
	
	(defun ParseDetailName ( nm ext / pos )
		(setq pos (vl-string-search "_" nm))
		(if pos
			(setq pos (+ 2 pos) nm (substr nm pos))
		)
		(vl-string-right-trim ext nm)
	)
	;; UPDATES PASSED STRING TO REMOVE PREFIX AND ".dwg" EXTENSION FROM STRING
	;; RETURNS PARSED STRING VALUE
	
	
	(defun CheckForExistingDetails ( crd mgn dst / sset )
		(while
			(or
				(setq sset
					(ssget "_C"
						(list (- (car crd) mgn) (- (cadr crd) mgn))
						(list (+ (car crd) mgn) (+ (cadr crd) mgn))
						'((-4 . "<NOT") (0 . "POINT,XLINE") (-4 . "NOT>"))
					)
				)
				(vl-member-if
					'(lambda ( cr )
						(and
							(>= (car crd)	(- (car cr) mgn))
							(<= (car crd)	(+ (car cr) mgn))
							(>= (cadr crd)	(- (cadr cr) mgn))
							(<= (cadr crd)	(+ (cadr cr) mgn))
						)
					)
					p:crds
				)
			)
			(setq crd (list (+ (car crd) dst) (cadr crd) (caddr crd)))
		) ;; CHECK PHYISICALLY EXISTING GEOMETRY
		crd
	)
	;; UPDATES PASSED COORDINATE X LOCATION IF ANY DETAIL OBJECTS ARE FOUND IN THE MARGIN WINDOW
	
	(defun DetailCoordinateMap ( det / crd )
		(cond
			((wcmatch det "BOS,PV,HV,FENCE")
				(setq crd (list 229.044 313.324 0.0))
			)
			((wcmatch det "BESS")
				(setq crd (list 229.044 1013.32 0.0))
			)
			((wcmatch det "FND")
				(setq crd (list 229.044 -2386.68 0.0))
			)
			((wcmatch det "CAB")
				(setq crd (list 229.044 -586.676 0.0))
			)
			( t
				(setq crd (list 229.044 -1486.68 0.0))
			) ;; MISC OPTIONS
		)
		(CheckForExistingDetails crd 8.00 360.00)
	)
	;; POTENTIALLY UPDATE USING 'WCMATCH' WILDCARD PATTERNS FOR DETAILS THAT ARE SIMILARLY RELATED (PCS / BESS)
	
	(vl-load-com)
	
	;; DCL LAYOUT
	(setq ttl "Details")
	(setq dcl
		(list
			"// Temporary DCL file;"
			(strcat ttl " : dialog {")
			"	label = \"Detail Parser Dialog Control\";"
			"	: row {"
			"		: column {"
			"			: popup_list {"
			"				key = \"subdir\";"
			"				label = \"Detail Type\";"
			"				width = 32;"
			"				fixed_width = true;"
			"			}"
			"			spacer_1;"
			"			errtile;"
			"			ok_cancel;"
			"		}"
			"		: column {"
			"			: list_box {"
			"				key = \"add_lb\";"
			"				label = \"Available Details\";"
			"				multiple_select = true;"
			"				width = 40;"
			"				fixed_width = true;"
			"			}"
			"			: row {"
			"				: spacer {"
			"					width = 1;"
			"				}"
			"				: button {"
			"					key = \"add\";"
			"					label = \"Add Detail\";"
			"					fixed_width = true;"
			"				}"
			"				: spacer {"
			"					width = 1;"
			"				}"
			"			}"
			"		}"
			"		: column {"
			"			: list_box {"
			"				key = \"rmv_lb\";"
			"				label = \"Selected Details\";"
			"				multiple_select = true;"
			"				width = 40;"
			"				fixed_width = true;"
			"			}"
			"			: row {"
			"				: spacer {"
			"					width = 1;"
			"				}"
			"				: button {"
			"					key = \"rmv\";"
			"					label = \"Remove\";"
			"					fixed_width = true;"
			"				}"
			"				: spacer {"
			"					width = 1;"
			"				}"
			"			}"
			"		}"
			"	}"
			"}"
		)
	)
	
	;; DCL FUNCTIONS
	(defun DCL:RunDialogChecks ( / )
		(cond
			((null slctd)
				(set_tile "error" "No details were selected.")
			)
			( t
				(done_dialog 1)
				
				;;; MAIN LOGIC AFTER DIALOG FINISHES
				
				(foreach det slctd
					(if (findfile (setq det (strcat dir (setq typ (substr det 1 (vl-string-search "_" det))) "\\" det)))
						(progn
							(vla-open dbx det :vlax-true)
							(setq crd (DetailCoordinateMap typ))
							(setq p:crds (cons crd p:crds))
							(PullObjectsByLayers dbx 7 nil nil crd) ;; 7 = IGNORE ANY CONSTRUCTION LINES OR NODES WHEN COPYING DETAILS OVER
						)
					)
				)
				
				(if (findfile tmp)
					(vl-file-delete tmp)
				)
				(if (and dbx (eq (type dbx) 'VLA-OBJECT) (not (vlax-object-released-p dbx)))
					(std:ObjectRelease (list dbx))
				)
			)
		)
	)
	
	(defun DCL:Cancel ( / )
		(done_dialog 0)
		(if (findfile tmp)
			(vl-file-delete tmp)
		)
	)
	;; CLEANS UP BACKEND IF PROGRAM IS CANCELED
	
	(defun DCL:ActionWrapper ( call / )
		(set_tile "error" "")
		(eval (read call))
	)
	;; RESETS ERROR TILE BASED ON ACTIONS
	
	(defun DCL:PopulateAvailableDetails ( / idx pth )
		(if (/= (setq idx (read (get_tile "subdir"))) "") ;; INDEX GETS PASSED FROM TILE RETURN AND NEEDS TO BE CONVERTED
			(setq pth (strcat dir (nth idx subs)))
			(setq pth dir)
		)
		(if (vl-file-directory-p pth)
			(progn
				(setq avail (vl-directory-files pth "*.dwg" 1)) ;; RETURN FILES ONLY
				(start_list "add_lb")
				(foreach itm avail
					(add_list (ParseDetailName itm ".dwg"))
				)
				(end_list)
			)
		)
	)
	;; POPULATES AVAILABLE DETAILS FROM CURRENT DIRECTORY
	
	(defun DCL:AddSelectedDetails ( / )
		(if (null sel)
			(set_tile "error" "Nothing was selected for add.")
			(progn
				(foreach itm (mapcar '(lambda ( x ) (nth x avail)) (read (strcat "(" sel ")")))
					(if (not (member itm slctd))
						(progn
							(setq slctd (append slctd (list itm)))
							(start_list "rmv_lb" 2) ;; APPEND TO LIST
							(add_list (ParseDetailName itm ".dwg"))
							(end_list)
						)
						(set_tile "error" "Duplicate detail selected.")
					)
				)
				(set_tile "add_lb" "") ;; REMOVE GHOSTING SELECTION
			)
		)
		(setq sel nil) ;; CLEAR SELECTION
	)
	;; ADDS TO THE SELECTED LIST BOX
	
	(defun DCL:RemoveSelectedDetails ( / )
		(if (null sel)
			(set_tile "error" "Nothing was selected for remove.")
			(progn
				(foreach itm (mapcar '(lambda ( x ) (nth x slctd)) (read (strcat "(" sel ")")))
					(setq slctd (vl-remove itm slctd))
				)
				(start_list "rmv_lb") ;; CLEARS THE LIST BOX
				(foreach itm slctd (add_list (ParseDetailName itm ".dwg")))
				(end_list)
				(set_tile "rmv_lb" "") ;; REMOVE GHOSTING SELECTION
			)
		)
		(setq sel nil) ;; CLEAR SELECTION
	)
	;; REMOVES SELECTED ITEMS FROM LIST, RE-BUILDS SELECTED LIST
	
	(setq dir "C:\\_ACC\\ACCDocs\\ANS Team\\ANS_STD\\Project Files\\04_DISCIPLINES\\05_STRUCTURAL\\01_CAD_Standards\\03_Details\\Reviewed\\")
	;; DIRECTORY TO ALL DRAWING DETAIL LOCATIONS
	;; _BOOKMARK_
	;; MAY HAVE TO ADD PORTION OF CODE TO USE "USERPROFILE" FOR THOSE THAT DID NOT UPDATE THEIR WORKSPACE ENVIRONMENT FOR ACC
	
	(setq funcs
		(list
			'std:CreateObjectDBX 'std:ObjectRelease
		)
	) ;; ADD ALL REQUIRED OUTSIDE FUNCTIONS HERE
	
	(cond
		((not (vl-every 'boundp funcs))
			(princ
				(vl-string-right-trim
					","
					(strcat
						"Error: Not all required library functions are loaded. Function is unable to continue."
						"\nUndefined / unloaded functions:"
						(apply 'strcat (mapcar '(lambda ( f ) (if (null (boundp f)) (strcat " " (vl-princ-to-string f) ",") "")) funcs))
					)
				)
			)
		)
		((null (vl-file-directory-p dir))
			(princ "\nSource directory not found. Function cancelled.")
		)
		((null (setq dbx (std:CreateObjectDBX)))) ;; ERROR HANDLING MANAGED IN DBX FUNCTION
		(	(not
				(and
					dcl
					(setq tmp (vl-filename-mktemp "DTLS" nil ".dcl"))
					(setq des (open tmp "w"))
					(foreach line dcl (write-line line des))
					(not (close des))
					(> (setq dch (load_dialog tmp)) 0)
					(new_dialog ttl dch)
				)
			)
			(prompt "\nError loading dialog box.")
		)
		( t
			(setq subs (ReturnSubDirectories dir))
			(setq avail '()) ;; MAINTAINS LIST OF AVAILABLE DETAILS FOR EACH SUB-DIRECTORY
			(setq slctd '()) ;; MAINTAINS LIST OF SELECTED DETAILS
		
			(start_list "subdir")
			(foreach sub subs (add_list sub))
			(end_list)
			
			(DCL:PopulateAvailableDetails) ;; INITIALIZE FOR CURRENT DIRECTORY LOCATION
			
			(action_tile "accept" "(DCL:RunDialogChecks)")
			(action_tile "cancel" "(DCL:Cancel)")
			
			(action_tile "add" "(DCL:ActionWrapper \"(DCL:AddSelectedDetails)\")") ;; ONLY ADDS TO THE "SELECTED" LIST BOX
			(action_tile "rmv" "(DCL:ActionWrapper \"(DCL:RemoveSelectedDetails)\")") ;; ONLY READS THE "SELECTED" LIST BOX AND RE-BUILDS THE "SELECTED" LIST (REMOVES USER SELECTED ITEMS)
			
			(action_tile "subdir" "(DCL:ActionWrapper \"(DCL:PopulateAvailableDetails)\")") ;; POPULATE "AVAILABLE" FOR USER SELECTION
			(action_tile "add_lb" "(setq sel $value)") ;; CREATES INDEX STRING FOR ADDING SELECTED ITEMS
			(action_tile "rmv_lb" "(setq sel $value)") ;; CREATES INDEX STRING FOR REMOVING SELECTED ITEMS
			
			(start_dialog)
			(unload_dialog dch)
		)
	)
	
	(princ)
	
)

;; DROPDOWN LIST TO CONTAIN "BUCKETS" FOR DETAIL LOCATION TYPES / NAMES
;; SELECTING A BUCKET DISPLAYS ALL DETAILS ASSOCIATED IN THAT BUCKET THAT THE USER CAN SELECT IN THE LIST BOX
;; SELECTING DETAIL NAMES IN THE LIST BOX AND CLICKING "ADD" ADDS DETAILS TO THE "SELECTED DETAILS" LIST BOX
;; SELECTING DETAIL NAMES IN THE SELECTED LIST BOX ALLOWS THE USER TO REMOVE THEM FROM THE LIST BY SELECTING "REMOVE"
;; DETAILS CAN ONLY BE SELECTED ONCE
;; AFTER SELECTING "OK" THE FUNCTION WILL ITERATE OVER EACH DETAIL IN THE SELECTION LIST BOX TO COPY DETAIL TO CURRENT DRAWING
;; DETAILS ARE TO BE SORTED BASED ON BUCKET AND POINT LOCATIONS
;; POINT LOCATIONS TO BE UPDATED BASED ON IF GEOMETRY IS FOUND AT THE INSERTION POINT
;; DETAIL COPIED INTO CURRENT DRAWING SUCCESSFULLY, END
