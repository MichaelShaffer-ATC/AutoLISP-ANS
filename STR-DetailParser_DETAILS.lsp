;; PULL OBJECTS FROM OUTSIDE DRAWING FILE

(defun c:DETAILS ( / )
	(ECHO:LogWrapper "STR-DetailParser_DETAILS.lsp" "DETAILS" DetailsMain)
)

(defun DetailsMain
	(
		/
		;Functions
		*error*
		PullObjectsByLayers ReturnSubDirectories ParseDetailName
		ObjectBoundingCache DetailCoordinateMap CheckForDetailOverlap
		;Variables
		dir funcs dbx det pfx
		; DCL
		DCL:RunDialogChecks DCL:Cancel DCL:ActionWrapper
		DCL:PopulateAvailableDetails DCL:AddSelectedDetails DCL:RemoveSelectedDetails
		ttl dcl tmp des dch
		subs subdir sel avail slctd
		; Caching
		bnds
		; Parent Variables
		p:crds
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
		(ECHO:Log "ERROR" msg nil (list (cons "CMDNAME" *ECHO:Log-Command*)))
		(ECHO:Log-Flush)
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
	
	
	(defun ObjectBoundingCache ( / cache msp typ ll ur )
		(setq cache nil)
		(setq msp (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object))))
		(vlax-for obj msp
			(setq typ (if (vlax-property-available-p obj "ObjectName") (vla-get-objectname obj)))
			(if (and typ (not (= typ "AcDbPoint")) (not (= typ "AcDbXline")))
				(if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-getboundingbox (list obj 'll 'ur))))
					(setq cache (cons (list (vlax-safearray->list ll) (vlax-safearray->list ur)) cache))
				)
			)
		)
		cache
	)
	;; SETS CACHE FOR OBJECT BOUNDING DATA
	
	
	(defun CheckForDetailOverlap ( crd mgn dst / clt )
		(setq clt (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object))))
		(while
			(or
				(vl-remove-if-not 
					'(lambda ( bnd / pt1 pt2 )
						(setq pt1 (car bnd) pt2 (cadr bnd))
						(and
							(< (- (car crd) mgn) (car pt2))
							(> (+ (car crd) mgn) (car pt1))
							(< (- (cadr crd) mgn) (cadr pt2))
							(> (+ (cadr crd) mgn) (cadr pt1))
						)
					)
					(if (not bnds) (setq bnds (ObjectBoundingCache)) bnds)
				) ;; CHECK FOR EXISTING OBJECT OVERLAP
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
				) ;; CHECK THAT COORDINATE HAS NOT BEEN USED
			)
			(setq crd (list (+ (car crd) dst) (cadr crd) (caddr crd)))
		) ;; CHECK PHYISICALLY EXISTING GEOMETRY AND CURRENTLY SET COORDINATES BEFORE GEOMETRY IS TRANSCRIBED TO MODELSPACE
		crd
	)
	;; UPDATES PASSED COORDINATE X LOCATION IF ANY DETAIL OBJECTS ARE FOUND IN THE MARGIN WINDOW
	
	(defun DetailCoordinateMap ( det )
		(cond
			((wcmatch det "BOS,PV,HV,FENCE")
				(list 229.044 313.324 0.0)
			)
			((wcmatch det "BESS")
				(list 229.044 1013.32 0.0)
			)
			((wcmatch det "FND")
				(list 229.044 -2386.68 0.0)
			)
			((wcmatch det "CAB")
				(list 229.044 -586.676 0.0)
			)
			( t
				(list 229.044 -1486.68 0.0)
			) ;; MISC OPTIONS
		)
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
					(if (findfile (setq det (strcat dir (setq pfx (substr det 1 (vl-string-search "_" det))) "\\" det)))
						(progn
							(vla-open dbx det :vlax-true)
							(setq crd (CheckForDetailOverlap (DetailCoordinateMap pfx) 180.0 360.0))
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
	
	(setq dir "C:\\_ACC\\ACCDocs\\ANS Team\\ANS_STD\\Project Files\\04_DISCIPLINES\\05_STRUCTURAL\\01_CAD_Standards\\03_Details\\Sorted and Reviewed\\")
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



(Load:DescriptionLog "DETAILS" "Dialog control for inserting selected details based on type")
