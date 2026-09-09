;; FUNCTION TO INSERT SELECTED LOGO BASED ON REPOSITORY LOCATION
;; C:\_ACC\ACCDocs\ANS Team\ANS_STD\Project Files\03_GLOBAL_CAD\05_Blocks_General\Client Logo Repository

(defun c:UCL ( )
	(c:UpdateClientLogo)
)

(defun c:UpdateClientLogo
	(
		/
		;Functions
		*error*
		InitializeBlockRegistry
		GetTitleBlockRef UpdateTitleBlockDefinition InsertLogoBlock PreviousLogoRemoved PurgeBlockDef
		EditTitleBlockInternal EditTitleBlockExternal IsExternalReferenceLocked
		;Variables
		bmap clnt
		dir dbx fns lgs
		;DCL Functions
		DCL:Cancel DCL:FilterClientLogoList
		;DCL Variables
		ttl dcl des dch res
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
		(if (and src (eq (type src) 'VLA-OBJECT) (not (vlax-object-released-p src)))
			(std:ObjectRelease (list src)) ;; IF DBX WAS SET THIS SHOULD BE LOADED
		)
		(if (not (member msg (list "Function cancelled" "quit / exit abort")))
			(vl-bt)
		) ;; FOR DEBUGGING
		(strcat "\nError: " msg)
	)
	
	(defun InitializeBlockRegistry ( pth / bnm )
		(if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-open (list dbx pth :vlax-true))))
			(progn
				(vlax-for blk (vla-get-blocks dbx)
					(setq bnm (vla-get-name blk))
					(if (and
							(= (vla-get-islayout blk) :vlax-false)
							(= (vla-get-isxref blk) :vlax-false)
							(not (wcmatch bnm "`**"))
						)
						(setq bmap (cons (cons bnm pth) bmap))
						;; CREATES DOTTED PAIRS: ("BlockName" . "C:\\Full\\Path\\To\\File.dwg")
					)
				)
			)
		)
	)
	;; CREATES A KEY VALUE LIST OF FILE NAMES AND BLOCK NAMES ASSOCIATED WITH THE FILE
	;; MAKES ADJUSTMENT TO THE 'bmap' PRIVATE VARIABLE
	
	(defun EditTitleBlockInternal ( blk / doc bnm res )
		(setq doc (vla-get-document blk))
		(setq bnm (vla-get-name blk))
		(setq res (UpdateTitleBlockDefinition doc bnm))
		(if res
			(prompt (strcat "\nSuccess: Internal title block [" bnm "] updated in local active memory."))
			(prompt (strcat "\nError: Local title block validation failed."))
		)
		res
	)
	;; EDITS TITLE BLOCK THAT IS INTERNAL TO THE DRAWING FILE, RETURNS T IF SUCCESSFUL ELSE NIL
	
	(defun EditTitleBlockExternal ( blk / pth dbx bnm res err )
		(cond
			((null (setq pth (findfile (vla-get-path blk))))
				(alert "Aborting: Path to externally referenced block could not be determined.")
				nil
			)
			((IsExternalReferenceLocked pth)
				(alert "Aborting: External file cannot currently be written to. It may be locked or open by another user.")
				nil
			)
			((null (setq dbx (std:CreateObjectDBX)))
				;; ERROR HANDLING MANAGED INTERNALLY BY STD:CREATEOBJECTDBX FUNCTION
				nil
			)
			((not (vl-catch-all-error-p (vl-catch-all-apply 'vla-open (list dbx pth :vlax-false))))
				;; *Model_Space - SEARCH ONLY MODEL SPACE, THIS LIMITS THE AMOUNT OF EXPOSED BLOCKS AND SHOULD IGNORE DELETED ONES
				(vlax-for b (vla-item (vla-get-blocks dbx) "*Model_Space")
					(if (= (vla-get-objectname b) "AcDbBlockReference")
						(progn
							(setq bnm (vla-get-name b))
							(if (and (null res) (wcmatch (strcase bnm) "*TITLE BLOCK*,*TITLE_BLOCK*,*TITLEBLOCK*,*TITLE*"))
								(setq res (UpdateTitleBlockDefinition dbx bnm)) ;; RETURNS T OR NIL
							)
						)
					)
				)
				(if res
					(progn
						;; SAFELY WRITE CHANGES TO THE EXTERNAL DWG ON DISK
						(setq err (vl-catch-all-apply 'vla-saveas (list dbx pth)))
						(if (vl-catch-all-error-p err)
							(progn
								(prompt (strcat "\nError: Failed to save changes to external file: " (vl-catch-all-error-message err)))
								(setq res nil) ;; FLIP RETURN TO NIL BECAUSE FILE WRITE FAILED
							)
							(prompt (strcat "\nSuccess: External file updated and saved successfully at: " pth))
						)
					)
					(prompt "\nError: Target title block validation criteria failed inside the external file.")
				)
				(std:ObjectRelease (list dbx))
				res
			)
			( t
				(prompt "\nError: Unable to process external reference updates.")
			)
		)
	)
	;; EDITS TITLE BLOCK THAT IS EXTERNAL TO THE DRAWING FILE, RETURNS T IF SUCCESSFUL ELSE NIL
	
	(defun IsExternalReferenceLocked ( ref / hnd )
		(setq hnd (open ref "a"))
		(if hnd
			(progn
				(close hnd)
				nil
			)
			t
		)
	)
	;; CHECKS IF EXTERNALLY REFERENCED FILE IS LOCKED AND ABLE TO BE APPENDED TO
	;; RETURNS T IF REFERENCE IS LOCKED, ELSE NIL
	
	(defun UpdateTitleBlockDefinition ( doc bnm / lyr str def ins res )
		(setq lyr "G-NPLT-TTLB")
		(setq str "CLIENT LOGO")
		(setq def (vla-item (vla-get-blocks doc) bnm))
		(vlax-for obj def
			(if (and (null res) (= (vla-get-objectname obj) "AcDbMText"))
				(if (and (= (vla-get-layer obj) lyr) (= (vla-get-textstring obj) str))
					(progn
						(setq ins (std:Variant->List (vla-get-insertionpoint obj)))
						(if (PreviousLogoRemoved def ins)
							(setq res (InsertLogoBlock def clnt ins)) ;; INSERT CLIENT NAME THAT WAS SELECTED
						)
					)
				)
			)
		)
		res
	)
	;; CONFIRMS IF THE PASSED BLOCK CONTAINS TEXT "CLIENT LOGO" ON A SPECIFIC NON-PLOT LAYER AND RETURNS T IF THE TITLE BLOCK UPDATE WAS SUCCESSFUL; ELSE NIL
	
	(defun InsertLogoBlock ( def clnt ins / pth src blks tgt arr err ref dic tbl )
		(setq pth (cdr (assoc clnt bmap)))
		(setq err t) ;; PRESET ERROR VARIABLE UNTIL ALL CHECKS ARE PASSED
		(cond
			((null (setq src (std:CreateObjectDBX)))
				(prompt "\nError: Unable to interface with source logo file.")
			)
			((vl-catch-all-error-p (vl-catch-all-apply 'vla-open (list src pth :vlax-true)))
				(prompt "\nError: Failed to open source logo file via DBX.")
			)
			((vl-catch-all-error-p (setq tgt (vl-catch-all-apply 'vla-item (list (vla-get-blocks src) clnt))))
				(prompt (strcat "\nError: Failed to acquire target block '" clnt "' from source."))
			)
			( t
				(setq arr (vlax-make-safearray vlax-vbobject '(0 . 0)))
				(vlax-safearray-put-element arr 0 tgt)
				(vla-CopyObjects src arr (vla-get-blocks (vla-get-document def)))
				;; CLONE THE BLOCK DEFINITION BLUEPRINT
				
				(setq err
					(vl-catch-all-error-p
						(setq ref (vl-catch-all-apply 'vla-InsertBlock 
								(list def (vlax-3d-point ins) clnt 1.0 1.0 1.0 0.0)
							)
						)
					)
				) ;; ATTEMPT THE INSERTION AND SAVE THE ERROR STATE (T OR NIL)
				
				(if err
					(prompt "\nError: Insertion of client logo failed.")
					(vl-catch-all-apply
						'(lambda ( )
							(setq dic (vla-getextensiondictionary def))
							(setq tbl (vla-addobject dic "ACAD_SORTENTS" "AcDbSortentsTable"))
							(setq arr (vlax-make-safearray vlax-vbobject '(0 . 0)))
							(vlax-safearray-put-element arr 0 ref)
							(vla-movetobottom tbl (vlax-make-variant arr))
						)
					)
					;; MOVE IMAGE LOGO TO BACK DUE TO LAYER OVERLAPPING
				)
			)
		)
		(std:ObjectRelease (list src))
		(not err)
	)
	;; INSERTS LOGO BLOCK FROM SOURCE LOCATION BASED ON PASSED NAME 'CLNT' INTO THE BLOCK DEFINITION FOR THE PASSED TITLE BLOCK
	;; USES PUBLIC VARIABLES 'bmap' AND 'clnt' TO GRAB THE CORRECT LOGO PATH
	
	(defun PreviousLogoRemoved ( def ins / lgo bnm bpt err )
		(vlax-for obj def
			(if (and (null lgo) (= (vla-get-objectname obj) "AcDbBlockReference"))
				(progn
					(setq bpt (std:Variant->List (vla-get-insertionpoint obj)))
					(if (equal ins bpt 0.001)
						(setq
							lgo obj
							bnm (vla-get-effectivename obj)
						)
					)
				)
			)
		)
		(if lgo
			(progn
				(setq err (vl-catch-all-error-p (vl-catch-all-apply 'vla-delete (list lgo))))
				(if err
					(prompt "\nError: Failed to delete previous logo from title block.")
					(progn
						(prompt "\nSuccess: Previous logo block definition removed.")
						(PurgeBlockDef (vla-get-document def) bnm)
					)
				)
			)
			(progn
				(prompt "\nNotice: No previous logo was found.")
				t
			)
		)
		(not err)
	)
	;; REMOVES PREVIOUS LOGO INSERTION IF IT EXISTS, RETURNS T IF SUCCESSFUL OR IF NO BLOCK WAS FOUND WITHIN INSERTION BOUNDRY; RETURNS NIL IF DELETE FAILS
	
	(defun PurgeBlockDef ( doc bnm / blk )
		(setq blk (vl-catch-all-apply 'vla-item (list (vla-get-blocks doc) bnm)))
		(cond
			( (vl-catch-all-error-p blk)
				(prompt "\nNotice: Logo block definition not found in drawing.")
				nil
			)
			( (vl-catch-all-error-p (vl-catch-all-apply 'vla-delete (list blk)))
				(prompt "\nNotice: Logo block still referenced elsewhere. Requires manual purge.")
				nil
			)
			( t
				(prompt "\nSuccess: Previous logo block purged from source drawing.")
				t
			)
		)
	)
	;; PURGES OUT LINGERING LOGO BLOCK FROM DRAWING SOURCE AFTER DELETION
	
	(defun GetTitleBlockRef ( doc / col bnm def res success )
		(setq col (vla-get-blocks doc))
		(vlax-for blk (vla-item col "*Paper_Space") ;; SEARCH ONLY PAPER SPACE BLOCKS / REFERENCES
			(if (and
					(vlax-read-enabled-p blk)
					(not (vlax-erased-p blk))
					(wcmatch (vla-get-objectname blk) "AcDbBlockReference,AcDbMInsertBlock")
					(null res)
				)
				(progn 
					(setq bnm (vla-get-name blk))
					(if (wcmatch (strcase bnm) "*TITLE BLOCK*,*TITLE_BLOCK*,*TITLEBLOCK*,*TITLE*")
						(progn
							(setq def (vla-item col bnm))
							(setq success
								(if (= (vla-get-isxref def) :vlax-true)
									(EditTitleBlockExternal blk) ;; RETURNS T OR NIL
									(EditTitleBlockInternal blk) ;; RETURNS T OR NIL
								)
							)
							(if success (setq res blk))
						)
					)
				)
			)
		)
		res
	)
	;; EDITS AND RETURNS TITLE BLOCK OBJECT FROM DOCUMENT
	
	
	;; MAIN ENTRY
	(setq dir "C:\\_ACC\\ACCDocs\\ANS Team\\ANS_STD\\Project Files\\03_GLOBAL_CAD\\05_Blocks_General\\Client Logo Repository\\")
	(setq bmap nil) ;; RESET GLOBAL REGISTRY MAP
	
	; (setq foo (getstring "Enter client logo name" t)) ;; DELETE THIS // TESTING ONLY
	
	(cond
		((null (vl-file-directory-p dir))
			(princ (strcat "\nDirectory is invalid: '" dir "'."))
		)
		((null (setq dbx (std:CreateObjectDBX)))
			;; ERROR HANDLING MANAGED INTERNALLY BY STD:CREATEOBJECTDBX FUNCTION
		)
		((null (setq fns (vl-directory-files dir "*.dwg" 1)))
			(princ (strcat "\nNo valid drawing files found: '" dir "'."))
			(std:ObjectRelease (list dbx))
		)
		( t
			(foreach fn fns 
				(InitializeBlockRegistry (strcat dir fn))
			)
			;; PROCESS FILES ONE-BY-ONE, PASSING THE FULL PATH TO DBX
			
			(std:ObjectRelease (list dbx))
			;; SAFELY CLOSE OUT THE ACTIVE DBX INTERFACE IMMEDIATELY AFTER HARVESTING
			
			(if bmap
				(progn
					(setq lgs (vl-sort (mapcar 'car bmap) '<))
					(princ "\nSuccessfully mapped repository blocks.")
					
					;; DCL DIALOG LOGIC
					(setq ttl "Logo_Master_List")
					(setq dcl
						(list
							"// Temporary DCL file;"
							(strcat ttl " : dialog {")
							"	label = \"Logo Master List Dialog Control\";"
							"	: row {"
							"		: boxed_column {"
							"			label = \"Client logo name\";"
							"			: list_box {"
							"				label = \"Client names: \";"
							"				multiple_select = false;"
							"				width = 32;"
							"				key = \"clnt\";"
							"			}"
							"			: edit_box {"
							"				label = \"Filter: \";"
							"				key = \"fltr\";"
							"			}"
							"			spacer_1;"
							"			errtile;"
							"			ok_cancel;"
							"		}"
							"	}"
							"}"
						)
					)
					
					(defun DCL:RunDialogChecks ( / doc col def )
						(cond
							((null clnt)
								(set_tile "error" "Error: No client selected.")
							)
							( t
								(done_dialog 1)
								
								; (alert clnt)
								;; FETCH, ROUTE, AND EXECUTE THE EDITS
								(setq tblk (vl-catch-all-apply 'GetTitleBlockRef (list (vla-get-activedocument (vlax-get-acad-object)))))
								
								;; ERROR HANDLING CHECK
								(if (or (null tblk) (vl-catch-all-error-p tblk))
									(prompt "\nError encountered or operation cancelled during Title Block search / edit.")
									
									;; POST-PROCESSING CONTEXT HANDLING
									(progn
										;; ESTABLISH SAFETY POINTERS TO THE ACTIVE DRAWING DATABASE
										(setq
											doc (vla-get-activedocument (vlax-get-acad-object))
											col (vla-get-blocks doc)
											def (vla-item col (vla-get-name tblk)) ;; SAFELY GET THE BACKGROUND DEFINITION RECORD
										)
										
										(if (= (vla-get-isxref def) :vlax-true)
											;; CASE A: IF IT'S EXTERNAL, TELL THE HOST DRAWING'S DEFINITION TABLE TO RELOAD THE NEWLY SAVED FILE
											(progn
												(vla-reload def)
												(vla-regen doc acAllViewports)
												(princ "\nExternal reference updated and reloaded successfully.")
											)
											;; CASE B: IF IT'S INTERNAL, SIMPLY REGEN THE SCREEN TO DISPLAY MODIFICATIONS
											(progn
												(vla-regen doc acAllViewports)
												(princ "\nInternal title block updated successfully.")
											)
										)
									)
								)
								(if (and (eq (type tmp) 'STR) (findfile tmp))
									(vl-file-delete tmp)
								)
							)
						)
					)
					;; ;; RUNS INNER FUNCTION DIALOG BOX CHECKS FOR ERRORS
					
					(defun DCL:Cancel ( / )
						(done_dialog 0)
						(if (findfile tmp)
							(vl-file-delete tmp)
						)
					)
					;; CLEANS UP BACKEND IF PROGRAM IS CANCELED
					
					(defun DCL:FilterClientLogoList ( ptr )
						(setq lgs
							(vl-remove-if-not
								'(lambda ( s )
									(eq (substr (strcase s) 1 (strlen ptr)) (strcase ptr))
								)
								(vl-sort (mapcar 'car bmap) '<)
							)
						)
						(start_list "clnt")
						(foreach lgo lgs (add_list lgo))
						(end_list)
					)
					;; FILTERS CLIENT LIST BASED ON MATCHING STRING VALUE FROM FILTER EDIT BOX, ADJUSTS 'lgs' GLOBALLY
					
					(if
						(and
							dcl
							(setq tmp (vl-filename-mktemp "LOGO" nil ".dcl"))
							(setq des (open tmp "w"))
							(foreach line dcl (write-line line des))
							(not (close des))
							(> (setq dch (load_dialog tmp)) 0)
							(new_dialog ttl dch)
						)
						(progn
							(start_list "clnt")
							(foreach lgo lgs (add_list lgo))
							(end_list)
							;; INITIALIZE LOGO LIST WITH LOGO NAMES 'lgs'
							
							(action_tile "accept" "(DCL:RunDialogChecks)")
							(action_tile "cancel" "(DCL:Cancel)")
							
							(action_tile "clnt" "(setq clnt (nth (read $value) lgs))")
							(action_tile "fltr" "(DCL:FilterClientLogoList $value)")
							
							(start_dialog)
							(unload_dialog dch)
							
							; (princ lgs) ;; Temporary printout to view your names array
							; (princ (cdr (assoc "Qcells" bmap)))
							;; Returns: "C:\_ACC\ACCDocs\...\0-9.dwg" (or whichever file holds it)
							
							; (if (assoc foo bmap)
								; (alert (cdr (assoc foo bmap)))
							; ) ;; DELETE THIS // TESTING ONLY
						)
						(prompt "\nError loading dialog box.")
					)
				)
				(princ "\nValid block objects were not returned from the extracted source files.")
			)
		)
	)
	(princ)
)


(Load:DescriptionLog "UPDATECLIENTLOGO / UCL" "Updates client logo within title block")
