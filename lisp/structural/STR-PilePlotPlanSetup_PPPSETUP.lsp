;; AUTOMATION FOR CREATING / STANDARDIZING PPP/BOM FILES
;; COMMANDS: PPPSETUP (NEW PPP SETUP DRAWING), PPPASSIGN (ARRAY BLOCKS TO TRACKER PRESETS, ALSO RUN AT THE END OF PPPSETUP)
;;           PPPSETTINGS (EDIT THE SETTINGS / TRACKER PRESETS OF AN EXISTING PROJECT), PPPPILES (PILE LABELS, PILE DRIVE FILE, BOM)
;;           PPPBOM (PILE DRIVE FILE AND BOM FROM THE PILE LABELS, NO SOLVE)
;; FILE MAKES OUTSIDE CALLS TO THE SHARED PPP FUNCTIONS (STR-PPP_Utilities.lsp), MUST BE LOADED FIRST
;; FILE MAKES OUTSIDE CALLS TO LIBRARY FILE FOR WSH FUNCTIONS (LIB-WSH_Functions.lsp)
;; FILE MAKES OUTSIDE CALLS TO LIBRARY FILE FOR JSON FUNCTIONS (LIB-JSON_Functions.lsp), USED FOR TRACKER PRESETS
;; FILE MAKES OUTSIDE CALLS TO LIBRARY FILE FOR STANDARD FUNCTIONS (LIB-STD_Sub-Functions.lsp), FILES, LISTS, LENGTH TEXT, DBX COPIES
;; FILE MAKES OPTIONAL CALLS TO LIBRARY FILE FOR GEOLOCATION FUNCTIONS (LIB-GEO_Functions.lsp), SKIPPED IF IT IS NOT LOADED
;; FILE MAKES OPTIONAL CALLS TO LIBRARY FILE FOR EXCEL FUNCTIONS (LIB-MSXL_Library_Functions.lsp), PPPPILES SKIPS THE .xlsx OUTPUT IF IT IS NOT LOADED
;; EACH MAIN FUNCTION LISTS THE LIBRARY FUNCTIONS IT CALLS IN 'funcs', THE SHARED 'PPP:' FUNCTIONS LIST THEIRS IN PPP:RequiredFunctions

;; STEPS:
;; - SELECT PROJECT ROOT AND ESTABLISH PPP-BOM FOLDER STRUCTURE UNDER STRUCTURAL
;; - LOAD PROJECT SETTINGS (IF THEY EXIST) AND DISPLAY SETTINGS DIALOG FOR USER INPUT (PPP:SettingsDialog, SHARED WITH PPPSETTINGS)
;; - MANAGE TRACKER PRESETS IN A NESTED DIALOG, SAVED TO PPP-PRESETS.json IN THE PPP-BOM ROOT (PVCASE .PVsettings FILES CAN BE IMPORTED)
;; - FIND X-ARRAY FILE FROM ELECTRICAL LOCATION (USER CONFIRMS OR SELECTS) AND COPY IT, ITS PATH AND DATE ARE SAVED FOR THE CHANGE CHECK
;; - PICK THE TRACKER BLOCKS FROM THE BLOCKS ON THE ARRAY LAYERS (SIZE MATCHES TO THE PRESETS ARE PRE-SELECTED), EXPLODE GROUP BLOCKS ONE LEVEL
;; - FLATTEN ARRAY OBJECTS TO Z = 0
;; - FIND THE CIVIL TOPO (V-TOPO*.dwg, EXISTING GRADE) FROM THE CIVIL XREF LOCATION (USER CONFIRMS OR SELECTS), CHECK ITS GEOLOCATION AGAINST
;;   THE X-ARRAY AND REFERENCE IT AS THE "PPP-CIVIL-TOPO" OVERLAY XREF (NOT COPIED), ITS PATH AND DATE ARE SAVED FOR THE CHANGE CHECK
;; - READ THE GEOLOCATION FROM THE X-ARRAY FILE (OR THE TOPO FILE), SAVED TO PPP-GEOLOCATION.txt IN THE PPP-BOM ROOT
;; - VALIDATE CONTOUR ELEVATIONS (FROM THE XREF) AND ARRAY / CONTOUR ALIGNMENT, OPTIONALLY IGNORE ARRAYS OUTSIDE THE CONTOURS
;; - ASSIGN ARRAY BLOCKS TO TRACKER PRESETS AND INTERIOR / EXTERIOR ROW TYPES (PPPASSIGN, SEE THE PPP ASSIGN SECTION)
;; - SET DRAWING UNITS TO FEET, SAVE DRAWING AND PROJECT SETTINGS TO PPP-BOM LOCATION
;;
;; - EDIT SETTINGS AND TRACKER PRESETS LATER WITHOUT A NEW SETUP DRAWING (PPPSETTINGS, SEE THE PPP SETTINGS SECTION)
;; - PLACE PILE LABELS WITH PILE DATA AND WRITE THE PILE DRIVE FILE AND BOM (PPPPILES, SEE THE PPP PILES SECTION)
;; - WRITE THE PILE DRIVE FILE AND BOM AGAIN FROM THE PILE LABELS, EX. WHEN THE BOM WAS OPEN IN EXCEL (PPPBOM, SEE THE PPP BOM SECTION)
;;
;; FILE LAYOUT: COMMANDS, PPP SETUP, PPP SETTINGS, PPP ASSIGN, PPP PILES, PPP BOM (SHARED 'PPP:' FUNCTIONS, INCLUDING THE SETTINGS DIALOG, ARE IN STR-PPP_Utilities.lsp)


;;; COMMANDS ;;;

(defun c:PPPSetup ( / )
	(if (PPPUtilitiesLoaded)
		; (ECHO:LogWrapper "STR-PilePlotPlanSetup_PPPSETUP.lsp" "PILEPLOTPLANSETUP" PPPSetupMain)
		(PPPSetupMain)
	)
)

(defun c:PPPSettings ( / )
	(if (PPPUtilitiesLoaded)
		; (ECHO:LogWrapper "STR-PilePlotPlanSetup_PPPSETUP.lsp" "PPPSETTINGS" PPPSettingsMain)
		(PPPSettingsMain)
	)
	(princ)
)

(defun c:PPPAssign ( / )
	(if (PPPUtilitiesLoaded)
		; (ECHO:LogWrapper "STR-PilePlotPlanSetup_PPPSETUP.lsp" "PPPASSIGN" (function (lambda ( ) (PPPAssignMain nil))))
		(PPPAssignMain nil)
	)
	(princ)
)
(defun c:PPPPiles ( / )
	(if (PPPUtilitiesLoaded)
		; (ECHO:LogWrapper "STR-PilePlotPlanSetup_PPPSETUP.lsp" "PPPPILES" (function (lambda ( ) (PPPPilesMain nil))))
		(PPPPilesMain nil)
	)
	(princ)
)
;; NIL CONTEXT == RUN AS A COMMAND, THE PROJECT CONTEXT IS READ FROM THE PPP-BOM ROOT (SEE PPP:ProjectContext)

(defun c:PPPBom ( / )
	(if (PPPUtilitiesLoaded)
		; (ECHO:LogWrapper "STR-PilePlotPlanSetup_PPPSETUP.lsp" "PPPBOM" PPPBomMain)
		(PPPBomMain)
	)
	(princ)
)

(defun PPPUtilitiesLoaded ( / )
	(cond
		((not (vl-every '(lambda ( fn ) (and (boundp fn) (eval fn))) '(PPP:MissingFunctions PPP:RequiredFunctions PPP:ErrorReport PPP:FillList)))
			(prompt "\nError: STR-PPP_Utilities.lsp is not loaded, the PPP commands need it. Check the support path / autoloader.")
			nil
		) ;; PPP:FillList IS THE LAST FUNCTION IN THE FILE, SO A LOAD THAT STOPPED PART WAY (EX. A NON-ASCII BYTE) IS CAUGHT
		((PPP:MissingFunctions (PPP:RequiredFunctions))
			nil ;; PPP:MissingFunctions PRINTED THE MISSING NAMES
		)
		( t )
	)
)
;; CHECKED BY EVERY PPP COMMAND BEFORE IT STARTS: THE SHARED 'PPP:' FUNCTIONS AND THE LIBRARY FUNCTIONS THEY CALL
;; EACH MAIN FUNCTION THEN CHECKS THE LIBRARY FUNCTIONS IT CALLS ITSELF ('funcs')

(defun c:PPPDebugOn ( / )
	(setq *PPP:Trace* t)
)

(defun c:PPPDebugOff ( / )
	(setq *PPP:Trace* nil)
)
;; DEBUG FUNCTIONS, "ON" TURNS DEBUG STEPPING / TRACING ON, OFF TURNS IT OFF


;;; PPP SETUP ;;;

(defun PPPSetupMain
	(
		/
		;Functions
		*error* Cleanup
		SourceFile ReadSourceDrawing CreateProjectFolders
		FlagIgnoredObjects ValidateDrawingData
		SaveProjectDrawing
		;Variables
		doc rt elc cvl cvo stc sav subs cfn pfn gfn xfn dir ppp lod cfg prs dlg geo gsv ius svd ent arr cpy flt ign asn ctx
		xpt tpt tgo tpx gck cln
		rws pck grp exg
		funcs
		;DCL Variables, SET BY THE DIALOGS (PPP:SettingsDialog, PPP:ArrayBlockPicker), LOCAL HERE SO *error* ONLY SEES A DIALOG THAT IS OPEN
		tmp des dch
	)
	
	(defun *error* ( msg / )
		(if (eq (type des) 'FILE)
			(close des)
		) ;; 'des' IS NEVER SET HERE: BY DYNAMIC SCOPE THIS CLOSES A FILE LEFT OPEN BY A HELPER THAT NAMES ITS HANDLE 'des' (EX. json:ReadFile, json:WriteFile, PPP:DialogLoad) WHEN AN ERROR OR ESC HITS PART WAY
		(setq dch (PPP:DialogUnload tmp dch)) ;; ALSO DELETES THE TEMPORARY DCL FILE
		(if (and dbx (eq (type dbx) 'VLA-OBJECT) (not (vlax-object-released-p dbx)))
			(std:ObjectRelease (list dbx))
		) ;; 'dbx' IS ALSO SEEN BY DYNAMIC SCOPE, SET ONLY WHILE ReadSourceDrawing IS RUNNING
		(vl-catch-all-apply 'Cleanup) ;; AN ERROR HERE MUST NOT HIDE THE REAL ERROR
		(PPP:ErrorReport msg
			(strcat "\nPPP setup cancelled, the drawing was not saved." (if cfg "\n  Tracker presets were already saved when the settings dialog was accepted." ""))
		)
	)
	;; CLEANUP RUNS FOR BOTH ERRORS AND USER CANCELS, A CANCEL IS LOGGED AS INFO AND SHOWN WITHOUT THE 'Error:' PREFIX (SEE PPP:ErrorReport)
	;; THE DRAWING AND SETTINGS SAVES ARE THE LAST STEPS, SO A CANCEL LEAVES THEM UNSAVED (UNITS ARE RESTORED ABOVE)
	;; TRACKER PRESETS AND THE PPP-BOM FOLDERS ARE KEPT ON PURPOSE, PRESETS ARE SAVED AS SOON AS THE SETTINGS DIALOG IS ACCEPTED
	
	(defun Cleanup ( / blk lst bad )
		(if (and ius (not svd))
			(setvar "INSUNITS" ius)
		) ;; RESTORE ORIGINAL UNITS IF THE DRAWING WAS NOT SAVED
		(if (and cln (not svd))
			(progn
				(if (setq blk (PPP:TopoXref doc))
					(vl-catch-all-apply 'vla-detach (list blk))
				) ;; ALSO ERASES THE XREF INSERT
				(vlax-for obj (vla-get-modelspace doc)
					(setq lst (cons obj lst))
				) ;; COLLECTED FIRST, CHANGING OBJECTS WHILE vlax-for WALKS THE SAME COLLECTION CAN CRASH AUTOCAD
				(setq bad 0)
				(foreach obj lst
					(if (vl-catch-all-error-p (vl-catch-all-apply 'vla-delete (list obj)))
						(setq bad (1+ bad)) ;; LIKELY A LOCKED LAYER
					)
				)
				(setq cln nil) ;; RUNS ONCE, EVEN IF *error* FIRES AFTER THE NORMAL END
				(if (or lst blk)
					(prompt
						(if (< 0 bad)
							(strcat "\n  Warning: " (itoa bad) " copied objects could not be removed (locked layers?), close this drawing and run PPPSETUP in a new one.")
							"\n  The X-ARRAY objects and the Civil topo xref brought in by this run were removed, PPPSETUP can run again in this drawing."
						)
					)
				)
			)
		)
	)
	;; PUTS THE DRAWING BACK THE WAY PPPSETUP FOUND IT WHEN SETUP STOPS BEFORE THE DRAWING IS SAVED (CANCEL, ERROR, FAILED STEP):
	;; UNITS RESTORED, THE CIVIL TOPO XREF DETACHED AND EVERYTHING COPIED INTO MODEL SPACE ERASED, SO A SECOND RUN NEVER STACKS A SECOND X-ARRAY
	;; ERASING ALL OF MODEL SPACE IS SAFE BECAUSE SETUP ONLY STARTS IN AN EMPTY DRAWING ('cln' IS SET AFTER THAT CHECK)
	;; CALLED BY *error* AND AT THE END OF EVERY RUN, BLOCK AND LAYER DEFINITIONS STAY (A RERUN REUSES THEM)
	
	(defun SourceFile ( sub lbl dfn ptn / fdr fnm )
		(cond
			((null (setq fdr (cond ((std:FindSubDirectory dir sub 3)) ((wsh:ReturnFolder (strcat "Select " lbl " File Location") 512 dir)))))
				nil
			)
			((and
					(setq
						fdr (vl-string-right-trim "\\" fdr)
						fnm (cond
								((and dfn (findfile (strcat fdr "\\" dfn))) dfn)
								((if ptn (std:FindNewestFile fdr ptn))) ;; 'if' RETURNS THE FILE NAME, 'and' WOULD ONLY RETURN T
							)
					)
					(= 6 (wsh:ConfirmationDialog (strcat "Use the " lbl " file found in the project?\n\n" fdr "\\" fnm) (strcat lbl " File") 0 36))
				)
				(strcat fdr "\\" fnm)
			)
			((getfiled (strcat "Select " lbl " Drawing File") (strcat fdr "\\") "dwg" 16))
		)
	)
	;; FINDS A SOURCE DRAWING UNDER THE PROJECT ROOT 'dir': THE KNOWN SUB-FOLDER (OR ONE THE USER BROWSES TO), THEN THE DEFAULT FILE NAME OR THE
	;; NEWEST FILE MATCHING THE PATTERN, WHICH THE USER CONFIRMS, ELSE THE USER SELECTS THE FILE (NAMING IS NOT ALWAYS STANDARD)
	;; RETURNS THE FULL PATH, NIL IF NOTHING WAS SELECTED
	;; [ sub ] == KNOWN SUB-PATH (EX. "02 ELECTRICAL\\^DWG\\XREF\\"), [ lbl ] == NAME SHOWN TO THE USER (EX. "X-ARRAY")
	;; [ dfn ] == DEFAULT FILE NAME OR NIL, [ ptn ] == FILE PATTERN FOR THE NEWEST FILE OR NIL (EX. "V-TOPO*.dwg")
	
	(defun ReadSourceDrawing ( pth cpy / dbx gdt )
		(if (and (= ".DWG" (strcase (cond ((vl-filename-extension pth)) ("")))) (setq dbx (std:CreateObjectDBX)))
			(progn
				(vla-open dbx pth :vlax-true)
				(if (std:FunctionLoaded 'geo:ReadGeoData) ;; OPTIONAL, ONLY WHEN LIB-GEO IS LOADED
					(if (vl-catch-all-error-p (setq gdt (vl-catch-all-apply 'geo:ReadGeoData (list dbx))))
						(setq gdt (geo:SetError (strcat "Geolocation could not be read: " (vl-catch-all-error-message gdt))))
					)
				) ;; READ WHILE THE SOURCE IS OPEN, THE GEOLOCATION IS NOT AN ENTITY SO IT IS NOT COPIED WITH THE DRAWING CONTENTS
				(if cpy
					(std:CopyDBXObjects dbx 7 nil nil '(0 0 0)) ;; 7 = IGNORE ANY CONSTRUCTION LINES OR NODES WHEN COPYING DETAILS OVER
				)
				(std:ObjectRelease (list dbx))
				(list t gdt)
			)
		)
	)
	;; OPENS A SOURCE DRAWING WITHOUT SHOWING IT (OBJECTDBX), READS ITS GEOLOCATION AND, WHEN [ cpy ] IS T, COPIES ITS CONTENTS INTO THIS DRAWING
	;; RETURNS ( T GEOLOCATION ) IF SUCCESSFUL (GEOLOCATION IS NIL IF NOT READ OR NOT USABLE, SEE geo:LastError), ELSE NIL
	;; [ pth ] == FULL PATH OF THE SOURCE DRAWING, [ cpy ] == T TO COPY (X-ARRAY), NIL TO ONLY READ (CIVIL TOPO, REFERENCED INSTEAD)
	
	(defun CreateProjectFolders ( pth lst / )
		(vl-every '(lambda ( sub ) (std:CreateDirectoryPath (strcat pth "\\" sub))) lst)
	)
	;; CREATES THE STANDARD PPP/BOM SUB-DIRECTORIES WITHIN THE PASSED PPP-BOM LOCATION
	;; RETURNS T IF ALL FOLDERS EXIST OR WERE CREATED, ELSE NIL
	
	(defun FlagIgnoredObjects ( lst / ent cnt )
		(regapp "PPPSETUP") ;; RETURNS NIL IF ALREADY REGISTERED, WHICH IS FINE
		(setq cnt 0)
		(foreach obj lst
			(if (and
					(setq ent (vlax-vla-object->ename obj))
					(entmod (append (entget ent) (list (list -3 (list "PPPSETUP" (cons 1000 "IGNORE-OUTSIDE-CONTOURS"))))))
				)
				(progn
					(vl-catch-all-apply 'vla-put-color (list obj 1)) ;; 1 = RED, MAKES IGNORED BLOCKS VISIBLE FOR REVIEW
					(setq cnt (1+ cnt))
				)
			)
		)
		cnt
	)
	;; TAGS EACH OBJECT WITH 'PPPSETUP' XDATA SO LATER STAGES SKIP IT, AND COLORS IT RED FOR REVIEW
	;; OBJECTS KEEP THEIR LAYER SO AREA ASSIGNMENT IS NOT LOST IF THEY ARE RE-INCLUDED LATER
	;; RETURNS THE NUMBER OF OBJECTS FLAGGED (OBJECTS ON LOCKED LAYERS FAIL AND ARE NOT COUNTED)
	
	(defun ValidateDrawingData ( arr / con crv ivl skp zro blk ext pnt obs msg ans )
		(setq
			con (PPP:ContourObjects doc cfg) ;; FROM THE CIVIL TOPO XREF
			crv (car con)
			ivl (PPP:ContourInterval crv) ;; REPORTED, IT LIMITS HOW FINE THE CONTOUR ESTIMATE CAN BE
			skp (cadr con) ;; OBJECTS ON A CONTOUR LAYER BUT NOT A SUPPORTED POLYLINE TYPE
			zro (length (vl-remove-if-not '(lambda ( obj ) (equal 0.0 (std:PolylineElevation obj) 1e-8)) crv))
		)
		(setq blk (vl-remove-if-not '(lambda ( obj ) (PPP:IsArrayBlock obj cfg)) arr))
		(if (setq ext (std:ObjectsExtents crv))
			(foreach obj blk
				(setq pnt (vlax-get obj 'InsertionPoint))
				(if (not (and
							(<= (car (car ext)) (car pnt) (car (cadr ext)))
							(<= (cadr (car ext)) (cadr pnt) (cadr (cadr ext)))
						)
					)
					(setq obs (cons obj obs))
				)
			)
		)
		(cond
			((null crv)
				(prompt (strcat "\nError: No contour polylines were found on layers [" (cdr (assoc "contour_layers" cfg)) "] in the " (caddr con) "."))
				nil
			)
			((= zro (length crv))
				(prompt "\nError: All contours have an elevation of zero. The Civil topo file appears to be flattened.")
				nil
			)
			((null blk)
				(prompt
					(strcat
						"\nError: None of the selected array blocks were found on layers [" (cdr (assoc "array_layers" cfg)) "] in the X-ARRAY file."
					)
				)
				nil
			)
			((= (length obs) (length blk))
				(prompt "\nError: No array blocks fall within the contour extents. Check that X-ARRAY and the Civil topo file share the same coordinate system and units.")
				nil
			)
			( t
				(prompt
					(strcat
						"\nValidation: " (itoa (length crv)) " contours found in the " (caddr con)
						(if ivl (strcat ", contour interval " (rtos ivl 2 2) " ft") "")
						(if (> zro 0) (strcat ", " (itoa zro) " with zero elevation") "")
						(if (> skp 0) (strcat ", " (itoa skp) " unsupported objects on contour layers ignored") "")
						"."
						"\nValidation: " (itoa (length blk)) " array blocks found"
						(if obs (strcat ", " (itoa (length obs)) " outside the contour extents") "")
						"."
					)
				)
				(setq msg
					(strcat
						"Validation found issues that may affect pile calculations:\n"
						(if (> zro 0) (strcat "\n- " (itoa zro) " of " (itoa (length crv)) " contours have zero elevation") "")
						(if obs (strcat "\n- " (itoa (length obs)) " of " (itoa (length blk)) " array blocks fall outside the contour extents") "")
					)
				)
				(cond
					(obs
						(setq ans
							(wsh:ConfirmationDialog
								(strcat
									msg
									"\n\nYes\t= Continue and include all array blocks"
									"\nNo\t= Continue and ignore array blocks outside the contour area"
									"\nCancel\t= Stop setup"
								)
								"PPP Setup Validation" 0 51 ;; 51 = YES/NO/CANCEL + WARNING
							)
						)
						(cond
							((= ans 6)
								t
							)
							((= ans 7)
								(setq ign (FlagIgnoredObjects obs))
								(prompt (strcat "\nValidation: " (itoa ign) " of " (itoa (length obs)) " array blocks outside the contour area were flagged and will be ignored."))
								(if (< ign (length obs))
									(prompt "\nWarning: Some blocks could not be flagged, check for locked layers.")
								)
								t
							)
						)
					)
					((> zro 0)
						(= 6 (wsh:ConfirmationDialog (strcat msg "\n\nContinue with setup?") "PPP Setup Validation" 0 52)) ;; 52 = YES/NO + WARNING
					)
					( t
						t
					)
				)
			)
		)
	)
	;; CHECKS CONTOURS FOR VALID ELEVATIONS AND CONFIRMS ARRAY BLOCKS ALIGN WITH THE CONTOUR EXTENTS
	;; ONLY BLOCKS PASSING 'PPP:IsArrayBlock' ARE CHECKED, OTHER X-ARRAY CONTENT IS NOT PART OF THE PILE LAYOUT
	;; BLOCKS OUTSIDE THE CONTOURS CAN BE INCLUDED, FLAGGED AS IGNORED (SETS 'ign' TO THE FLAGGED COUNT), OR STOP THE SETUP
	;; [ arr ] == OBJECTS COPIED FROM X-ARRAY, THE CONTOURS ARE READ FROM THE CIVIL TOPO XREF (SEE PPP:ContourObjects)
	;; RETURNS T IF VALIDATION PASSES OR THE USER CHOOSES TO CONTINUE PAST WARNINGS, ELSE NIL
	
	(defun SaveProjectDrawing ( doc pth / err )
		(cond
			((and (findfile pth) (/= 6 (wsh:ConfirmationDialog (strcat "A drawing already exists at:\n\n" pth "\n\nOverwrite it?") "PPP Setup" 0 52)))
				(prompt "\nSave cancelled: existing drawing was not overwritten.")
				nil
			)
			((and (findfile pth) (std:IsFileLocked pth))
				(prompt "\nError: Existing drawing is locked or open by another user.")
				nil
			)
			((vl-catch-all-error-p (setq err (vl-catch-all-apply 'vla-saveas (list doc pth))))
				(prompt (strcat "\nError: Failed to save drawing: " (vl-catch-all-error-message err)))
				nil
			)
			( t
				(prompt (strcat "\nSuccess: Drawing saved to " pth))
				t
			)
		)
	)
	;; SAVES THE ACTIVE DRAWING TO THE PASSED PATH, CONFIRMING WITH THE USER BEFORE OVERWRITING
	;; RETURNS T IF SUCCESSFUL, ELSE NIL
	
	
	(setq doc (vla-get-activedocument (vlax-get-acad-object)))
	(setq rt "C:\\_ACC\\ACCDocs\\ANS Team\\") ;; ROOT PATH
	(setq elc "02 ELECTRICAL\\^DWG\\XREF\\") ;; X-ARRAY.dwg, X-ARRAY - LP.dwg
	(setq cvl "01 CIVIL\\CAD\\XREF\\") ;; V-TOPO*.dwg (EXISTING GRADE CONTOURS), THE CONSISTENT CIVIL XREF LOCATION
	(setq stc "05 STRUCTURAL\\ENGINEERING\\") ;; PPP-BOM FOLDER IS CREATED UNDER THIS LOCATION
	(setq subs (list "1. PVCase" "2. BOM Processor" "3. Wind" "4. COTTzilla")) ;; STANDARD PPP-BOM SUB-DIRECTORIES
	(setq sav "1. PVCase") ;; SUB-DIRECTORY THE SETUP DRAWING IS SAVED TO, MATCHES WHERE THE PVCASE WORKING DRAWING IS SAVED TODAY
	(setq cfn (PPP:SettingsFile)) ;; PROJECT SETTINGS FILE, SAVED IN THE PPP-BOM ROOT
	(setq pfn (PPP:PresetsFile)) ;; TRACKER PRESETS FILE, SAVED IN THE PPP-BOM ROOT
	(setq gfn (PPP:GeoFile)) ;; GEOLOCATION FILE, SAVED IN THE PPP-BOM ROOT
	(setq xfn "X-ARRAY.dwg") ;; DEFAULT ARRAY FILE, USER CONFIRMS OR SELECTS A DIFFERENT FILE
	(setq ius (getvar "INSUNITS")) ;; ORIGINAL UNITS, RESTORED IF THE DRAWING IS NOT SAVED
	
	(setq funcs
		(list
			'std:CreateObjectDBX
			'std:ObjectRelease
			'std:CopyDBXObjects
			'std:FindSubDirectory
			'std:FindNewestFile
			'std:CreateDirectoryPath
			'std:IsFileLocked
			'std:CollectNewObjects
			'std:AssocSet
			'std:FlattenObjects
			'std:PolylineElevation
			'std:ObjectsExtents
			'wsh:ReturnFolder
			'wsh:ConfirmationDialog
			'json:Items
			'json:StringJoin
		)
	) ;; THE SETTINGS DIALOG'S LIBRARY FUNCTIONS ARE LISTED IN PPP:RequiredFunctions
	
	(cond
		((/= (getvar "DWGTITLED") 0) ;; CHECK DRAWING IS NOT SAVED CURRENTLY
			(alert "Function must be run in a clean and unsaved drawing file.")
			;; WE'LL FIX THIS UP TO WORK WITH EXISTING FILES (UPDATED DRAWING) LATER
		)
		((PPP:MissingFunctions funcs)
			nil ;; PPP:MissingFunctions PRINTED THE MISSING NAMES
		)
		;; AN EARLIER RUN THAT STOPPED PART WAY (OR ANYTHING DRAWN BY HAND) WOULD BE MIXED WITH THIS RUN'S X-ARRAY COPY: STACKED TRACKERS GET
		;; TWO TABLE NUMBERS AND DOUBLED PILE LABELS, SO SETUP ONLY STARTS IN AN EMPTY MODEL SPACE
		((< 0 (vla-get-count (vla-get-modelspace doc)))
			(alert
				(strcat
					"PPPSETUP needs an empty drawing, this one already has "
					(itoa (vla-get-count (vla-get-modelspace doc)))
					" objects in model space.\n\nStart a new drawing and run PPPSETUP again."
				)
			)
		)
		((null
				(progn
					(setq cln t) ;; MODEL SPACE IS EMPTY, SO Cleanup MAY ERASE EVERYTHING THIS RUN BRINGS IN
					(setq dir (wsh:ReturnFolder "Select Project Root Location" 512 rt))
				)
			) ;; UP TO THE USER TO SELECT THE APPROPRIATE PROJECT FOLDER ROOT LOCATION
			(prompt "\nNo project folder was selected.")
		)
		
		;; ESTABLISH PPP-BOM FOLDER STRUCTURE
		;; C:\_ACC\ACCDocs\ANS Team\XXXXXX - Client - Project Name\Project Files\02 ENGINEERING\05 STRUCTURAL\ENGINEERING\PPP-BOM
		((null (or (setq stc (std:FindSubDirectory dir stc 3)) (setq stc (wsh:ReturnFolder "Select Structural Engineering Folder Location" 512 dir))))
			(prompt "\nStructural engineering directory was not established.")
		)
		((null (setq ppp (std:CreateDirectoryPath (strcat stc "\\PPP-BOM"))))
			(prompt "\nFailed to create PPP-BOM directory.")
		)
		((null (CreateProjectFolders ppp subs))
			(prompt "\nFailed to create one or more PPP-BOM sub-directories.")
		)
		
		;; TRACKER PRESETS, AN UNREADABLE FILE STOPS SETUP SO IT IS NOT OVERWRITTEN WITH AN EMPTY LIST
		((and (null (setq prs (PPP:LoadPresets (strcat ppp "\\" pfn)))) (findfile (strcat ppp "\\" pfn)))
			(prompt (strcat "\nError: Tracker preset file could not be read, fix or rename it and run setup again:\n  " ppp "\\" pfn))
		)
		
		;; PROJECT SETTINGS, SAVED VALUES FROM A PREVIOUS RUN TAKE PRIORITY OVER DEFAULTS (THE PROJECT NUMBER / NAME DEFAULTS COME FROM THE ROOT FOLDER NAME)
		;; THE DIALOG ONLY SHOWS THE DEFAULT KEYS, SETTINGS ADDED BY HAND (EX. "area_label_layer") ARE KEPT IN THE RETURNED SETTINGS SO THE SAVE KEEPS THEM
		((null
				(progn
					(setq
						lod (PPP:LoadSettings (strcat ppp "\\" cfn))
						dlg (PPP:SettingsDialog lod (json:Items prs) dir nil)
					)
					(if dlg
						(setq
							cfg (car dlg)
							prs (cadr dlg)
						)
					)
					dlg ;; NOT THE SETQ, AN EMPTY PRESET LIST WOULD READ AS CANCELLED
				)
			)
			(prompt "\nSetup was cancelled.")
		)
		((null (PPP:SavePresets (strcat ppp "\\" pfn) prs))
			(prompt (strcat "\nError: Tracker presets could not be saved to " ppp "\\" pfn))
		)
		;; PRESETS ARE SAVED AS SOON AS THE SETTINGS DIALOG IS ACCEPTED SO PRESET WORK IS KEPT IF A LATER STEP STOPS SETUP
		
		;; DRAWING UNITS ARE SET BEFORE ANYTHING IS BROUGHT IN: AN XREF ATTACHED WHILE THE NEW DRAWING IS STILL IN INCHES (THE TEMPLATE DEFAULT) IS
		;; SCALED x12 FROM 0,0,0 AND LANDS MILLIONS OF FEET AWAY (MCCRACKEN, 11-2026), PPP:AttachTopo ALSO FORCES SCALE 1 AFTER ATTACHING
		((null (setvar "INSUNITS" (if (= "US Survey Feet" (cdr (assoc "units" cfg))) 21 2))) ;; 2 = FEET, 21 = US SURVEY FEET
			(prompt "\nFailed to set drawing units.")
		)
		
		;; FIND AND COPY X-ARRAY DATA
		;; C:\_ACC\ACCDocs\ANS Team\XXXXXX - Client - Project Name\Project Files\02 ENGINEERING\02 ELECTRICAL\^DWG\XREF\X-ARRAY.DWG
		;; X-ARRAY.DWG IS THE DEFAULT, BUT SOME PROCESSES USE A DIFFERENT OR SLIGHTLY RENAMED FILE, THE USER CONFIRMS OR SELECTS IT (SEE SourceFile)
		((null (setq xpt (SourceFile elc "X-ARRAY" xfn nil)))
			(prompt "\nX-ARRAY file was not selected.")
		)
		((null
				(progn
					(setq ent (entlast)) ;; MARK THE LAST ENTITY SO COPIED OBJECTS CAN BE COLLECTED
					(setq cpy (ReadSourceDrawing xpt t))
					(setq geo (cadr cpy)) ;; GEOLOCATION FROM THE X-ARRAY FILE, NIL IF IT IS NOT GEOLOCATED
					cpy
				)
			)
			(prompt (strcat "\nFailed to copy the X-ARRAY file: " xpt))
		)
		((null (setq arr (std:CollectNewObjects ent)))
			(prompt "\nNo objects were copied from the X-ARRAY file.")
		)
		
		;; PICK THE TRACKER BLOCKS FROM THE BLOCKS ON THE ARRAY LAYERS, THE EXACT NAMES ARE SAVED SO NOBODY NEEDS TO OPEN X-ARRAY FOR THEM
		;; RUNS AFTER THE PRESETS ARE SET UP, SO BLOCKS WITH A PRESET'S SIZE ARE ALREADY SELECTED
		((null (setq rws (PPP:ScanArrayBlocks doc arr cfg prs)))
			(prompt (strcat "\nError: No blocks were found on the array layers [" (cdr (assoc "array_layers" cfg)) "] in the X-ARRAY file."))
		)
		((null (setq pck (PPP:ArrayBlockPicker rws cfg)))
			(prompt "\nSetup was cancelled at the array block selection.")
		)
		
		;; GROUP BLOCKS (SEVERAL TRACKERS IN ONE BLOCK, EX. MCCRACKEN "12STR A") ARE EXPLODED ONE LEVEL IN THIS WORKING COPY, NEVER STOPS SETUP
		((progn
				(setq cfg (std:AssocSet cfg "array_block_names" (json:StringJoin pck "|")))
				(if (setq grp (PPP:ArrayGroups doc arr cfg))
					(setq
						exg (PPP:ExplodeGroups doc grp cfg)
						arr (std:CollectNewObjects ent) ;; THE EXPLODED GROUPS ARE GONE, THEIR CONTENTS ARE NEW OBJECTS
					)
				)
				nil
			)
		)
		((null (setq flt (std:FlattenObjects arr)))
			(prompt "\nAn error occured while flattening data.")
		)
		;; ARRAYS ARE FLATTENED TO Z = 0, ELEVATIONS WILL BE CALCULATED FROM THE CONTOUR SURFACE IN LATER STAGES
		
		;; REFERENCE THE CIVIL TOPO (EXISTING GRADE CONTOURS) AS AN XREF, ITS CONTOURS ARE READ FROM THE XREF FOR VALIDATION AND THE PPPPILES ESTIMATE
		;; C:\_ACC\ACCDocs\ANS Team\XXXXXX - Client - Project Name\Project Files\02 ENGINEERING\01 CIVIL\CAD\XREF\V-TOPO*.dwg
		;; ONLY V-TOPO FILES ARE OFFERED: C-TOPO IN THE SAME FOLDER IS CIVIL'S PROPOSED SURFACE (CIVIL 3D OBJECTS, PLAIN AUTOCAD CANNOT READ THEM)
		((null (setq tpt (SourceFile cvl "Civil Topo (V-TOPO)" nil "V-TOPO*.dwg")))
			(prompt "\nCivil topo file was not selected.")
		)
		((null (setq tgo (ReadSourceDrawing tpt nil)))
			(prompt (strcat "\nCivil topo file could not be read: " tpt))
		)
		;; BOTH FILES GEOLOCATED: THEY MUST USE THE SAME COORDINATE SYSTEM AND UNIT, OR THE XREF AT 0,0,0 WOULD NOT LINE UP WITH THE ARRAYS
		((null
				(setq gck
					(cond
						((not (and geo (cadr tgo))) (strcat "not checked, " (if geo "the topo file" "the X-ARRAY") " is not geolocated"))
						((geo:SameSystem geo (cadr tgo)) (strcat "matches the X-ARRAY (" (cdr (assoc "code" geo)) ", " (cdr (assoc "unit" geo)) ")"))
					)
				)
			)
			(prompt
				(strcat
					"\nError: The Civil topo and the X-ARRAY use different coordinate systems, they would not line up. Setup stopped."
					"\n  X-ARRAY: " (cdr (assoc "code" geo)) ", " (cdr (assoc "unit" geo))
					"\n  Civil topo: " (cdr (assoc "code" (cadr tgo))) ", " (cdr (assoc "unit" (cadr tgo))) " (" tpt ")"
				)
			)
		)
		((progn
				(if (null geo)
					(setq geo (cadr tgo))
				) ;; FALL BACK TO THE TOPO FILE'S GEOLOCATION
				(= 'STR (type (setq tpx (PPP:AttachTopo doc tpt))))
			)
			(prompt (strcat "\nError: The Civil topo could not be referenced, " tpx))
		)
		((progn
				(setq cfg (PPP:SourceStamp (PPP:SourceStamp cfg "xarray" xpt) "topo" tpt)) ;; PATHS AND DATES FOR THE CHANGE CHECK (SEE PPP:SourceCheck)
				nil
			)
		)
		
		;; VALIDATE, SET UNITS, AND SAVE
		((null (ValidateDrawingData arr))
			(prompt "\nSetup stopped: drawing data did not pass validation.")
		)
		
		;; FIND THE CIVIL GRADING OUTPUT (SLOPE TOOL) FOLDER, NEVER STOPS SETUP, IT MAY NOT EXIST YET AND PPPPILES CAN ASK FOR IT LATER
		((progn
				(if (setq cvo (PPP:FindCivilOutput ppp))
					(setq cfg (std:AssocSet cfg "civil_output" cvo))
				)
				nil
			)
		)
		
		;; SAVE THE GEOLOCATION, NEVER STOPS SETUP, A PROJECT WITHOUT ONE ONLY LOSES LATITUDE / LONGITUDE
		((progn
				(setq gsv (and geo (geo:SaveGeoFile (strcat ppp "\\" gfn) geo)))
				nil
			)
		)
		
		;; ASSIGN ARRAY BLOCKS TO TRACKER PRESETS BEFORE THE DRAWING IS SAVED
		;; THIS STEP NEVER STOPS SETUP, IF IT IS CANCELLED OR FAILS PPPASSIGN CAN BE RUN IN THE SAVED DRAWING
		((progn
				(setq ctx (list (cons "ppp" ppp) (cons "cfg" cfg) (cons "presets" prs) (cons "geo" geo))) ;; PROJECT CONTEXT, SEE PPP:ProjectContext
				(setq asn (vl-catch-all-apply 'PPPAssignMain (list ctx)))
				(cond
					((vl-catch-all-error-p asn)
						(prompt (strcat "\nWarning: PPPAssign stopped with an error: " (vl-catch-all-error-message asn)))
						(setq asn nil)
					)
					(asn
						(setq
							ctx asn ;; NOW CARRIES THE ASSIGNED TRACKERS FOR LATER STAGES
							cfg (cdr (assoc "cfg" asn)) ;; KEEPS ANY EDGE SETTING CHANGES MADE IN THE ASSIGN DIALOG
						)
					)
				)
				nil
			)
		)
		((null (setq svd (SaveProjectDrawing doc (strcat ppp "\\" sav "\\" (cdr (assoc "file_name" cfg)) ".dwg"))))
			(prompt "\nDrawing was not saved.")
		)
		((null (PPP:SaveSettings (strcat ppp "\\" cfn) cfg))
			(prompt "\nWarning: Drawing was saved, but the project settings file could not be written.")
		)
		( t
			(prompt
				(strcat
					"\nSuccess: PPP setup complete."
					"\n  X-ARRAY copied from: " xpt
					"\n  Civil topo referenced as xref " (PPP:TopoName) ": " tpt " (coordinate system " gck ")"
					"\n  Arrays flattened: " (itoa (car flt)) (if (> (cadr flt) 0) (strcat " (" (itoa (cadr flt)) " skipped, check for locked layers)") "")
					(if (and ign (> ign 0)) (strcat "\n  Array blocks ignored (outside contours, shown in red): " (itoa ign)) "")
					"\n  Array block types: " (json:StringJoin pck ", ")
					(if exg (strcat "\n  Group blocks exploded: " (itoa (car exg)) " (" (itoa (cadr exg)) " array blocks)" (if (caddr exg) (strcat ", " (itoa (length (caddr exg))) " mirrored or scaled groups left as they are") "")) "")
					"\n  COTT range: " (rtos (cdr (assoc "cott_min" cfg)) 2 3) " to " (rtos (cdr (assoc "cott_max" cfg)) 2 3) " ft"
					"\n  Max table slope: " (rtos (cdr (assoc "table_slope_deg" cfg)) 2 3) " degrees"
					"\n  Settings saved to: " ppp "\\" cfn
					"\n  Tracker presets: " (itoa (length prs)) " saved to " ppp "\\" pfn
					(cond
						(cvo (strcat "\n  Civil grading output: " cvo))
						((cdr (assoc "civil_output" cfg)) (strcat "\n  Civil grading output (saved earlier): " (cdr (assoc "civil_output" cfg))))
						("\n  Note: Civil grading output folder not found, PPPPILES will ask for it.")
					)
					(cond
						((not geo)
							(strcat "\n  Note: No geolocation saved (" (cond ((and (std:FunctionLoaded 'geo:LastError) (geo:LastError)) (geo:LastError)) ((not (std:FunctionLoaded 'geo:ReadGeoData)) "LIB-GEO not loaded") ("not found")) "), only needed for latitude / longitude")
						)
						(gsv
							(strcat "\n  Geolocation: " (cond ((cdr (assoc "code" geo))) ("unknown")) (if (cdr (assoc "epsg" geo)) (strcat " (EPSG " (cdr (assoc "epsg" geo)) ")") "") " saved to " ppp "\\" gfn)
						)
						( t
							(strcat "\n  Warning: Geolocation was read but could not be saved to " ppp "\\" gfn)
						)
					)
					(if asn
						(strcat
							"\n  Array blocks assigned: " (itoa (cdr (assoc "assigned" (cdr (assoc "assign" asn)))))
							" of " (itoa (cdr (assoc "blocks" (cdr (assoc "assign" asn)))))
							" (" (itoa (cdr (assoc "exterior" (cdr (assoc "assign" asn))))) " exterior)"
						)
						"\n  Array blocks were not assigned, run PPPASSIGN in this drawing."
					)
					(if (= "0" (cdr (assoc "eor_confirmed" cfg))) "\n  Note: COTT range has not been confirmed with the engineer of record." "")
				)
			)
		)
	)
	(Cleanup) ;; ONLY ACTS WHEN THE RUN STOPPED BEFORE THE DRAWING WAS SAVED
	
	(princ)
)



;;; PPP SETTINGS ;;;
;; EDITS THE PROJECT SETTINGS AND TRACKER PRESETS OF AN EXISTING PROJECT, RUN IN THE SAVED PPP SETUP DRAWING
;; SAME DIALOG AS PPPSETUP (SEE PPP:SettingsDialog), THE FIELDS PPPSETUP USED TO BUILD THE DRAWING ARE SHOWN BUT LOCKED:
;;  PROJECT NUMBER / NAME (FROM THE PROJECT FOLDER), DRAWING NAME (ALREADY SAVED), DRAWING UNITS, CONTOUR LAYERS (READ FROM THE CIVIL TOPO XREF)
;; ONLY CHANGED FILES ARE SAVED (PPP-SETTINGS.cfg, PPP-PRESETS.json IN THE PPP-BOM ROOT), THE DRAWING ITSELF IS NOT CHANGED HERE
;;
;; WHAT NEEDS A RERUN:
;;  PPPASSIGN		PRESETS USED IN THE DRAWING WITH NEW GEOMETRY OR DELETED / RENAMED, EXTERIOR ROWS, STAGGER ALLOWANCE, ARRAY LAYERS
;;					OFFERED RIGHT AFTER THE SAVE (YES / NO), ITS "NOTHING CHANGED" CHECK COMPARES PRESET GEOMETRY, SO A RERUN IS ALWAYS SAFE
;;  PPPPILES		EVERYTHING PPPASSIGN NEEDS, PLUS BUCKET HEIGHTS, BUCKET STEP, MAX TABLE SLOPE, TOLERANCE, ALLOWED FLOOD, EDGE CLEARANCE,
;;					FLOOD FREEBOARD, COTT TO PILE TOP OFFSETS, ANCHOR PILE
;;  NOTHING		MILESTONE, MANUFACTURER, ARRAY BLOCK PATTERN, DECIMALS, EOR CONFIRMED, PRESETS NOT USED IN THE DRAWING

(defun PPPSettingsMain
	(
		/
		;Functions
		*error*
		SameSetting ShowSetting SamePreset UsedPresets PresetNames
		;Variables
		doc lck lbl ctx ppp cfg prs dlg ncf nps def ost nst chg use add del chn ugeo umis prc asn pil svp svs
		funcs
		;DCL Variables, SET BY PPP:SettingsDialog, LOCAL HERE SO *error* ONLY SEES A DIALOG THAT IS OPEN
		tmp des dch
	)
	
	(defun *error* ( msg / )
		(if (eq (type des) 'FILE)
			(close des)
		) ;; 'des' IS NEVER SET HERE: BY DYNAMIC SCOPE THIS CLOSES A FILE LEFT OPEN BY A HELPER THAT NAMES ITS HANDLE 'des' (EX. json:WriteFile, PPP:SaveSettings)
		(setq dch (PPP:DialogUnload tmp dch)) ;; ALSO DELETES THE TEMPORARY DCL FILE
		(PPP:ErrorReport msg
			(cond
				((and svp svs) "\nPPP settings cancelled, the tracker presets and settings were already saved.")
				(svp "\nPPP settings cancelled, the tracker presets were already saved.")
				(svs "\nPPP settings cancelled, the settings were already saved.")
				("\nPPP settings cancelled, nothing was saved.")
			)
		)
	)
	;; CLEANUP RUNS FOR BOTH ERRORS AND USER CANCELS (SEE PPP:ErrorReport), THE DRAWING IS NOT CHANGED BY THIS COMMAND
	;; ONLY ACTIVE UNTIL PPPASSIGN IS STARTED, PPPASSIGN HAS ITS OWN *error*
	
	(defun SameSetting ( key a b / )
		(cond
			((= key "cott_buckets")
				(equal (std:ParseNumberList a) (std:ParseNumberList b) 1e-9) ;; "6,6.5,7" == "6.0, 6.5, 7.0"
			)
			((and (std:ToNumber a) (std:ToNumber b))
				(equal (std:ToNumber a) (std:ToNumber b) 1e-9) ;; "8" == "8.0"
			)
			( t
				(= (strcase a) (strcase b)) ;; LAYER NAMES AND PATTERNS ARE NOT CASE-SENSITIVE
			)
		)
	)
	;; RETURNS T IF TWO SETTING VALUES (STRINGS) MEAN THE SAME THING, SO RETYPING A NUMBER DIFFERENTLY IS NOT A CHANGE
	
	(defun ShowSetting ( key lst / val )
		(setq val (cdr (assoc key lst)))
		(cond
			((= key "eor_confirmed")
				(if (= "1" val) "yes" "no")
			)
			((= "" val)
				"(blank)"
			)
			( t
				val
			)
		)
	)
	;; RETURNS A SETTING VALUE FOR THE REPORT, [ lst ] == SETTINGS MERGED OVER THE DEFAULTS (EVERY VALUE IS A STRING)
	
	(defun SamePreset ( pre nxt / )
		(and
			(PPP:PresetGeometryEqual pre nxt)
			(vl-every '(lambda ( key ) (= (json:Get pre key) (json:Get nxt key))) (list "Name" "Manufacturer" "Description"))
		)
	)
	;; RETURNS T IF TWO PRESETS ARE THE SAME, PACKS ARE COMPARED PARSED SO A PRESET OPENED IN THE DIALOG ("Modules,6;Motor,1" -> "Modules,6; Motor,1") IS NOT A CHANGE
	
	(defun UsedPresets ( / val out )
		(vlax-for obj (vla-get-modelspace doc)
			(if (and
					(= "AcDbBlockReference" (vla-get-objectname obj))
					(setq val (PPP:AssignValues (vlax-vla-object->ename obj)))
					(= 'STR (type (car val)))
				)
				(setq out (std:AssocIncrement out (car val)))
			)
		)
		out
	)
	;; RETURNS ( ( PRESET-NAME . BLOCK-COUNT ) ... ) FOR THE PRESETS ASSIGNED TO ARRAY BLOCKS IN THIS DRAWING ('PPPASSIGN' XDATA)
	
	(defun PresetNames ( lst ref / )
		(vl-remove-if '(lambda ( nam ) (PPP:FindPreset ref nam)) (mapcar '(lambda ( pre ) (json:Get pre "Name")) lst))
	)
	;; RETURNS THE NAMES OF THE PRESETS IN [ lst ] THAT ARE NOT IN [ ref ] (NOT CASE-SENSITIVE)
	
	
	(setq doc (vla-get-activedocument (vlax-get-acad-object)))
	(setq lck (list "project_number" "project_name" "file_name" "units" "contour_layers")) ;; SET BY PPPSETUP, SHOWN BUT LOCKED
	(setq lbl
		(list
			(cons "milestone" "Milestone")
			(cons "array_layers" "Array layers")
			(cons "array_blocks" "Array block pattern")
			(cons "manufacturer" "Manufacturer")
			(cons "exterior_rows" "Exterior rows per side")
			(cons "stagger_allowance" "Stagger allowance (ft)")
			(cons "table_slope" "Max table slope (%)")
			(cons "cott_buckets" "Bucket heights (ft)")
			(cons "bucket_step" "Bucket step (ft)")
			(cons "tolerance" "Tolerance (ft)")
			(cons "allowed_flood" "Allowed flood (ft)")
			(cons "decimals" "Decimals")
			(cons "eor_confirmed" "COTT range confirmed with EOR")
			(cons "edge_clearance" "Edge clearance (ft)")
			(cons "flood_freeboard" "Flood freeboard (ft)")
			(cons "cott_reveal_offset" "COTT to pile top, array (ft)")
			(cons "cott_reveal_offset_motor" "COTT to pile top, motor (ft)")
			(cons "anchor_pile" "Anchor pile")
		)
	) ;; EVERY SETTING THE DIALOG CAN CHANGE AND ITS REPORT LABEL, THE LOCKED KEYS ABOVE CANNOT CHANGE
	
	(setq funcs
		(list
			'std:AssocMerge
			'std:AssocIncrement
			'std:ParseNumberList
			'wsh:ConfirmationDialog
			'json:Get
			'json:StringJoin
		)
	) ;; THE SETTINGS DIALOG'S LIBRARY FUNCTIONS ARE LISTED IN PPP:RequiredFunctions
	
	(cond
		((PPP:MissingFunctions funcs)
			nil ;; PPP:MissingFunctions PRINTED THE MISSING NAMES
		)
		((null (setq ctx (PPP:ProjectContext)))
			nil ;; PPP:ProjectContext PRINTED THE REASON
		)
		((null
				(setq
					ppp (cdr (assoc "ppp" ctx))
					cfg (cdr (assoc "cfg" ctx))
					prs (cdr (assoc "presets" ctx))
					dlg (PPP:SettingsDialog cfg prs nil lck)
				)
			)
			(prompt "\nPPP settings cancelled, nothing was saved.")
		)
	
		;; WHAT CHANGED, PRESETS ARE COMPARED BY NAME AND CONTENT (THE DIALOG SORTS THEM, SO THE ORDER IS NOT A CHANGE)
		((progn
				(setq
					ncf (car dlg)
					nps (cadr dlg)
					def (PPP:DefaultSettings nil)
					ost (std:AssocMerge cfg def) ;; OLD AND NEW SETTINGS OVER THE DEFAULTS, EVERY VALUE IS A STRING
					nst (std:AssocMerge ncf def)
					chg (vl-remove-if '(lambda ( itm ) (SameSetting (car itm) (cdr (assoc (car itm) ost)) (cdr (assoc (car itm) nst)))) lbl)
					add (PresetNames nps prs)
					del (PresetNames prs nps)
					chn
						(vl-remove-if-not
							'(lambda ( pre / nxt )
								(and
									(setq nxt (PPP:FindPreset nps (json:Get pre "Name")))
									(not (SamePreset pre nxt))
								)
							)
							prs
						)
					prc (or add del chn)
					use (if prc (UsedPresets)) ;; ONLY NEEDED WHEN A PRESET CHANGED
					umis (vl-remove-if '(lambda ( itm ) (PPP:FindPreset nps (car itm))) use)
					ugeo
						(vl-remove-if-not
							'(lambda ( itm / pre nxt )
								(and
									(setq pre (PPP:FindPreset prs (car itm)))
									(setq nxt (PPP:FindPreset nps (car itm)))
									(not (PPP:PresetGeometryEqual pre nxt))
								)
							)
							use
						)
				)
				nil
			)
		)
		((and (null chg) (not prc))
			(prompt "\nPPP settings: nothing changed, the project files were left as they are.")
		)
		((and
				umis
				(/= 6
					(wsh:ConfirmationDialog
						(strcat
							"Array blocks in this drawing are assigned to presets that no longer exist:\n"
							(apply 'strcat
								(mapcar
									'(lambda ( itm ) (strcat "\n  " (car itm) " (" (itoa (cdr itm)) " block" (if (= 1 (cdr itm)) "" "s") ")"))
									(if (> (length umis) 10) (reverse (member (nth 9 umis) (reverse umis))) umis) ;; FIRST 10, PRESET NAMES ARE UNIQUE
								)
							)
							(if (> (length umis) 10) "\n  ..." "")
							"\n\nPPPASSIGN will ask for a tracker for those block types. Save anyway?"
						)
						"PPP Settings" 0 52 ;; 52 = YES/NO + WARNING
					)
				)
			)
			(prompt "\nPPP settings cancelled, nothing was saved.")
		)
		((and prc (null (setq svp (PPP:SavePresets (strcat ppp "\\" (PPP:PresetsFile)) nps))))
			(prompt (strcat "\nError: Tracker presets could not be saved to " ppp "\\" (PPP:PresetsFile) ", nothing was saved."))
		)
		((and chg (null (setq svs (PPP:SaveSettings (strcat ppp "\\" (PPP:SettingsFile)) ncf))))
			(prompt
				(strcat
					"\nError: Project settings could not be saved to " ppp "\\" (PPP:SettingsFile)
					(if svp ", the tracker presets were saved." ".")
				)
			)
		)
	
		;; REPORT, THEN OFFER PPPASSIGN WHEN THE ARRAY BLOCK ASSIGNMENTS ARE AFFECTED
		( t
			(setq
				asn (or umis ugeo (vl-some '(lambda ( itm ) (member (car itm) (list "exterior_rows" "stagger_allowance" "array_layers"))) chg))
				pil (or asn (vl-some '(lambda ( itm ) (member (car itm) (list "cott_buckets" "bucket_step" "table_slope" "tolerance" "allowed_flood" "edge_clearance" "flood_freeboard" "cott_reveal_offset" "cott_reveal_offset_motor" "anchor_pile"))) chg))
			)
			(prompt
				(strcat
					"\nPPP settings saved."
					(if chg
						(strcat
							"\n  Settings changed:"
							(apply 'strcat
								(mapcar
									'(lambda ( itm ) (strcat "\n    " (cdr itm) ": " (ShowSetting (car itm) ost) " -> " (ShowSetting (car itm) nst)))
									chg
								)
							)
							"\n  Saved to: " ppp "\\" (PPP:SettingsFile)
						)
						""
					)
					(if prc
						(strcat
							"\n  Tracker presets: " (itoa (length add)) " added, " (itoa (length del)) " deleted, " (itoa (length chn)) " changed"
							"\n  Saved to: " ppp "\\" (PPP:PresetsFile)
						)
						""
					)
					(if ugeo
						(strcat "\n  Presets used in this drawing with new geometry: " (json:StringJoin (mapcar 'car ugeo) ", "))
						""
					)
					(if umis
						(strcat "\n  Presets used in this drawing that no longer exist: " (json:StringJoin (mapcar 'car umis) ", "))
						""
					)
					(cond
						(asn "\n  Run PPPASSIGN to update the array blocks, then PPPPILES to update the pile labels and BOM.")
						(pil "\n  Run PPPPILES to update the pile labels and BOM.")
						("\n  The drawing does not need to be updated.")
					)
					(if (= "0" (cdr (assoc "eor_confirmed" nst))) "\n  Note: COTT range has not been confirmed with the engineer of record." "")
				)
			)
			(if (and
					asn
					(= 6 (wsh:ConfirmationDialog "The array block assignments are affected by these changes.\n\nRun PPPASSIGN now?" "PPP Settings" 0 36)) ;; 36 = YES/NO + QUESTION
				)
				(progn
					(PPPAssignMain nil) ;; RUN AS THE COMMAND WOULD: IT READS THE FILES JUST SAVED, SHOWS ITS DIALOG, AND HANDLES ITS OWN UNDO / ERRORS
					(prompt "\nPPP settings: save the drawing, then run PPPPILES to update the pile labels and BOM.")
				)
			)
		)
	)
	(princ)
)
	;; RUNS THE PPPSETTINGS COMMAND: LOADS THE PROJECT FILES, SHOWS THE SETTINGS DIALOG WITH THE SETUP FIELDS LOCKED, SAVES WHAT CHANGED,
;; REPORTS WHICH COMMANDS NEED A RERUN AND OFFERS TO RUN PPPASSIGN
;; THE SAVED PRESETS ARE NOT COMPARED AGAINST THE DRAWING'S PPP BLOCKS HERE, PPPASSIGN REBUILDS ANY BLOCK WHOSE PRESET GEOMETRY CHANGED



;;; PPP ASSIGN ;;;
;; ASSIGNS EVERY ARRAY BLOCK A TRACKER PRESET AND AN INTERIOR / EXTERIOR ROW TYPE, STORED ON THE BLOCK AS 'PPPASSIGN' XDATA
;; CALLED AT THE END OF PPPSETUP (BEFORE THE DRAWING IS SAVED) AND RUN BY THE USER AS PPPASSIGN FOR REPLACEMENTS / UPDATES
;;
;; BLOCK TYPE -> TRACKER:
;;  EACH ARRAY BLOCK NAME IS ASSIGNED A TRACKER (A PRESET NAME WITHOUT ITS INT- / EXT- PREFIX, EX. "28M56") IN THE ASSIGN DIALOG
;;  THE BLOCK LENGTH GIVES THE MODULE COUNT, THE MOTOR OBJECT IN THE BLOCK (PVCASE LAYER E-TRACKER-MOTOR) PICKS BETWEEN
;;  TRACKERS WITH THE SAME LENGTH (EX. 28M56 / 56M28), ANYTHING NOT DETECTED IS CHOSEN BY THE USER IN THE ASSIGN DIALOG
;;  BLOCK TYPE == ELECTRICAL BLOCK NAME + ORIENTATION, INSERTS ROTATED 180 ARE THEIR OWN TYPE (EX. "QCELLS 30 MOD 1 STR <rotated 180>") SO THE MOTOR END IS RIGHT
;;  CHOICES ARE SAVED TO PPP-ASSIGN.json IN THE PPP-BOM ROOT AND PRE-FILLED THE NEXT TIME
;;
;; ARRAY BLOCKS:
;;  FOUND BY THE BLOCK NAMES PICKED IN THE ARRAY BLOCKS DIALOG (PPPSETUP, SEE PPP:ArrayBlockPicker), THE DIALOG IS SHOWN HERE WHEN NOTHING MATCHES
;;  GROUP BLOCKS HOLDING ARRAY BLOCKS (EX. MCCRACKEN "12STR A") ARE EXPLODED ONE LEVEL FIRST, THE PPPASSIGN COMMAND ASKS (SEE PPP:ExplodeGroups)
;;  FROM PPPSETUP THE DIALOG IS SKIPPED WHEN EVERY BLOCK TYPE IS DETECTED (OR SAVED) WITHOUT A CONFLICT, THE PPPASSIGN COMMAND ALWAYS SHOWS IT
;;
;; INTERIOR / EXTERIOR:
;;  THE FIRST N TRACKERS FROM THE EAST AND WEST SIDES OF EACH CONTINUOUS GROUP ARE EXTERIOR (N == "exterior_rows"), NORTH / SOUTH ENDS DO NOT COUNT
;;  A TRACKER IS CHECKED ALONG ITS WHOLE LENGTH, SO A PARTLY EXPOSED TRACKER (EX. BESIDE A SHORTER NEIGHBOR) IS STILL EXTERIOR
;;  NEIGHBOR ENDS OFFSET BY NO MORE THAN "stagger_allowance" ARE TREATED AS LINED UP (STAGGERED / DIAGONAL LAYOUTS)
;;  NEIGHBORS ARE TRACKERS IN THE NEXT COLUMN WITHIN 1.5 x THE ROW PITCH, THE PITCH IS DETECTED FROM THE DRAWING
;;  !REVISIT! THE NEIGHBOR DISTANCE RULE (ROADS, PADS, MISSING ROWS) STILL NEEDS TO BE CONFIRMED WITH THE TEAM
;;
;; 'PPPASSIGN' XDATA (ALL 1000 STRINGS, IN ORDER):
;;  PRESET NAME ("INT-28M56"), ROW TYPE ("interior" / "exterior", MATCHES THE BOM 'Row type'), EXPOSED SIDES ("W", "E", "WE", "NONE"),
;;  EXPOSED ENDS ("N", "S", "NS", "NONE"), ORIGINAL INVERTER LAYER, ORIGINAL (ELECTRICAL) BLOCK NAME
;;  BLOCKS THAT ARE IGNORED, UNSUPPORTED, OR LEFT UNASSIGNED HAVE ANY OLD 'PPPASSIGN' XDATA REMOVED (A REPLACED BLOCK IS PUT BACK FIRST)
;;
;; PPP ARRAY BLOCKS:
;;  EVERY ASSIGNED INSERT IS SWAPPED IN PLACE TO A BLOCK BUILT FROM ITS PRESET ("PPP-INT-28M56"), ON PPP-ARRAY-INT (CYAN) OR PPP-ARRAY-EXT (MAGENTA)
;;  THE INSERT KEEPS ITS HANDLE, XDATA, AND INSERTION POINT, SO PPPPILES PLACES THE SAME PILES; RERUNS FIND REPLACED BLOCKS BY THEIR XDATA
;;
;; NOTHING CHANGED:
;;  AFTER A RUN THE INPUTS ARE SAVED IN THE DRAWING (LDATA "PPPASSIGN" / "State"), A RUN WITH THE SAME INPUTS, EVERY BLOCK ALREADY
;;  MAPPED AND REPLACED, STOPS WITHOUT CHANGING ANYTHING (EDGE SETTINGS, BLOCK TYPE TRACKERS, PRESET GEOMETRY, BLOCK COUNTS AND POSITIONS)

(defun PPPAssignMain
	(
		ctx
		/
		;Functions
		*error*
		SetSetting IsIgnored TrackerPairs FindPair MotorText TrackerLabel
		LoadAssignFile SaveAssignFile SavedTracker TypeSource ModelObjects ArrayFound PickBlocks GroupBlocks CollectArrayBlocks FlipNorth SuggestTracker
		AssignDialog ClassifyTrackers EndExposure WriteAssignment ClearAssignment MeasureBlock
		IsPPPBlock BlockName RowLayer EnsureLayers BlockSignature DrawRect DrawTracker BuildBlock SwapBlock RestoreBlock
		AssignState AllMapped
		;Variables
		ppp cfg prs sta doc und afn funcs pairs col typs sugs map sel skp nrw alw nmp cls
		trk sid pre row cnt ext una fal stats svm svc trks out exs nxp sxp blks bmp swp sfl ste sto msl exg plc
		;DCL Functions
		DCL:BlockRow DCL:ListRefresh DCL:Mismatch DCL:ShowBlock DCL:BlockSelect DCL:TrackerSelect DCL:Summary
		DCL:Show DCL:ShowExample DCL:Accept
		;DCL Variables
		ttl dcl tmp des dch res idx opt ers est
	)
	
	(defun *error* ( msg / )
		(if (eq (type des) 'FILE)
			(close des)
		) ;; 'des' IS NEVER SET HERE: BY DYNAMIC SCOPE THIS CLOSES A FILE LEFT OPEN BY A HELPER THAT NAMES ITS HANDLE 'des' (EX. json:ReadFile, json:WriteFile, PPP:DialogLoad) WHEN AN ERROR OR ESC HITS PART WAY
		(setq dch (PPP:DialogUnload tmp dch)) ;; ALSO DELETES THE TEMPORARY DCL FILE
		(if plc
			(vl-catch-all-apply 'PPP:RestoreTrackers (list plc nil))
		) ;; TRACKERS FLATTENED AT THE START GO BACK TO THEIR LAST PPPPILES HEIGHTS
		(if und
			(vla-endundomark doc)
		) ;; CLOSES THE UNDO GROUP SO A SINGLE UNDO REVERTS ANY BLOCKS ALREADY UPDATED
		(PPP:ErrorReport msg "\nPPP assign cancelled, use UNDO to revert any blocks already updated.")
	)
	;; ONLY ACTIVE WHEN RUN AS THE PPPASSIGN COMMAND, PPPSETUP CALLS THIS FUNCTION THROUGH VL-CATCH-ALL-APPLY SO AN ERROR HERE DOES NOT STOP SETUP
	;; THE DIALOG IS LOADED AND UNLOADED WITHIN AssignDialog, SO NO DIALOG IS LEFT OPEN WHEN BLOCKS ARE BEING UPDATED
	
	
	
	;; PROJECT DATA ;;
	
	(defun SetSetting ( key val / )
		(setq cfg (std:AssocSet cfg key val))
	)
	;; SETS A PROJECT SETTING IN 'cfg', MISSING KEYS ARE ADDED AT THE END
	
	(defun IsIgnored ( ent / )
		(and (member (cons 1000 "IGNORE-OUTSIDE-CONTOURS") (PPP:XdataValues ent "PPPSETUP")) t)
	)
	;; RETURNS T IF PPPSETUP FLAGGED THE BLOCK AS OUTSIDE THE CONTOURS (SEE FlagIgnoredObjects IN PPPSetupMain), ELSE NIL
	
	
	;; TRACKERS ;;
	
	(defun TrackerPairs ( / nam par lay nmn fnd out )
		(foreach pre prs
			(setq nam (json:Get pre "Name"))
			(if (and
					(= 'STR (type nam))
					(= "INT" (PPP:RowPrefix nam))
					(setq par (PPP:FindPreset prs (PPP:PairName nam)))
					(null (PPP:PresetProblem pre))
					(null (PPP:PresetProblem par))
					(setq lay (PPP:TrackerLayout pre))
				)
				(progn
					(setq nmn 0 fnd nil)
					(foreach pck (PPP:ParsePacks (json:Get pre "Packs"))
						(cond
							(fnd nil)
							((= "Motor" (car pck)) (setq fnd t))
							((= "Modules" (car pck)) (setq nmn (+ nmn (cdr pck))))
						)
					) ;; MODULES NORTH OF THE MOTOR
					(setq out
						(cons
							(list
								(PPP:TrackerName nam) ;; 0 TRACKER NAME, THE PRESET NAME WITHOUT ITS PREFIX (EX. "28M56", "90-45M45")
								(PPP:TubeLength pre) ;; 1 TORQUE TUBE LENGTH, MATCHES THE BLOCK LENGTH
								(if (cadr lay) (+ (json:Get pre "PilingOverhangLeftSize") (cadr lay))) ;; 2 MOTOR GAP CENTER FROM THE NORTH OVERHANG LINE, NIL IF NO MOTOR
								(json:Get pre "Columns") ;; 3 MODULES
								(json:Get pre "SizeY") ;; 4 MODULE WIDTH (N-S)
								(json:Get pre "MotorGapSize") ;; 5 MOTOR GAP
								nam ;; 6 INTERIOR PRESET NAME
								(json:Get par "Name") ;; 7 EXTERIOR PRESET NAME
								nmn ;; 8 MODULES NORTH OF THE MOTOR
							)
							out
						)
					)
				)
			)
		)
		(vl-sort out '(lambda ( a b ) (< (strcase (car a)) (strcase (car b)))))
	)
	;; RETURNS THE TRACKERS THAT CAN BE ASSIGNED: VALID INT- PRESETS WITH A VALID EXT- PAIR, SORTED BY NAME
	;; EACH ITEM IS ( NAME LENGTH MOTOR MODULES MODULE-WIDTH MOTOR-GAP INT-NAME EXT-NAME MODULES-NORTH-OF-MOTOR ), SEE THE INDEX NOTES ABOVE
	
	(defun FindPair ( nam / )
		(if (= 'STR (type nam))
			(vl-some '(lambda ( trk ) (if (= (strcase (car trk)) (strcase nam)) trk)) pairs)
		)
	)
	;; RETURNS THE TRACKER ITEM FROM 'pairs' WITH THE PASSED NAME (NOT CASE-SENSITIVE), ELSE NIL
	
	(defun MotorText ( trk / )
		(cond
			((null (nth 2 trk)) "no motor")
			((= 0 (nth 8 trk)) "motor at the NORTH end")
			((= (fix (nth 3 trk)) (nth 8 trk)) "motor at the SOUTH end")
			( t (strcat "motor " (itoa (nth 8 trk)) " modules from the north end"))
		)
	)
	;; DESCRIBES WHERE A TRACKER'S MOTOR IS, EX. "motor at the SOUTH end"
	
	(defun TrackerLabel ( trk / )
		(strcat (car trk) "  -  " (itoa (fix (nth 3 trk))) " modules, " (MotorText trk))
	)
	;; TRACKER NAME AND DESCRIPTION FOR THE DIALOG AND COMMAND LINE, EX. "28M0  -  28 modules, motor at the SOUTH end"
	
	(defun LoadAssignFile ( pth / dat blk nam out )
		(if (and (findfile pth) (setq dat (json:ReadFile pth)))
			(foreach itm (json:Items (json:Get dat "Blocks"))
				(if (and (= 'STR (type (setq blk (json:Get itm "Block")))) (= 'STR (type (setq nam (json:Get itm "Tracker")))))
					(setq out (cons (cons blk nam) out))
				)
			)
		)
		(reverse out)
	)
	;; READS SAVED BLOCK ASSIGNMENTS AS A LIST OF ( "BLOCK NAME" . "TRACKER" ) PAIRS, NIL IF THE FILE IS MISSING OR EMPTY
	
	(defun SaveAssignFile ( pth lst / old )
		(setq old
			(vl-remove-if
				'(lambda ( itm ) (vl-some '(lambda ( cur ) (= (strcase (car cur)) (strcase (car itm)))) lst))
				map
			)
		) ;; ENTRIES FOR BLOCK NAMES NOT IN THIS DRAWING ARE KEPT, 'map' IS THE FILE AS READ AT THE START OF THE RUN
		(json:WriteFile pth
			(json:Object
				(list
					(cons "Version" 1)
					(cons "Blocks"
						(json:Array
							(mapcar
								'(lambda ( itm ) (json:Object (list (cons "Block" (car itm)) (cons "Tracker" (cdr itm)))))
								(append (vl-remove-if-not 'cdr lst) old) ;; UNASSIGNED BLOCK NAMES CLEAR THEIR SAVED ENTRY
							)
						)
					)
				)
			)
			"\t"
		)
	)
	;; SAVES THE BLOCK ASSIGNMENTS FOR THIS DRAWING [ lst ] == ( "BLOCK NAME" . "TRACKER" OR NIL ) PAIRS
	;; RETURNS T IF SUCCESSFUL, ELSE NIL (SEE json:LastError)
	
	(defun SavedTracker ( nam / )
		(vl-some '(lambda ( itm ) (if (= (strcase (car itm)) (strcase nam)) (cdr itm))) map)
	)
	;; RETURNS THE SAVED TRACKER NAME FOR A BLOCK NAME FROM PPP-ASSIGN.json, ELSE NIL
	
	(defun TypeSource ( pos / nam sug sav )
		(setq
			nam (nth pos sel)
			sug (nth pos sugs)
			sav (SavedTracker (car (nth pos typs)))
		)
		(cond
			((null nam) "not assigned")
			((and (caddr sug) (= (strcase nam) (strcase (car sug)))) "detected")
			((and sav (= (strcase nam) (strcase sav))) "saved")
			( t "chosen")
		)
	)
	;; RETURNS HOW A BLOCK TYPE'S TRACKER WAS SET: "detected", "saved", "chosen", OR "not assigned"
	
	
	;; ARRAY BLOCKS ;;
	
	(defun MeasureBlock ( nam / dfd )
		(PPP:DebugStep "PPPASSIGN" (strcat "reading block definition: " nam))
		(setq dfd (vl-catch-all-apply 'PPP:DefinitionData (list doc nam)))
		(if (vl-catch-all-error-p dfd)
			(setq dfd nil)
		)
		(PPP:DebugStep "PPPASSIGN" (if dfd (strcat "  motors found: " (itoa (length (nth 3 dfd)))) "  not measurable"))
		dfd
	)
	;; MEASURES A BLOCK DEFINITION (SEE PPP:DefinitionData), NIL IF IT HAS NO MEASURABLE OBJECTS OR CANNOT BE READ
	
	(defun ModelObjects ( / out )
		(vlax-for obj (vla-get-modelspace doc)
			(setq out (cons obj out))
		)
		(reverse out)
	)
	;; RETURNS EVERY MODEL SPACE OBJECT
	
	(defun ArrayFound ( lst / )
		(or
			(vl-some
				'(lambda ( obj )
					(or
						(PPP:IsArrayBlock obj cfg)
						(and (IsPPPBlock obj) (PPP:AssignValues (vlax-vla-object->ename obj)))
					)
				)
				lst
			)
			(PPP:ArrayGroups doc lst cfg)
		)
	)
	;; RETURNS NON-NIL IF THE ARRAY BLOCK NAMES FIND ANYTHING: AN ARRAY BLOCK, A BLOCK AN EARLIER RUN REPLACED, OR A GROUP HOLDING ARRAY BLOCKS
	
	(defun PickBlocks ( lst / rws pck )
		(prompt "\nNo array blocks match the array block names, select them from the blocks on the array layers.")
		(cond
			((null (setq rws (PPP:ScanArrayBlocks doc lst cfg prs)))
				(prompt (strcat "\nError: No blocks were found on the array layers [" (cdr (assoc "array_layers" cfg)) "]."))
				nil
			)
			((null (setq pck (PPP:ArrayBlockPicker rws cfg)))
				(prompt "\nPPP assign was cancelled, no blocks were changed.")
				nil
			)
			( t
				(SetSetting "array_block_names" (json:StringJoin pck "|"))
				(if (and sta (not (PPP:SaveSettings (strcat ppp "\\" (PPP:SettingsFile)) cfg)))
					(prompt "\nWarning: The array block names could not be saved to the project settings file.")
				) ;; PPPSETUP SAVES THE RETURNED SETTINGS ITSELF
				t
			)
		)
	)
	;; SHOWS THE ARRAY BLOCKS DIALOG (SEE PPP:ArrayBlockPicker) AND SAVES THE PICKED NAMES, FOR PROJECTS SET UP BEFORE THE DIALOG
	;; OR WHEN THE SAVED NAMES NO LONGER MATCH THE DRAWING. RETURNS T IF BLOCKS WERE PICKED, NIL IF CANCELLED (THE REASON IS PRINTED)
	
	(defun GroupBlocks ( lst / grp )
		(cond
			((null (setq grp (PPP:ArrayGroups doc lst cfg)))
				nil
			)
			((and
					sta
					(/= 6
						(wsh:ConfirmationDialog
							(strcat
								(itoa (length grp)) " group blocks hold " (itoa (apply '+ (mapcar 'cdr grp))) " array blocks."
								"\n\nExplode the groups one level so each array block can be assigned?"
							)
							"PPP Assign" 0 36 ;; 36 = YES/NO + QUESTION
						)
					)
				)
				(prompt "\nWarning: Group blocks were left as they are, the array blocks inside them are not assigned.")
				nil
			)
			( t
				(if (and sta (not und))
					(progn
						(vla-startundomark doc)
						(setq und t)
					)
				) ;; ONE UNDO REVERTS THE EXPLODE AND THE ASSIGNMENT
				(PPP:ExplodeGroups doc grp cfg)
			)
		)
	)
	;; EXPLODES GROUP BLOCKS ONE LEVEL (SEE PPP:ExplodeGroups), A ONE-TIME STEP FOR DRAWINGS SET UP BEFORE PPPSETUP DID THIS ITSELF
	;; THE PPPASSIGN COMMAND ASKS FIRST, RETURNS THE PPP:ExplodeGroups RESULT, NIL IF THERE ARE NO GROUPS OR THE USER SAID NO
	
	(defun CollectArrayBlocks ( / Measured ent val org nam cur rot ort key sgn dfs gfs dfd gdf ins trk ign uns )
		(defun Measured ( nam / )
			(if (not (assoc nam gfs))
				(setq gfs (cons (cons nam (MeasureBlock nam)) gfs))
			)
			(cdr (assoc nam gfs))
		)
		;; MEASURES A BLOCK DEFINITION ONCE PER NAME (SEE MeasureBlock)
		
		(vlax-for obj (vla-get-modelspace doc)
			(setq
				ent (vlax-vla-object->ename obj)
				val nil
				org nil
			)
			(cond
				((not
						(or
							(PPP:IsArrayBlock obj cfg)
							(and (IsPPPBlock obj) (= 'STR (type (setq org (std:SafeNth (setq val (PPP:AssignValues ent)) 5)))))
						)
					)
					nil
				) ;; ELECTRICAL ARRAY BLOCKS, AND INSERTS AN EARLIER RUN REPLACED (THEIR XDATA KEEPS THE ELECTRICAL BLOCK NAME AND ROTATION)
				((IsIgnored ent)
					(setq ign (cons ent ign))
				)
				((not
						(and
							(equal 0.0 (sin (setq rot (vla-get-rotation obj))) 1e-6) ;; 0 OR 180 DEGREES (MCCRACKEN 30 MODULE BLOCKS ARE ROTATED 180)
							(equal 1.0 (vla-get-xscalefactor obj) 1e-6)
							(equal 1.0 (vla-get-yscalefactor obj) 1e-6)
						)
					)
					(setq uns (cons ent uns)) ;; ONLY NORTH-SOUTH TRACKERS AT UNIT SCALE ARE SUPPORTED. !REVISIT! OTHER ROTATIONS (OFF-AXIS TRACKERS) AND MIRRORED BLOCKS
				)
				((not
						(and
							(setq
								cur (vla-get-effectivename obj)
								nam (cond (org) (cur)) ;; ELECTRICAL BLOCK NAME, A REPLACED INSERT STAYS UNDER ITS ELECTRICAL BLOCK NAME
								ort (cond ((null org) rot) ((numberp (std:SafeNth val 7)) (std:SafeNth val 7)) (0.0)) ;; ELECTRICAL ROTATION, A PPP BLOCK IS ALWAYS AT 0
								key (if (< (cos ort) 0) (strcat nam " <rotated 180>") nam) ;; BLOCK TYPE, "<" CANNOT BE PART OF A BLOCK NAME SO THE KEY NEVER CLASHES
							)
							(setq dfd
								(cond
									((assoc key dfs)
										(cdr (assoc key dfs))
									)
									( t
										(setq
											dfd (Measured (if (tblsearch "BLOCK" nam) nam cur))
											dfd (if (and dfd (< (cos ort) 0)) (FlipNorth dfd) dfd)
											dfs (cons (cons key dfd) dfs)
										)
										dfd
									)
								)
							) ;; BLOCK TYPE DATA FOR MATCHING: THE ELECTRICAL DEFINITION (OR THE PPP BLOCK IF IT WAS PURGED), MOTORS MEASURED FROM THE DRAWING'S NORTH
							(setq gdf (Measured cur)) ;; GEOMETRY OF THE DEFINITION THE INSERT USES NOW
						)
					)
					(setq uns (cons ent uns))
				)
				( t
					(setq
						ins (vlax-get obj 'InsertionPoint)
						sgn (if (> (cos rot) 0) 1.0 -1.0) ;; -1 == ROTATED 180, THE DEFINITION'S NORTH EDGE IS THE SOUTH EDGE IN THE DRAWING
					)
					(setq trk
						(cons
							(list
								ent
								key
								(+ (car ins) (* sgn 0.5 (+ (nth 0 (caddr gdf)) (nth 2 (caddr gdf))))) ;; TRACKER CENTER LINE
								(+ (cadr ins) (if (> sgn 0) (nth 1 (caddr gdf)) (- (nth 3 (caddr gdf))))) ;; SOUTH EDGE
								(+ (cadr ins) (if (> sgn 0) (nth 3 (caddr gdf)) (- (nth 1 (caddr gdf))))) ;; NORTH EDGE
								(cadr gdf) ;; WIDTH
							)
							trk
						)
					)
					;; !REVISIT! THE BLOCK'S BOUNDING BOX IS TAKEN AS THE TORQUE TUBE ENDS (VERIFIED ON ORCA FALLS). MCCRACKEN BLOCKS ARE INSERTED AT THE SOUTH-WEST CORNER,
					;; SO THE BOX MATTERS THERE: CHECK THE PPPPILES CIVIL MATCH SUMMARY (AVERAGE NORTH-SOUTH OFFSET) ON THE FIRST MCCRACKEN RUN
				)
			)
		) ;; WALKS MODEL SPACE DIRECTLY LIKE PPPSETUP VALIDATION, SSGET "_X" MISSED ONE BLOCK WHEN RUN FROM PPPSETUP
		(list trk ign uns (vl-remove-if-not 'cdr dfs))
	)
	;; COLLECTS THE ARRAY BLOCKS IN MODEL SPACE: BLOCKS PASSING PPP:IsArrayBlock (ARRAY LAYERS AND BLOCK NAMES) AND REPLACED PPP BLOCKS
	;; BLOCK TYPE == ELECTRICAL BLOCK NAME + ORIENTATION: INSERTS ROTATED 180 ARE THEIR OWN TYPE ("NAME <rotated 180>"), SO THE MOTOR END IS
	;; DETECTED FOR EACH ORIENTATION (MCCRACKEN: 110 OF 112 "QCELLS 30 MOD 1 STR" ARE ROTATED 180), A REPLACED INSERT USES ITS XDATA ROTATION
	;; A REPLACED BLOCK'S GEOMETRY COMES FROM THE PPP BLOCK IT USES NOW
	;; RETURNS ( TRACKERS IGNORED UNSUPPORTED DEFINITIONS )
	;; TRACKERS == ( ENAME BLOCK-TYPE X SOUTH NORTH WIDTH ) ITEMS, IGNORED / UNSUPPORTED == ENAME LISTS,
	;; DEFINITIONS == ( BLOCK-TYPE . PPP:DefinitionData ) PAIRS, MOTORS MEASURED FROM THE NORTH EDGE IN THE DRAWING (SEE FlipNorth)
	
	(defun FlipNorth ( dfd / )
		(list
			(car dfd)
			(cadr dfd)
			(caddr dfd)
			(reverse (mapcar '(lambda ( mot ) (- (car dfd) mot)) (nth 3 dfd)))
		)
	)
	;; RETURNS PPP:DefinitionData FOR A BLOCK ROTATED 180: MOTORS MEASURED FROM THE OTHER END (LENGTH - DISTANCE), STILL NORTH TO SOUTH
	;; LENGTH, WIDTH, AND EXTENTS (BLOCK COORDINATES) ARE UNCHANGED
	
	(defun SuggestTracker ( dfd / mot cnd hit )
		(setq
			mot (nth 3 dfd)
			cnd (vl-remove-if-not '(lambda ( trk ) (<= (abs (- (nth 1 trk) (car dfd))) (* 0.5 (nth 4 trk)))) pairs) ;; SAME LENGTH WITHIN HALF A MODULE
			hit
				(vl-remove-if-not
					'(lambda ( trk )
						(if mot
							(and (nth 2 trk) (= 1 (length mot)) (<= (abs (- (nth 2 trk) (car mot))) 0.5))
							(null (nth 2 trk))
						)
					)
					cnd
				)
		)
		(cond
			((null cnd)
				(list nil "No tracker preset matches this block length." nil)
			)
			((= 1 (length hit))
				(list
					(car (car hit))
					(if mot
						(strcat "Detected from the block: " (MotorText (car hit)) " (" (rtos (car mot) 2 3) " ft from the north edge).")
						"Detected from the block: no motor."
					)
					t
				)
			)
			(mot
				(list nil
					(strcat
						"Motor found " (json:StringJoin (mapcar '(lambda ( val ) (rtos val 2 3)) mot) ", ")
						" ft from the north edge, no single tracker of this length matches. Use Show in drawing, then choose."
					)
					nil
				)
			)
			((= 1 (length cnd))
				(list (car (car cnd)) "Suggested from the block length only, no motor found in the block." nil)
			)
			( t
				(list nil "No motor found in the block. Use Show in drawing to see which end the motor is on, then choose." nil)
			)
		)
	)
	;; SUGGESTS A TRACKER FOR A BLOCK DEFINITION [ dfd ] (SEE PPP:DefinitionData)
	;; RETURNS ( TRACKER-NAME-OR-NIL HINT-MESSAGE DETECTED ), DETECTED IS T ONLY WHEN THE LENGTH AND MOTOR POSITION BOTH MATCH ONE TRACKER
	
	
	;; DIALOG ;;
	
	(defun AssignDialog ( / sts dlg )
		(setq ttl "PPP_Assign")
		(setq dcl
			(list
				"// Temporary DCL file;"
				(strcat ttl " : dialog {")
				"	label = \"Pile Plot Plan Assign - Array Blocks to Tracker Presets\";"
				"	: boxed_column {"
				"		label = \"Array block types\";"
				"		: list_box { label = \"Block name / Blocks / Tracker / Set by\"; key = \"block_list\"; width = 96; height = 12; tabs = \"46 54 64\"; }"
				"		: row {"
				"			: popup_list { label = \"Tracker preset:\"; key = \"tracker\"; edit_width = 56; }"
				"			: button { label = \"Show in drawing\"; key = \"show\"; width = 18; fixed_width = true; }"
				"		}"
				"		: text { key = \"block_info\"; width = 96; }"
				"		: text { key = \"block_hint\"; width = 96; }"
				"	}"
				"	: boxed_column {"
				"		label = \"Edge classification\";"
				"		: row {"
				"			: edit_box { label = \"Exterior rows per side:\"; key = \"exterior_rows\"; edit_width = 8; }"
				"			: edit_box { label = \"Stagger allowance (ft):\"; key = \"stagger_allowance\"; edit_width = 8; }"
				"		}"
				"		: text { label = \"Exterior = first trackers in from the east and west sides of each group, north and south ends do not count.\"; }"
				"		: text { label = \"Stagger allowance = end offset allowed between neighboring trackers before the exposed part is an edge.\"; }"
				"	}"
				"	: text { key = \"summary\"; width = 96; }"
				"	spacer_1;"
				"	errtile;"
				"	ok_cancel;"
				"}"
			)
		)
		
		(defun DCL:BlockRow ( pos / )
			(strcat
				(car (nth pos typs)) "\t"
				(itoa (cadr (nth pos typs))) "\t"
				(cond ((nth pos sel)) ("(not assigned)")) "\t"
				(if (nth pos sel) (TypeSource pos) "")
			)
		)
		;; RETURNS THE LIST BOX ROW FOR A BLOCK TYPE
		
		(defun DCL:ListRefresh ( / pos )
			(setq pos -1)
			(PPP:FillList "block_list" (mapcar '(lambda ( typ ) (DCL:BlockRow (setq pos (1+ pos)))) typs) idx)
		)
		;; RELOADS THE BLOCK TYPE LIST AND KEEPS THE CURRENT SELECTION
		
		(defun DCL:Mismatch ( pos / trk )
			(and
				(setq trk (FindPair (nth pos sel)))
				(> (abs (- (nth 1 trk) (car (caddr (nth pos typs))))) (* 0.5 (nth 4 trk)))
			)
		)
		;; RETURNS T IF THE ASSIGNED TRACKER LENGTH DIFFERS FROM THE BLOCK LENGTH BY MORE THAN HALF A MODULE
		
		(defun DCL:ShowBlock ( / typ trk sav )
			(setq
				typ (nth idx typs)
				trk (FindPair (nth idx sel))
				sav (SavedTracker (car typ))
				opt (vl-remove-if-not '(lambda ( itm ) (<= (abs (- (nth 1 itm) (car (caddr typ)))) (* 0.5 (nth 4 itm)))) pairs)
			) ;; ONLY TRACKERS WITH THE BLOCK'S LENGTH ARE OFFERED
			(if (null opt)
				(setq opt pairs)
			) ;; NOTHING MATCHES, OFFER EVERY TRACKER
			(if (and trk (not (member trk opt)))
				(setq opt (append opt (list trk)))
			) ;; KEEP A SAVED OR EARLIER CHOICE VISIBLE EVEN IF ITS LENGTH DOES NOT MATCH
			(PPP:FillList "tracker" (cons "(not assigned)" (mapcar 'TrackerLabel opt)) (if trk (1+ (vl-position trk opt)) 0))
			(set_tile "block_info"
				(strcat
					"Block length " (rtos (car (caddr typ)) 2 3) " ft, width " (rtos (cadr (caddr typ)) 2 3) " ft"
					(cond
						((null trk) ".")
						((DCL:Mismatch idx) (strcat ". Tracker length " (rtos (nth 1 trk) 2 3) " ft DOES NOT MATCH the block."))
						( t (strcat ", tracker length " (rtos (nth 1 trk) 2 3) " ft."))
					)
				)
			)
			(set_tile "block_hint"
				(strcat
					(cond
						((and sav trk (= (strcase sav) (strcase (car trk))) (car (nth idx sugs)) (/= (strcase sav) (strcase (car (nth idx sugs)))))
							(strcat "Saved assignment, but the block looks like " (car (nth idx sugs)) ". ")
						) ;; SAVED AND DETECTED DISAGREE
						((and sav trk (= (strcase sav) (strcase (car trk)))) "Saved assignment. ")
						("")
					)
					(cadr (nth idx sugs))
				)
			)
			(DCL:Summary)
		)
		;; DISPLAYS THE SELECTED BLOCK TYPE'S TRACKER CHOICES, MEASUREMENTS, AND HOW IT WAS DETECTED
		
		(defun DCL:BlockSelect ( val / )
			(setq idx (atoi val))
			(DCL:ShowBlock)
		)
		;; BLOCK TYPE LIST SELECTION CHANGED
		
		(defun DCL:TrackerSelect ( val / pos new )
			(setq
				new (if (< 0 (atoi val)) (car (nth (1- (atoi val)) opt)))
				pos -1
				sel (mapcar '(lambda ( itm ) (if (= idx (setq pos (1+ pos))) new itm)) sel)
			)
			(DCL:ListRefresh)
			(DCL:ShowBlock)
		)
		;; TRACKER POPUP CHANGED, UPDATES THE SELECTED BLOCK TYPE
		
		(defun DCL:Summary ( / una blk det pos )
			(setq una 0 blk 0 det 0 pos -1)
			(foreach typ typs
				(setq
					blk (+ blk (cadr typ))
					pos (1+ pos)
				)
				(cond
					((null (nth pos sel)) (setq una (+ una (cadr typ))))
					((= "detected" (TypeSource pos)) (setq det (1+ det)))
				)
			)
			(set_tile "summary"
				(strcat
					(itoa (length typs)) " block types (" (itoa det) " detected), "
					(itoa blk) " array blocks, "
					(if (= 0 una) "all assigned." (strcat (itoa una) " not assigned (they will be skipped)."))
				)
			)
		)
		;; DISPLAYS THE BLOCK COUNTS, HOW MANY TYPES WERE DETECTED, AND HOW MANY BLOCKS ARE NOT ASSIGNED
		
		(defun DCL:Show ( / )
			(setq
				ers (get_tile "exterior_rows")
				est (get_tile "stagger_allowance")
			) ;; KEEP THE EDGE SETTINGS WHILE THE DIALOG IS CLOSED
			(done_dialog 2)
		)
		;; CLOSES THE DIALOG SO ONE BLOCK OF THE SELECTED TYPE CAN BE SHOWN, THE DIALOG REOPENS AFTERWARDS
		
		(defun DCL:ShowExample ( / nam rec acd pad )
			(setq
				nam (car (nth idx typs))
				rec (vl-some '(lambda ( itm ) (if (= nam (cadr itm)) itm)) (car col))
				acd (vlax-get-acad-object)
			)
			(if rec
				(progn
					(setq pad (* 0.1 (- (nth 4 rec) (nth 3 rec))))
					(vla-zoomwindow acd
						(vlax-3d-point (list (- (nth 2 rec) (* 3 pad)) (- (nth 3 rec) pad) 0.0))
						(vlax-3d-point (list (+ (nth 2 rec) (* 3 pad)) (+ (nth 4 rec) pad) 0.0))
					)
					(redraw (car rec) 3) ;; HIGHLIGHT
					(vl-catch-all-apply 'getstring
						(list (strcat "\nShowing one '" nam "' block (highlighted, north is up). Press Enter to return to the assign dialog: "))
					) ;; ESC HERE ONLY RETURNS TO THE DIALOG
					(redraw (car rec) 4)
					(vl-catch-all-apply 'vla-zoomprevious (list acd))
				)
			)
		)
		;; ZOOMS TO ONE BLOCK OF THE SELECTED TYPE AND WAITS FOR THE USER
		
		(defun DCL:Accept ( / rws stg una mis pos msg )
			(setq
				rws (std:ToNumber (get_tile "exterior_rows"))
				stg (std:ToNumber (get_tile "stagger_allowance"))
				una 0
				mis 0
				pos -1
			)
			(foreach typ typs
				(setq pos (1+ pos))
				(cond
					((null (nth pos sel)) (setq una (+ una (cadr typ))))
					((DCL:Mismatch pos) (setq mis (1+ mis)))
				)
			)
			(setq msg
				(strcat
					(if (> una 0) (strcat "\n- " (itoa una) " array blocks are not assigned and will be skipped") "")
					(if (> mis 0) (strcat "\n- " (itoa mis) " block types are assigned a tracker that does not match the block length") "")
				)
			)
			(cond
				((not (and rws (<= 0 rws 99) (equal rws (fix rws) 1e-8)))
					(set_tile "error" "Error: Exterior rows must be a whole number from 0 to 99.")
				)
				((not (and stg (>= stg 0)))
					(set_tile "error" "Error: Stagger allowance must be a number of zero or greater.")
				)
				((and (/= "" msg) (/= 6 (wsh:ConfirmationDialog (strcat "Check the block assignments:\n" msg "\n\nContinue anyway?") "PPP Assign" 0 52)))
					nil ;; 52 = YES/NO + WARNING, THE DIALOG STAYS OPEN ON NO
				)
				( t
					(setq res (list sel (fix rws) stg (vl-string-trim " " (get_tile "exterior_rows")) (vl-string-trim " " (get_tile "stagger_allowance"))))
					(done_dialog 1)
				)
			)
		)
		;; CHECKS THE EDGE SETTINGS AND CONFIRMS UNASSIGNED OR MISMATCHED BLOCK TYPES, SETS 'res' AND CLOSES THE DIALOG IF ACCEPTED
		
		
		(setq
			res nil
			idx (cond ((vl-position nil sel)) (0)) ;; START ON THE FIRST BLOCK TYPE THAT STILL NEEDS A CHOICE
			sts 2
			ers (PPP:SettingText cfg "exterior_rows" "2")
			est (PPP:SettingText cfg "stagger_allowance" "10.0")
		)
		(if (setq dlg (PPP:DialogLoad dcl "PPPASSIGN"))
			(progn
				(setq
					tmp (car dlg)
					dch (cdr dlg)
				)
				(while (= 2 sts) ;; 2 == SHOW IN DRAWING, REOPEN AFTERWARDS
					(if (new_dialog ttl dch)
						(progn
							(DCL:ListRefresh)
							(set_tile "exterior_rows" ers)
							(set_tile "stagger_allowance" est)
							(DCL:ShowBlock)
							
							(action_tile "block_list" "(DCL:BlockSelect $value)")
							(action_tile "tracker" "(DCL:TrackerSelect $value)")
							(action_tile "show" "(DCL:Show)")
							(action_tile "accept" "(DCL:Accept)")
							(action_tile "cancel" "(done_dialog 0)")
							
							(setq sts (start_dialog))
							(if (= 2 sts)
								(DCL:ShowExample)
							)
						)
						(progn
							(prompt "\nError loading dialog box.")
							(setq sts 0)
						)
					)
				)
				(setq dch (PPP:DialogUnload tmp dch))
			)
			(prompt "\nError loading dialog box.")
		)
		(if (= 1 sts) res)
	)
	;; DISPLAYS THE BLOCK TYPE ASSIGNMENT DIALOG, PRE-FILLED WITH 'sel'
	;; RETURNS ( SELECTIONS EXTERIOR-ROWS STAGGER-ALLOWANCE EXTERIOR-ROWS-TEXT STAGGER-ALLOWANCE-TEXT ) IF ACCEPTED, NIL IF CANCELLED
	
	
	;; CLASSIFICATION ;;
	
	(defun ClassifyTrackers ( trk nrw alw / Exposed tol scn srt cnd cnt lst rst a b i j dx mnd pit mxd nbe ord enb wnb )
		(defun Exposed ( pos nbl lo hi lvl / nbs ivs )
			(setq
				nbs (nth pos nbl)
				ivs (list (list lo hi))
			)
			(foreach nbr nbs
				(setq ivs (std:SubtractInterval ivs (- (nth 3 (nth nbr srt)) alw) (+ (nth 4 (nth nbr srt)) alw)))
			)
			(cond
				((vl-some '(lambda ( iv ) (> (- (cadr iv) (car iv)) tol)) ivs)
					t
				)
				((< (1+ lvl) nrw)
					(vl-some
						'(lambda ( nbr / low hgh )
							(setq
								low (max lo (nth 3 (nth nbr srt)))
								hgh (min hi (nth 4 (nth nbr srt)))
							)
							(and (> (- hgh low) tol) (Exposed nbr nbl low hgh (1+ lvl)))
						)
						nbs
					)
				)
			)
		)
		;; RETURNS T IF PART OF TRACKER [ pos ] BETWEEN [ lo ] AND [ hi ] IS WITHIN [ nrw ] TRACKERS OF AN EDGE ON ONE SIDE
		;; [ nbl ] == NEIGHBOR LISTS FOR THE SIDE BEING CHECKED, [ lvl ] == TRACKERS ALREADY PASSED (0 FOR THE TRACKER ITSELF)
		;; THE SPAN NOT COVERED BY A NEIGHBOR (EACH EXTENDED BY THE STAGGER ALLOWANCE) IS EXPOSED, COVERED PARTS ARE CHECKED THROUGH THAT NEIGHBOR
		
		(PPP:DebugStep "PPPASSIGN" (strcat "classify: sorting " (itoa (length trk)) " trackers"))
		(setq
			tol 0.05 ;; FT, OVERLAP TOLERANCE VALIDATED ON ORCA FALLS (KEPT WHEN PPP:PresetTolerance WAS TIGHTENED)
			scn 100.0 ;; FT, EAST-WEST SEARCH DISTANCE USED TO DETECT THE ROW PITCH
			srt (mapcar '(lambda ( pos ) (nth pos trk)) (vl-sort-i (mapcar 'caddr trk) '<)) ;; SORTED WEST TO EAST
			i 0
			lst srt
		)
		(while lst
			(setq
				a (car lst)
				rst (cdr lst)
				j (1+ i)
				mnd nil
			)
			(while (and rst (<= (setq dx (- (caddr (car rst)) (caddr a))) scn))
				(setq b (car rst))
				(if (and
						(> dx (* 0.5 (nth 5 a))) ;; A DIFFERENT COLUMN, NOT A TRACKER NORTH OR SOUTH IN THE SAME COLUMN
						(> (- (min (nth 4 a) (nth 4 b)) (max (nth 3 a) (nth 3 b))) tol) ;; OVERLAPPING NORTH-SOUTH
					)
					(setq
						cnd (cons (list i j dx) cnd)
						mnd (if mnd (min mnd dx) dx)
					)
				)
				(setq
					rst (cdr rst)
					j (1+ j)
				)
			)
			(if mnd
				(setq cnt (std:AssocIncrement cnt (fix (+ 0.5 (* 10.0 mnd))))) ;; COUNT OF EACH NEAREST SPACING, ROUNDED TO 0.1 FT
			)
			(setq
				lst (cdr lst)
				i (1+ i)
			)
		)
		;; EVERY OVERLAPPING PAIR OF TRACKERS WITHIN THE SEARCH DISTANCE, AND A COUNT OF EACH TRACKER'S NEAREST EAST SPACING
		
		(PPP:DebugStep "PPPASSIGN" (strcat "classify: " (itoa (length cnd)) " overlapping pairs, " (itoa (length cnt)) " spacings"))
		(setq
			pit (if cnt (/ (car (car (vl-sort cnt '(lambda ( p q ) (> (cdr p) (cdr q)))))) 10.0)) ;; MOST COMMON SPACING == ROW PITCH
			mxd (if pit (* 1.5 pit) 0.0) ;; !REVISIT! NEIGHBOR DISTANCE RULE, 1.5 x PITCH BREAKS A GROUP AT ANY MISSING ROW
			nbe (reverse (vl-remove-if '(lambda ( itm ) (> (caddr itm) mxd)) cnd)) ;; ( WEST EAST DX ), SORTED BY THE WEST INDEX
			ord (vl-sort-i (mapcar 'cadr nbe) '<)
			enb (std:GroupByIndex (mapcar '(lambda ( itm ) (cons (car itm) (cadr itm))) nbe) (length srt))
			wnb (std:GroupByIndex (mapcar '(lambda ( pos / itm ) (setq itm (nth pos nbe)) (cons (cadr itm) (car itm))) ord) (length srt))
			i -1
		)
		(PPP:DebugStep "PPPASSIGN" (strcat "classify: pitch " (if pit (rtos pit 2 1) "none") ", " (itoa (length nbe)) " neighbor pairs, checking exposure"))
		(list
			srt
			(mapcar
				'(lambda ( itm )
					(setq i (1+ i))
					(if (< 0 nrw)
						(strcat
							(if (Exposed i wnb (nth 3 itm) (nth 4 itm) 0) "W" "")
							(if (Exposed i enb (nth 3 itm) (nth 4 itm) 0) "E" "")
						)
						""
					)
				)
				srt
			)
			pit
			mxd
		)
	)
	;; CLASSIFIES TRACKERS [ trk ] (SEE CollectArrayBlocks) BY THEIR EXPOSED SIDES
	;; [ nrw ] == EXTERIOR ROWS PER SIDE, [ alw ] == STAGGER ALLOWANCE (FT)
	;; RETURNS ( TRACKERS-SORTED-WEST-TO-EAST EXPOSED-SIDES ROW-PITCH NEIGHBOR-DISTANCE ), EXPOSED SIDES ARE "", "W", "E", OR "WE"
	;; NORTH / SOUTH END EXPOSURE (EDGE PILES) IS CHECKED SEPARATELY BY EndExposure
	;; IF NO PITCH IS FOUND (EX. A SINGLE ROW) NO TRACKER HAS NEIGHBORS AND ALL ARE EXTERIOR
	;; VALIDATED AGAINST THE ORCA FALLS PVCASE BOM: ALL 3,304 TRACKERS MATCH WITH 2 EXTERIOR ROWS AND A STAGGER ALLOWANCE OF 2.5 FT OR MORE
	
	(defun EndExposure ( srt pit / FirstN BuildColumns Window lim nmn xt ytl i rcs cols bhd rst cur win out gn gs ExposedEnd )
		(defun FirstN ( lst num / out )
			(while (and lst (> num 0))
				(setq
					out (cons (car lst) out)
					lst (cdr lst)
					num (1- num)
				)
			)
			(reverse out)
		)
		;; RETURNS THE FIRST [ num ] ITEMS OF A LIST
		
		(defun BuildColumns ( rcs / col out )
			(foreach rec rcs
				(if (and col (<= (- (cadr rec) (car col)) xt))
					(setq col (append col (list rec)))
					(progn
						(if col
							(setq out (cons col out))
						)
						(setq col (list (cadr rec) rec))
					)
				)
			)
			(if col
				(setq out (cons col out))
			)
			(reverse out)
		)
		;; GROUPS RECORDS ( INDEX X SOUTH NORTH ... ), SORTED WEST TO EAST, INTO COLUMNS ( FIRST-X RECORD ... )
		;; A COLUMN HOLDS THE TRACKERS WITHIN HALF A ROW PITCH OF ITS FIRST TRACKER (TRACKERS NORTH / SOUTH OF EACH OTHER)
		
		(defun Window ( bhd cur rst num / )
			(apply 'append (mapcar 'cdr (append (reverse (FirstN bhd num)) (list cur) (FirstN rst num))))
		)
		;; RETURNS THE RECORDS IN THE CURRENT COLUMN AND [ num ] COLUMNS ON EACH SIDE
		
		(defun ExposedEnd ( rec win sid / gap end thr mul fnd dir tgx cnd )
			(setq
				gap (if (= sid "N") (nth 4 rec) (nth 5 rec))
				end (if (= sid "N") (nth 3 rec) (nth 2 rec))
			)
			(cond
				((or (null gap) (> gap lim))
					t ;; NO TRACKER FACES THIS END, OR THE GAP IS WIDER THAN THE LIMIT (NX: 60 FT)
				)
				( t
					(setq
						thr (max (+ gap pit) (* nmn pit)) ;; NX: ROAD WIDTH MEASURED IN FROM THE OPENING, AT LEAST THE TWO NEAREST TRACKERS
						mul 1
					)
					(while (and (not fnd) (<= (* mul pit) (+ thr 0.01)))
						(foreach dir (list 1.0 -1.0)
							(setq
								tgx (+ (cadr rec) (* dir mul pit))
								cnd
									(vl-remove-if-not
										'(lambda ( oth )
											(and
												(< (abs (- (cadr oth) tgx)) xt)
												(< (abs (- (if (= sid "N") (nth 3 oth) (nth 2 oth)) end)) ytl)
											)
										)
										win
									)
							)
							(if (not
									(vl-some
										'(lambda ( oth / ogp )
											(setq ogp (if (= sid "N") (nth 4 oth) (nth 5 oth)))
											(and ogp (<= ogp lim))
										)
										cnd
									)
								)
								(setq fnd t) ;; NO SHELTERED TRACKER IN THAT COLUMN ALONG THIS GAP == THE GAP OPENS HERE
							)
						)
						(setq mul (1+ mul))
					)
					fnd
				)
			)
		)
		;; RETURNS T IF THE NORTH ("N") OR SOUTH ("S") END OF RECORD [ rec ] IS EXPOSED (NEXTRACKER PJE-000006 SECTIONS 5.2 AND 9)
		;; RECORD == ( INDEX X SOUTH NORTH NORTH-GAP SOUTH-GAP ), [ win ] == RECORDS IN THE NEARBY COLUMNS
		
		(setq
			lim (PPP:SettingNumber cfg "exposure_gap_limit" 60.0) ;; FT, NX: A GAP OVER 60 FT EXPOSES THE WHOLE ROW
			nmn (PPP:SettingNumber cfg "exposure_min_trackers" 2.0) ;; TRACKERS NEAREST A GAP OPENING THAT ARE ALWAYS EXPOSED
		)
		(cond
			((null pit)
				(mapcar '(lambda ( itm ) "NS") srt) ;; NO ROW PITCH, EVERY TRACKER IS EXTERIOR ANYWAY
			)
			( t
				(setq
					xt (* 0.5 pit)
					ytl (* 2.0 pit) ;; ENDS WITHIN THIS NORTH-SOUTH DISTANCE ARE ON THE SAME GAP
					i -1
					rcs (mapcar '(lambda ( itm ) (list (setq i (1+ i)) (caddr itm) (nth 3 itm) (nth 4 itm))) srt)
					cols (BuildColumns rcs)
					rst cols
				)
				(while rst
					(setq
						cur (car rst)
						rst (cdr rst)
						win (Window bhd cur rst 1)
					)
					(foreach rec (cdr cur)
						(setq gn nil gs nil)
						(foreach oth win
							(if (and (/= (car oth) (car rec)) (< (abs (- (cadr oth) (cadr rec))) xt))
								(progn
									(if (> (nth 2 oth) (- (nth 3 rec) 0.5)) ;; NORTH OF THIS TRACKER
										(setq gn (if gn (min gn (- (nth 2 oth) (nth 3 rec))) (- (nth 2 oth) (nth 3 rec))))
									)
									(if (< (nth 3 oth) (+ (nth 2 rec) 0.5)) ;; SOUTH OF THIS TRACKER
										(setq gs (if gs (min gs (- (nth 2 rec) (nth 3 oth))) (- (nth 2 rec) (nth 3 oth))))
									)
								)
							)
						)
						(setq out (cons (append rec (list (if gn (max gn 0.0)) (if gs (max gs 0.0)))) out))
					)
					(setq bhd (cons cur bhd))
				) ;; PASS 1: NORTH-SOUTH GAP TO THE FACING TRACKER AT EACH END (NIL == NOTHING FACES THAT END)
				(setq
					cols (BuildColumns (reverse out))
					rst cols
					bhd nil
					out nil
				)
				(while rst
					(setq
						cur (car rst)
						rst (cdr rst)
						win (Window bhd cur rst (+ 2 (fix (/ (+ lim pit) pit))))
					)
					(foreach rec (cdr cur)
						(setq out
							(cons
								(cons
									(car rec)
									(strcat (if (ExposedEnd rec win "N") "N" "") (if (ExposedEnd rec win "S") "S" ""))
								)
								out
							)
						)
					)
					(setq bhd (cons cur bhd))
				) ;; PASS 2: AN END WITH A FACING TRACKER IS STILL EXPOSED NEAR WHERE ITS GAP OPENS
				(mapcar 'cdr (vl-sort out '(lambda ( a b ) (< (car a) (car b)))))
			)
		)
	)
	;; CLASSIFIES THE NORTH AND SOUTH ENDS OF EVERY TRACKER AS EXPOSED OR SHELTERED, FOR EDGE AND END PILES
	;; [ srt ] == TRACKERS SORTED WEST TO EAST (FIRST ITEM OF ClassifyTrackers), [ pit ] == ROW PITCH
	;; RETURNS ONE STRING PER TRACKER IN THE SAME ORDER: "N", "S", "NS" (EXPOSED ENDS) OR "" (BOTH SHELTERED)
	;; RULES (NEXTRACKER PJE-000006 REV C, SECTIONS 5.2 AND 9), THRESHOLDS ARE PROJECT SETTINGS:
	;;   AN END WITH NO FACING TRACKER, OR A GAP OVER 60 FT TO IT, IS EXPOSED
	;;   FOR A GAP OF 60 FT OR LESS, ENDS WITHIN THE GAP WIDTH + ONE PITCH (AT LEAST TWO TRACKERS) OF WHERE THE GAP OPENS ARE EXPOSED
	;; VALIDATED ON ORCA FALLS AGAINST THE HAND-DRAWN EXP POLYGONS: 5,477 OF 5,492 INTERIOR TRACKER ENDS AGREE (99.7%), APPROVED
	
	
	;; XDATA ;;
	
	(defun WriteAssignment ( ent pre row sid nse / dxf val lay blk opt ort )
		(setq
			dxf (entget ent)
			val (PPP:AssignValues ent)
			lay (if (= 'STR (type (std:SafeNth val 4))) (std:SafeNth val 4) (cdr (assoc 8 dxf))) ;; KEEP THE ORIGINAL LAYER FROM AN EARLIER RUN
			blk (if (= 'STR (type (std:SafeNth val 5))) (std:SafeNth val 5) (vla-get-effectivename (vlax-ename->vla-object ent))) ;; KEEP THE ELECTRICAL BLOCK NAME
			opt (if (vl-consp (std:SafeNth val 6)) (std:SafeNth val 6) (cdr (assoc 10 dxf))) ;; KEEP THE ELECTRICAL INSERTION POINT
			ort (if (numberp (std:SafeNth val 7)) (std:SafeNth val 7) (cdr (assoc 50 dxf))) ;; KEEP THE ELECTRICAL ROTATION (RADIANS)
		)
		(entmod
			(append
				dxf
				(list
					(list -3
						(list "PPPASSIGN"
							(cons 1000 pre)
							(cons 1000 row)
							(cons 1000 (if (= "" sid) "NONE" sid))
							(cons 1000 (if (= "" nse) "NONE" nse))
							(cons 1000 lay)
							(cons 1000 blk)
							(cons 1010 opt)
							(cons 1040 (cond (ort) (0.0)))
						)
					)
				)
			)
		)
	)
	
	;; WRITES THE 'PPPASSIGN' XDATA TO A BLOCK, OTHER APPLICATIONS' XDATA (EX. 'PPPSETUP') IS KEPT
	;; XDATA VALUES, IN ORDER: PRESET NAME, ROW TYPE ("interior" / "exterior"), EXPOSED SIDES EAST-WEST ("W", "E", "WE", "NONE"),
	;; EXPOSED ENDS NORTH-SOUTH ("N", "S", "NS", "NONE"), ORIGINAL LAYER (THE INVERTER LAYER, WHICH DEFINES THE AREA AND NUMBERING),
	;; ORIGINAL BLOCK NAME (THE ELECTRICAL BLOCK, KEPT SO A REPLACED INSERT IS STILL FOUND AND MATCHED BY ITS TYPE),
	;; ORIGINAL INSERTION POINT AND ROTATION (THE PPP BLOCK MOVES TO THE NORTH-EAST CORNER, RestoreBlock PUTS THE ELECTRICAL BLOCK BACK)
	;; THE ORIGINAL LAYER IS KEPT ACROSS RUNS, SO BLOCKS CAN LATER MOVE TO EXTERIOR / INTERIOR LAYERS WITHOUT LOSING THEIR AREA
	;; RETURNS THE UPDATED ENTITY LIST, NIL IF THE BLOCK COULD NOT BE UPDATED (EX. LOCKED LAYER)
	
	(defun ClearAssignment ( ent / )
		(RestoreBlock ent) ;; A REPLACED INSERT GOES BACK TO ITS ELECTRICAL BLOCK AND LAYER FIRST
		(if (PPP:XdataValues ent "PPPASSIGN")
			(entmod (append (entget ent) (list (list -3 (list "PPPASSIGN")))))
			t
		)
	)
	;; REMOVES ANY 'PPPASSIGN' XDATA FROM A BLOCK (AN APPLICATION NAME WITH NO VALUES DELETES THAT APPLICATION'S XDATA)
	;; RETURNS NIL IF THE BLOCK HAD XDATA THAT COULD NOT BE REMOVED, ELSE NON-NIL
	
	
	;; PPP ARRAY BLOCKS ;;
	;; ASSIGNED ARRAY BLOCKS ARE SWAPPED IN PLACE TO A BLOCK BUILT FROM THEIR PRESET (THE INSERT KEEPS ITS HANDLE, XDATA, AND INSERTION POINT)
	;; ONE DEFINITION PER PRESET, NAMED "PPP-<PRESET>" (EX. "PPP-INT-28M56"), SO A BLOCK COUNT GIVES THE PRESET COUNTS LIKE THE PVCASE PPP DRAWING
	;; BASE POINT == NORTH END OF THE TORQUE TUBE ON THE EAST MODULE EDGE, THE SAME POINT PPPPILES MEASURES THE PILES FROM
	;; CONTENTS ON LAYER 0 (BYLAYER) TAKE THE INSERT'S LAYER COLOR: PPP-ARRAY-INT (CYAN), PPP-ARRAY-EXT (MAGENTA)
	;; THE MOTOR SQUARE IS ON PPP-ARRAY-MOTOR, SO PPP:DefinitionData STILL FINDS THE MOTOR IN A PPP BLOCK (LAYER *MOTOR*)
	
	(defun IsPPPBlock ( obj / nam )
		(and
			(= (vla-get-objectname obj) "AcDbBlockReference")
			(not (vl-catch-all-error-p (setq nam (vl-catch-all-apply 'vla-get-effectivename (list obj)))))
			(wcmatch (strcase nam) "PPP-*")
		)
	)
	;; RETURNS T IF THE OBJECT IS AN INSERT OF A PPP ARRAY BLOCK (SEE BlockName), ELSE NIL
	
	(defun BlockName ( pre / nam )
		(setq nam (strcat "PPP-" pre))
		(if (snvalid nam)
			nam
			(vl-string-translate "<>/\\\":;?*|,=`" "_____________" nam)
		)
	)
	;; RETURNS THE PPP ARRAY BLOCK NAME FOR A PRESET NAME (EX. "INT-28M56" -> "PPP-INT-28M56"), CHARACTERS NOT ALLOWED IN BLOCK NAMES BECOME "_"
	
	(defun RowLayer ( row / )
		(if (= "EXTERIOR" (strcase row))
			"PPP-ARRAY-EXT"
			"PPP-ARRAY-INT"
		)
	)
	;; RETURNS THE LAYER FOR AN ASSIGNED INSERT FROM ITS ROW TYPE ("interior" / "exterior")
	
	(defun EnsureLayers ( / )
		(foreach itm (list (cons "PPP-ARRAY-INT" 4) (cons "PPP-ARRAY-EXT" 6) (cons "PPP-ARRAY-MOTOR" 1))
			(if (null (tblsearch "LAYER" (car itm)))
				(vla-put-color (vla-add (vla-get-layers doc) (car itm)) (cdr itm))
			)
		)
	)
	;; CREATES THE PPP ARRAY LAYERS IF MISSING: INT CYAN (4), EXT MAGENTA (6), MOTOR RED (1), EXISTING LAYERS KEEP THEIR COLORS
	
	(defun BlockSignature ( pre / )
		(apply 'strcat
			(cons "PPP-BLOCK-1"
				(mapcar
					'(lambda ( key / val )
						(setq val (json:Get pre key))
						(strcat "|"
							(cond
								((numberp val) (rtos val 2 6))
								((= 'STR (type val)) val)
								((vl-consp (setq val (json:Items val)))
									(apply 'strcat (mapcar '(lambda ( num ) (if (numberp num) (strcat (rtos num 2 6) ",") "?,")) val))
								)
								( t "?")
							)
						)
					)
					(list "SizeX" "SizeY" "ModuleGap" "JointGapSize" "MotorGapSize" "PilingOverhangLeftSize" "Packs" "HorizontalDistances")
				)
			)
		)
	)
	;; RETURNS A TEXT SIGNATURE OF A PRESET'S DRAWN GEOMETRY, STORED IN THE BLOCK DEFINITION'S COMMENTS
	;; A DEFINITION WHOSE SIGNATURE NO LONGER MATCHES ITS PRESET IS REDRAWN, "PPP-BLOCK-1" CHANGES IF THE DRAWING STYLE CHANGES
	
	(defun DrawRect ( blk xa ya xb yb lay / arr pln )
		(setq arr (vlax-make-safearray vlax-vbDouble (cons 0 7)))
		(vlax-safearray-fill arr (list xa ya xb ya xb yb xa yb))
		(setq pln (vla-addlightweightpolyline blk arr))
		(vla-put-closed pln :vlax-true)
		(vla-put-layer pln lay)
		pln
	)
	;; ADDS A CLOSED RECTANGLE FROM ( xa ya ) TO ( xb yb ) ON LAYER [ lay ] TO BLOCK DEFINITION [ blk ]
	
	(defun DrawTracker ( blk pre / wid len ovh siz gap mtr cen tub mrk spn pos )
		(setq
			wid (json:Get pre "SizeX")
			len (PPP:TubeLength pre)
			ovh (PPP:TubeOverhangs pre) ;; ( NORTH SOUTH ), TORQUE TUBE END TO MODULES
			siz (json:Get pre "SizeY")
			gap (json:Get pre "ModuleGap")
			mtr (json:Get pre "MotorGapSize")
			cen (* -0.5 wid) ;; TORQUE TUBE CENTER LINE
			tub (* 0.03 wid) ;; HALF THE TORQUE TUBE MARK WIDTH (PVCASE: 0.469 FT FOR A 7.822 FT MODULE)
			mrk (* 0.1 wid) ;; HALF THE JOINT / MOTOR GAP MARK WIDTH (PVCASE: 1.564 FT), ALSO THE MOTOR SQUARE SIZE (0.782 FT)
			spn (PPP:PackSpans (PPP:ParsePacks (json:Get pre "Packs")) siz gap (json:Get pre "JointGapSize") mtr) ;; FT FROM THE NORTH EDGE OF THE MODULES
		)
		(DrawRect blk (- wid) 0.0 0.0 (- len) "0") ;; OUTLINE, TORQUE TUBE END TO END
		(if (> (car ovh) (PPP:PresetTolerance))
			(DrawRect blk (- cen tub) 0.0 (+ cen tub) (- (car ovh)) "0")
		) ;; NORTH TORQUE TUBE END
		(foreach itm spn
			(setq pos (+ (car ovh) (caddr itm))) ;; START OF THE SPAN, DISTANCE SOUTH OF THE BASE POINT (THE MODULES START AFTER THE NORTH OVERHANG)
			(cond
				((= "Modules" (car itm))
					(repeat (cadr itm)
						(DrawRect blk (- wid) (- pos) 0.0 (- (+ pos siz)) "0")
						(setq pos (+ pos siz gap))
					)
				)
				( t
					(DrawRect blk (- cen mrk) (- pos) (+ cen mrk) (- (+ (car ovh) (cadddr itm))) "0") ;; JOINT OR MOTOR GAP MARK
					(if (= "Motor" (car itm))
						(DrawRect blk
							(- cen (* 0.5 mrk)) (- (+ pos (* 0.5 mtr) (* -0.5 mrk)))
							(+ cen (* 0.5 mrk)) (- (+ pos (* 0.5 mtr) (* 0.5 mrk)))
							"PPP-ARRAY-MOTOR"
						)
					) ;; MOTOR SQUARE AT THE MOTOR GAP CENTER
				)
			)
		)
		(if (> (cadr ovh) (PPP:PresetTolerance))
			(DrawRect blk (- cen tub) (- (+ (car ovh) (cadddr (last spn)))) (+ cen tub) (- len) "0")
		) ;; SOUTH TORQUE TUBE END, FROM THE LAST MODULE
	)
	;; DRAWS A TRACKER INTO BLOCK DEFINITION [ blk ] FROM PRESET [ pre ], NORTH IS UP, THE BLOCK SPANS ( -WIDTH, -TUBE LENGTH ) TO ( 0, 0 )
	;; OUTLINE, ONE RECTANGLE PER MODULE, TORQUE TUBE ENDS, JOINT GAP MARKS, MOTOR GAP MARK AND MOTOR SQUARE (PVCASE PROPORTIONS)
	
	(defun BuildBlock ( pre / nam sig blk old )
		(setq nam (BlockName (json:Get pre "Name")))
		(cond
			((assoc nam bmp)
				nam
			) ;; ALREADY CHECKED THIS RUN
			((not
					(and
						(numberp (json:Get pre "SizeX"))
						(PPP:TubeLength pre)
						(PPP:TubeOverhangs pre)
						(PPP:ParsePacks (json:Get pre "Packs"))
					)
				)
				nil
			) ;; THE PRESET CANNOT BE DRAWN
			( t
				(setq
					sig (BlockSignature pre)
					blk
						(if (tblsearch "BLOCK" nam)
							(vla-item blks nam)
							(vla-add blks (vlax-3d-point 0.0 0.0 0.0) nam)
						)
				)
				(if (/= sig (vla-get-comments blk))
					(progn
						(vlax-for obj blk
							(setq old (cons obj old))
						)
						(foreach obj old
							(vla-delete obj)
						) ;; COLLECTED FIRST, A COLLECTION IS NOT CHANGED WHILE IT IS BEING WALKED
						(DrawTracker blk pre)
						(vla-put-comments blk sig)
					)
				) ;; NEW, OR ITS PRESET CHANGED SINCE IT WAS DRAWN, EVERY INSERT UPDATES WITH THE DEFINITION
				(setq bmp (cons (cons nam pre) bmp))
				nam
			)
		)
	)
	;; RETURNS THE PPP ARRAY BLOCK NAME FOR A PRESET, CREATING OR REDRAWING ITS DEFINITION AS NEEDED, NIL IF THE PRESET CANNOT BE DRAWN
	
	(defun SwapBlock ( ent pre row trk / obj pst nam lay ne )
		(setq
			obj (vlax-ename->vla-object ent)
			lay (RowLayer row)
		)
		(and
			(setq pst (PPP:FindPreset prs pre)) ;; PRESET OBJECT, [ pre ] IS ITS NAME
			(setq nam (BuildBlock pst))
			(numberp (json:Get pst "SizeX"))
			(setq ne (list (+ (nth 2 trk) (* 0.5 (json:Get pst "SizeX"))) (nth 4 trk) 0.0)) ;; NORTH END OF THE TUBE ON THE EAST MODULE EDGE == THE PPP BLOCK'S BASE POINT
			(not
				(vl-catch-all-error-p
					(vl-catch-all-apply
						'(lambda ( )
							(if (/= (strcase (vla-get-effectivename obj)) (strcase nam))
								(vla-put-name obj nam)
							)
							(if (not (equal 0.0 (vla-get-rotation obj) 1e-9))
								(vla-put-rotation obj 0.0)
							)
							(if (not (equal (vlax-get obj 'InsertionPoint) ne 1e-6))
								(vlax-put obj 'InsertionPoint ne)
							) ;; ELECTRICAL BLOCKS CAN BE INSERTED AT ANY CORNER OR ROTATED (MCCRACKEN), THE PPP BLOCK ALWAYS SITS AT THE NORTH-EAST CORNER
							(if (/= (strcase (vla-get-layer obj)) lay)
								(vla-put-layer obj lay)
							)
						)
					)
				)
			) ;; FAILS ON A LOCKED LAYER
		)
	)
	;; POINTS AN ASSIGNED INSERT AT ITS PRESET'S PPP BLOCK, MOVES IT TO THE INT / EXT LAYER, AND PLACES IT AT THE TRACKER'S NORTH-EAST CORNER (ROTATION 0)
	;; [ trk ] == THE TRACKER ITEM ( ENAME BLOCK-NAME X SOUTH NORTH WIDTH ... ) FROM CollectArrayBlocks, IN DRAWING COORDINATES
	;; RETURNS T IF THE INSERT WAS UPDATED, NIL IF THE PRESET CANNOT BE DRAWN OR THE INSERT COULD NOT BE CHANGED
	
	(defun RestoreBlock ( ent / obj val )
		(setq
			obj (vlax-ename->vla-object ent)
			val (PPP:AssignValues ent)
		)
		(if (and (IsPPPBlock obj) (= 'STR (type (std:SafeNth val 5))))
			(vl-catch-all-apply
				'(lambda ( )
					(if (tblsearch "BLOCK" (std:SafeNth val 5))
						(progn
							(vla-put-name obj (std:SafeNth val 5))
							(if (vl-consp (std:SafeNth val 6))
								(vlax-put obj 'InsertionPoint (std:SafeNth val 6))
							)
							(if (numberp (std:SafeNth val 7))
								(vla-put-rotation obj (std:SafeNth val 7))
							) ;; BACK TO THE ELECTRICAL PLACEMENT
						)
					) ;; A PURGED ELECTRICAL DEFINITION CANNOT COME BACK, THE PPP BLOCK STAYS WHERE IT IS
					(if (and (= 'STR (type (std:SafeNth val 4))) (tblsearch "LAYER" (std:SafeNth val 4)))
						(vla-put-layer obj (std:SafeNth val 4))
					)
				)
			)
		)
	)
	;; PUTS A REPLACED INSERT BACK TO ITS ELECTRICAL BLOCK, PLACEMENT AND INVERTER LAYER (FROM ITS XDATA) BEFORE ITS ASSIGNMENT IS CLEARED
	
	
	;; RUN STATE ;;
	
	(defun AssignState ( sel nrw alw / sx sn )
		(setq
			sx 0.0
			sn 0.0
		)
		(foreach itm (car col)
			(setq
				sx (+ sx (nth 2 itm)) ;; TRACKER CENTER LINE X
				sn (+ sn (nth 4 itm)) ;; NORTH EDGE Y, THE SAME FOR THE ELECTRICAL AND THE PPP BLOCK
			)
		) ;; THE SOUTH EDGE IS NOT USED, A PPP BLOCK CAN BE A MODULE GAP SHORTER THAN THE ELECTRICAL BLOCK IT REPLACED
		(append
			(list
				"PPP-ASSIGN-1"
				(itoa nrw)
				(rtos alw 2 4)
				(strcat (itoa (length (car col))) "," (itoa (length (cadr col))) "," (itoa (length (caddr col))))
				(strcat (rtos sx 2 2) "," (rtos sn 2 2))
			)
			(mapcar
				'(lambda ( typ nam / trk )
					(strcat
						(car typ) "=" (cond (nam) ("-"))
						(if (setq trk (FindPair nam))
							(strcat
								"|" (BlockSignature (PPP:FindPreset prs (nth 6 trk)))
								"|" (BlockSignature (PPP:FindPreset prs (nth 7 trk)))
							)
							""
						)
					)
				)
				typs
				sel
			)
		)
	)
	;; RETURNS THE INPUTS OF AN ASSIGN RUN AS A LIST OF STRINGS: EDGE SETTINGS, BLOCK COUNTS AND POSITIONS, AND EACH BLOCK TYPE'S
	;; TRACKER WITH ITS INT / EXT PRESET GEOMETRY, SAVED IN THE DRAWING AFTER A RUN (LDATA "PPPASSIGN" / "State")
	
	(defun AllMapped ( / )
		(vl-every
			'(lambda ( itm / trk val )
				(setq
					trk (FindPair (cdr (assoc (cadr itm) nmp)))
					val (PPP:AssignValues (car itm))
				)
				(if trk
					(and
						(= 'STR (type (std:SafeNth val 5))) ;; REPLACED BY AN EARLIER RUN
						(member (strcase (car val)) (list (strcase (nth 6 trk)) (strcase (nth 7 trk))))
						(= (strcase (vla-get-effectivename (vlax-ename->vla-object (car itm)))) (strcase (BlockName (car val))))
					)
					(null val)
				)
			)
			(car col)
		)
	)
	;; RETURNS T IF EVERY BLOCK ALREADY HAS THE RESULT A RUN WOULD GIVE IT: ASSIGNED TO ITS TYPE'S TRACKER AND REPLACED, OR NOT ASSIGNED
	
	
	(setq
		sta (null ctx) ;; T WHEN RUN AS THE PPPASSIGN COMMAND, NIL WHEN CALLED BY PPPSETUP WITH ITS PROJECT CONTEXT
		doc (vla-get-activedocument (vlax-get-acad-object))
	)
	(setq funcs
		(list
			'std:AssocSet
			'std:AssocIncrement
			'std:SafeNth
			'std:SubtractInterval
			'std:GroupByIndex
			'wsh:ConfirmationDialog
			'json:ReadFile
			'json:WriteFile
			'json:Object
			'json:Array
			'json:Items
			'json:Get
			'json:StringJoin
		)
	)
	
	(PPP:DebugStep "PPPASSIGN" (if sta "start (PPPASSIGN command)" "start (called by PPPSETUP)"))
	(cond
		((PPP:MissingFunctions funcs)
			nil ;; PPP:MissingFunctions PRINTED THE MISSING NAMES
		)
		
		;; PROJECT CONTEXT, PASSED IN BY PPPSETUP OR READ FROM THE PPP-BOM ROOT WHEN RUN AS A COMMAND
		((and sta (null (setq ctx (PPP:ProjectContext))))
			nil ;; PPP:ProjectContext PRINTED THE REASON
		)
		((null
				(progn
					(setq
						ppp (cdr (assoc "ppp" ctx))
						cfg (cdr (assoc "cfg" ctx))
						prs (cdr (assoc "presets" ctx))
					)
					(if sta
						(prompt (cadr (PPP:SourceCheck doc cfg nil)))
					) ;; A CHANGED X-ARRAY IS REPORTED, THE LAYOUT IN THIS DRAWING MAY BE OUT OF DATE
					(PPP:DebugStep "PPPASSIGN" "project data loaded, reading tracker presets")
					(setq pairs (TrackerPairs))
				)
			)
			(prompt "\nError: No valid INT / EXT tracker preset pairs were found. Add presets in the PPPSetup presets dialog.")
		)
		
		;; TRACKERS PLACED AT THEIR HEIGHTS BY PPPPILES ARE FLATTENED FIRST, SO EVERY POSITION, ROTATION AND EDGE CHECK BELOW READS A FLAT BLOCK
		;; THEY ARE PUT BACK AT THE END (SEE PPP:RestoreTrackers), NEVER STOPS THE RUN
		((progn
				(if (and sta (not und))
					(progn
						(vla-startundomark doc)
						(setq und t)
					)
				)
				(setq plc (PPP:FlattenTrackers doc))
				nil
			)
		)
		
		;; ARRAY BLOCK NAMES, A PROJECT SET UP BEFORE THE ARRAY BLOCKS DIALOG (OR WITH NAMES THAT NO LONGER MATCH) PICKS THE BLOCKS HERE
		((and
				(not (ArrayFound (setq msl (ModelObjects))))
				(not (PickBlocks msl))
			)
			nil ;; PickBlocks PRINTED THE REASON
		)
		
		;; GROUP BLOCKS ARE EXPLODED ONE LEVEL, A ONE-TIME STEP FOR DRAWINGS SET UP BEFORE PPPSETUP DID THIS, NEVER STOPS THE RUN
		((progn
				(setq exg (GroupBlocks msl))
				nil
			)
		)
		
		;; ARRAY BLOCKS
		((null
				(progn
					(PPP:DebugStep "PPPASSIGN" (strcat (itoa (length pairs)) " trackers, collecting array blocks"))
					(car (setq col (CollectArrayBlocks)))
				)
			)
			(prompt
				(strcat
					"\nError: No array blocks were found on layers [" (cdr (assoc "array_layers" cfg)) "]."
					(if (or (cadr col) (caddr col)) (strcat " " (itoa (length (cadr col))) " ignored and " (itoa (length (caddr col))) " unsupported blocks were skipped.") "")
				)
			)
		)
		((null
				(progn
					(setq
						afn (strcat ppp "\\PPP-ASSIGN.json") ;; SAVED BLOCK ASSIGNMENTS, IN THE PPP-BOM ROOT
						map (LoadAssignFile afn)
						typs
							(vl-sort
								(mapcar
									'(lambda ( dfn / nam )
										(setq nam (car dfn))
										(list nam (length (vl-remove-if-not '(lambda ( itm ) (= nam (cadr itm))) (car col))) (cdr dfn))
									)
									(vl-remove-if-not '(lambda ( dfn ) (vl-some '(lambda ( itm ) (= (car dfn) (cadr itm))) (car col))) (cadddr col))
								)
								'(lambda ( a b ) (< (strcase (car a)) (strcase (car b))))
							) ;; ( BLOCK-NAME COUNT PPP:DefinitionData ) FOR EACH BLOCK NAME IN USE
						sugs (mapcar '(lambda ( typ ) (SuggestTracker (caddr typ))) typs)
						sel
							(mapcar
								'(lambda ( typ sug / sav )
									(cond
										((FindPair (setq sav (SavedTracker (car typ)))) (car (FindPair sav))) ;; A SAVED ASSIGNMENT WINS IF THAT TRACKER STILL EXISTS
										((car sug))
									)
								)
								typs
								sugs
							)
						idx -1
						skp
							(and
								(not sta)
								(vl-every
									'(lambda ( nam sug )
										(setq idx (1+ idx))
										(and
											(member (TypeSource idx) (list "detected" "saved"))
											(not (and (caddr sug) (/= (strcase nam) (strcase (car sug))))) ;; A SAVED CHOICE THAT DISAGREES WITH DETECTION IS SHOWN
										)
									)
									sel
									sugs
								)
							)
					)
					(PPP:DebugStep "PPPASSIGN" (strcat (itoa (length typs)) " block types, " (if skp "all detected, skipping the dialog" "opening the dialog")))
					(if skp
						(setq res
							(list
								sel
								(fix (PPP:SettingNumber cfg "exterior_rows" 2))
								(PPP:SettingNumber cfg "stagger_allowance" 10.0)
								(PPP:SettingText cfg "exterior_rows" "2")
								(PPP:SettingText cfg "stagger_allowance" "10.0")
							)
						) ;; FROM PPPSETUP, EVERY BLOCK TYPE DETECTED WITH NO CONFLICTING SAVED ASSIGNMENT, NO NEED TO ASK
						(AssignDialog)
					)
				)
			)
			(prompt (if exg "\nPPP assign was cancelled, the group blocks were already exploded (use UNDO to revert)." "\nPPP assign was cancelled, no blocks were changed."))
		)
		
		;; NOTHING CHANGED SINCE THE LAST RUN, LEAVE THE BLOCKS AS THEY ARE
		((and
				(setq
					nmp (mapcar '(lambda ( typ nam ) (cons (car typ) nam)) typs (nth 0 res)) ;; BLOCK NAME -> TRACKER NAME
					ste (AssignState (nth 0 res) (nth 1 res) (nth 2 res))
				)
				(vl-consp (setq sto (vl-catch-all-apply 'vlax-ldata-get (list "PPPASSIGN" "State"))))
				(equal ste (car sto))
				(AllMapped)
			)
			(PPP:DebugStep "PPPASSIGN" "inputs match the last run and every block is mapped, nothing to do")
			(prompt "\nPPP assign: nothing changed since the last run, the array blocks were left as they are.")
			(setq out
				(std:AssocSet
					(std:AssocSet ctx "cfg" cfg)
					"assign"
					(list (cons "assigned" (nth 1 sto)) (cons "exterior" (nth 2 sto)) (cons "blocks" (nth 3 sto)))
				)
			)
		)
		
		;; CLASSIFY, WRITE XDATA, AND REPLACE THE BLOCKS
		( t
			(setq
				sel (nth 0 res)
				nrw (nth 1 res)
				alw (nth 2 res)
				nmp (mapcar '(lambda ( typ nam ) (cons (car typ) nam)) typs sel) ;; BLOCK NAME -> TRACKER NAME
			)
			(if (and sta (not und))
				(progn
					(vla-startundomark doc)
					(setq und t)
				)
			) ;; ALREADY OPEN IF GROUP BLOCKS WERE EXPLODED
			(PPP:DebugStep "PPPASSIGN" (strcat "dialog done, " (itoa nrw) " exterior rows, " (rtos alw 2 3) " ft stagger"))
			(regapp "PPPASSIGN") ;; RETURNS NIL IF ALREADY REGISTERED, WHICH IS FINE
			(setq
				cls (ClassifyTrackers (car col) nrw alw)
				exs (progn (PPP:DebugStep "PPPASSIGN" "exposure: checking north and south ends") (EndExposure (car cls) (caddr cls)))
				nxp 0
				sxp 0
				cnt 0
				ext 0
				una 0
				fal 0
			)
			(PPP:DebugStep "PPPASSIGN" "classify done, writing xdata")
			(mapcar
				'(lambda ( itm sid nse / )
					(if (= 0 (rem (+ cnt una fal) 500))
						(PPP:DebugStep "PPPASSIGN" (strcat "  " (itoa (+ cnt una fal)) " blocks processed"))
					)
					(if (setq trk (FindPair (cdr (assoc (cadr itm) nmp))))
						(progn
							(setq
								pre (if (= "" sid) (nth 6 trk) (nth 7 trk))
								row (if (= "" sid) "interior" "exterior")
							)
							(if (WriteAssignment (car itm) pre row sid nse)
								(progn
									(setq
										trks (cons (append itm (list pre row (if (= "" sid) "NONE" sid) (if (= "" nse) "NONE" nse))) trks) ;; FOR LATER STAGES, SEE THE PROJECT CONTEXT
										nxp (if (and (= "" sid) (wcmatch nse "*N*")) (1+ nxp) nxp) ;; INTERIOR TRACKERS ONLY, EXTERIOR PILES ARE HEAVY EITHER WAY
										sxp (if (and (= "" sid) (wcmatch nse "*S*")) (1+ sxp) sxp)
										cnt (1+ cnt)
										ext (if (= "" sid) ext (1+ ext))
										stats (std:AssocIncrement stats pre) ;; BLOCKS PER PRESET
									)
								)
								(setq fal (1+ fal)) ;; LIKELY ON A LOCKED LAYER
							)
						)
						(progn
							(setq una (1+ una))
							(if (not (ClearAssignment (car itm)))
								(setq fal (1+ fal))
							)
						) ;; NOT ASSIGNED, IT STILL COUNTS AS A NEIGHBOR FOR EDGE CLASSIFICATION
					)
				)
				(car cls)
				(cadr cls)
				exs
			)
			(PPP:DebugStep "PPPASSIGN" "clearing ignored and unsupported blocks")
			(foreach ent (append (cadr col) (caddr col))
				(if (not (ClearAssignment ent))
					(setq fal (1+ fal))
				)
			) ;; IGNORED AND UNSUPPORTED BLOCKS DROP ANY ASSIGNMENT FROM AN EARLIER RUN
			(PPP:DebugStep "PPPASSIGN" "replacing array blocks with PPP blocks")
			(EnsureLayers)
			(setq
				blks (vla-get-blocks doc)
				bmp nil
				swp 0
				sfl 0
			)
			(foreach itm trks
				(if (SwapBlock (car itm) (nth 6 itm) (nth 7 itm) itm)
					(setq swp (1+ swp))
					(setq sfl (1+ sfl))
				)
			) ;; ( ENAME BLOCK-TYPE X SOUTH NORTH WIDTH PRESET ROW SIDES ENDS ) PER ASSIGNED BLOCK
			(if (and (= 0 fal) (= 0 sfl))
				(vl-catch-all-apply 'vlax-ldata-put (list "PPPASSIGN" "State" (list ste cnt ext (length (car col)))))
				(vl-catch-all-apply 'vlax-ldata-delete (list "PPPASSIGN" "State"))
			) ;; A RUN WITH FAILURES IS NOT SAVED, SO THE NEXT RUN TRIES AGAIN
			(if und
				(progn
					(vla-endundomark doc)
					(setq und nil)
				)
			)
			
			;; SAVE BLOCK ASSIGNMENTS AND EDGE SETTINGS
			(PPP:DebugStep "PPPASSIGN" "saving PPP-ASSIGN.json")
			(setq svm (SaveAssignFile afn nmp))
			(PPP:DebugStep "PPPASSIGN" "saving settings and reporting")
			(if (or
					(/= (nth 3 res) (cdr (assoc "exterior_rows" cfg)))
					(/= (nth 4 res) (cdr (assoc "stagger_allowance" cfg)))
				)
				(progn
					(SetSetting "exterior_rows" (nth 3 res))
					(SetSetting "stagger_allowance" (nth 4 res))
					(setq svc (if sta (PPP:SaveSettings (strcat ppp "\\" (PPP:SettingsFile)) cfg) t)) ;; PPPSETUP SAVES THE RETURNED SETTINGS ITSELF
				)
				(setq svc t)
			)
			
			(prompt
				(strcat
					"\nPPP assign: " (itoa cnt) " of " (itoa (length (car col))) " array blocks assigned (" (itoa ext) " exterior, " (itoa (- cnt ext)) " interior)."
					(if (caddr cls)
						(strcat "\n  Row pitch: " (rtos (caddr cls) 2 1) " ft, neighbors within " (rtos (cadddr cls) 2 1) " ft")
						"\n  Warning: No row pitch was found, every tracker is treated as exterior."
					)
					"\n  Exterior rows per side: " (itoa nrw) ", stagger allowance: " (rtos alw 2 3) " ft"
					"\n  Interior trackers with an exposed end (edge piles): " (itoa nxp) " north, " (itoa sxp) " south"
					(if skp "\n  Every block type was detected, the assign dialog was skipped (run PPPASSIGN to review)." "")
					"\n  Block types:"
					(progn
						(setq idx -1)
						(apply 'strcat
							(mapcar
								'(lambda ( typ )
									(setq idx (1+ idx))
									(strcat "\n    " (car typ) " -> " (cond ((nth idx sel)) ("(not assigned)")) " (" (TypeSource idx) ")")
								)
								typs
							)
						)
					)
					"\n  Blocks per preset:"
					(apply 'strcat
						(mapcar
							'(lambda ( itm ) (strcat "\n    " (car itm) ": " (itoa (cdr itm))))
							(vl-sort stats '(lambda ( a b ) (< (car a) (car b))))
						)
					)
					(if exg (strcat "\n  Group blocks exploded: " (itoa (car exg)) " (" (itoa (cadr exg)) " array blocks)") "")
					"\n  Array blocks replaced: " (itoa swp) " inserts, " (itoa (length bmp)) " PPP blocks (PPP-ARRAY-INT cyan, PPP-ARRAY-EXT magenta)"
					(if (> sfl 0) (strcat "\n  Warning: " (itoa sfl) " assigned blocks could not be replaced, check for locked layers or incomplete presets.") "")
					(if (> una 0) (strcat "\n  Not assigned (no tracker chosen): " (itoa una)) "")
					(if (cadr col) (strcat "\n  Ignored (outside contours, flagged by PPPSetup): " (itoa (length (cadr col)))) "")
					(if (caddr col) (strcat "\n  Skipped (rotated, scaled, or empty block): " (itoa (length (caddr col)))) "")
					(if (> fal 0) (strcat "\n  Warning: " (itoa fal) " blocks could not be updated, check for locked layers.") "")
					(if svm (strcat "\n  Block assignments saved to: " afn) (strcat "\n  Warning: Block assignments could not be saved to " afn))
					(if svc "" "\n  Warning: Edge settings could not be saved to the project settings file.")
				)
			)
			(setq out
				(std:AssocSet
					(std:AssocSet
						(std:AssocSet ctx "cfg" cfg)
						"assign"
						(list (cons "assigned" cnt) (cons "exterior" ext) (cons "blocks" (length (car col))))
					)
					"trackers"
					(reverse trks) ;; WEST TO EAST, SAME ORDER AS THE CLASSIFICATION
				)
			)
		)
	)
	(if plc
		(progn
			(PPP:RestoreTrackers plc nil)
			(prompt (strcat "\n  " (itoa (length plc)) " trackers were put back at their last PPPPILES heights, run PPPPILES if assignments changed."))
		)
	) ;; SWAPPED BLOCKS KEEP THEIR ENAME (SEE SwapBlock), SO THEIR PLACEMENT COMES BACK TOO
	(if und
		(progn
			(vla-endundomark doc)
			(setq und nil)
		)
	) ;; GROUP BLOCKS WERE EXPLODED OR TRACKERS FLATTENED BUT THE RUN STOPPED BEFORE THE BLOCKS WERE UPDATED (EX. DIALOG CANCELLED)
	out
)
;; RETURNS THE PROJECT CONTEXT WITH "cfg" UPDATED AND "assign" / "trackers" ADDED (SEE PPP:ProjectContext), NIL IF NO BLOCKS WERE ASSIGNED
;; [ ctx ] == PROJECT CONTEXT FROM PPPSETUP, OR NIL TO READ IT FROM THE PPP-BOM ROOT
;; THE RETURNED SETTINGS INCLUDE ANY EDGE SETTING CHANGES MADE IN THE DIALOG, PPPSETUP SAVES THEM WITH THE REST OF ITS SETTINGS



;;; PPP PILES ;;;
;; PLACES A LABEL AT EVERY PILE OF EVERY ASSIGNED ARRAY BLOCK AND WRITES THE PILE DRIVE FILE AND THE BOM (.xlsx, BOM PROCESSOR FOLDER)
;; - READS EACH ARRAY BLOCK'S 'PPPASSIGN' XDATA (PRESET, ROW TYPE, EXPOSED ENDS, ORIGINAL INVERTER LAYER), RUN PPPASSIGN FIRST
;; - PILE POSITIONS: THE INSERTION POINT IS THE NORTH END OF THE TORQUE TUBE ON THE EAST MODULE EDGE, THE TUBE RUNS HALF A MODULE WEST,
;;   PILE N SITS THE SUM OF THE FIRST N HorizontalDistances SOUTH OF THE INSERTION POINT
;;   VERIFIED ON ORCA FALLS: ALL 36,032 PVCASE BOM PILES WITHIN 0.055 FT (FLAT, BEFORE SLOPE CORRECTION)
;; - NUMBERING MATCHES PVCASE: AREAS IN INVERTER LAYER ORDER, WEST TO EAST BY COLUMN, NORTH TO SOUTH IN A COLUMN, PILE 1 IS THE NORTH PILE
;; - AREA CODES COME FROM THE AREA LABELS (EX. "PCS-1A1" ON E-TEXT), MATCHED TO EACH INVERTER LAYER'S TRACKERS
;; - PILE CLASS FOLLOWS THE NEXTRACKER SIX PIER DESIGN (PJE-000006): HMP / HAP ON EXTERIOR TRACKERS,
;;   SMP / SAPE (EXPOSED END) / SAPEND (SHELTERED END) / SAP ON INTERIOR TRACKERS
;; - EVERY LABEL CARRIES ITS PILE DATA AS 'PPPPILE' XDATA, RE-RUNNING REPLACES THE LABELS FROM AN EARLIER RUN
;; - GROUND (Z TERRAIN ENTER) COMES FROM THE CIVIL GRADING OUTPUT (SLOPE TOOL WORKBOOKS, "Structural" SHEET ONLY), MATCHED TO PPP'S PILES BY X / Y
;; - THE OPTIONS DIALOG LISTS EVERY WORKBOOK IN THE CIVIL FOLDER, ONLY THE SELECTED ONES (AREAS) ARE CALCULATED: TRACKERS WITH NO PILE MATCHED
;;   TO THEM ARE LEFT AS THEY ARE (EARLIER LABELS AND PLACEMENT KEPT), THE OUTPUT FILES ARE THEN NAMED "(SELECTED AREAS)" SO THE FULL BOM IS NOT REPLACED
;; - PILES WITH NO CIVIL GROUND (NO CIVIL OUTPUT AT ALL, OR NO CIVIL PILE WITHIN THE MATCH TOLERANCE) ARE ESTIMATED FROM THE CIVIL TOPO XREF
;;   (PPP-CIVIL-TOPO, EXISTING GRADE, NO GRADING; OLDER PROJECTS USE THEIR COPIED CONTOURS), EVERY ESTIMATED PILE IS MARKED AS NOT CIVIL DATA:
;;   PILE LABEL "(est)", XDATA "ESTIMATE", BOM ROW, AND ONE ESTIMATED PILE MAKES THE RUN AN ESTIMATE (BOM FILE NAME, COMMAND LINE, WARNING POPUP)
;; - THE X-ARRAY AND CIVIL TOPO FILES ARE CHECKED FOR CHANGES SINCE SETUP FIRST: A CHANGED TOPO IS RELOADED, A CHANGED X-ARRAY IS REPORTED
;; - EVERY TRACKER IS SOLVED AS ONE STRAIGHT TORQUE TUBE (PPP:SolveTable): LEAST GRADING (DEFAULT) OR FEWEST PILES OUT OF RANGE, TRACKERS THAT FIT
;;   GO AS LOW AS POSSIBLE (LOWEST REVEAL). "Civil COTT as delivered" KEEPS CIVIL'S COTT. EACH PILE'S MIN COTT IS THE WINDOW MINIMUM, AND ON A
;;   FLOODED PILE (QGIS FLOOD CSV) AT LEAST FLOOD DEPTH + FREEBOARD + EDGE CLEARANCE + MODULE DROP AT MAX TILT
;; - PILE PLAN POSITIONS ARE SLOPE CORRECTED ABOUT THE ANCHOR PILE (SETTING, MOTOR BY DEFAULT), THE TRACKER BLOCK IS PLACED AT ITS HEIGHT AND TILT
;;   (PPP:PlaceTracker), PLACED BLOCKS ARE FLATTENED AT THE START OF PPPASSIGN AND PPPPILES SO PLAN LOGIC ALWAYS READS FLAT BLOCKS
;; - BOM: COTT OK? (OK / LOW / LOW_PER_FLOOD / HIGH WITH THE TOLERANCE), PILE AND TABLE RECOMMENDATION (GOOD / GRADE), CUT(-) / FILL(+) TO THE OK BAND
;; - THE BOM ROWS ARE BUILT FROM THE SAME PILE DATA THE LABELS CARRY (PPP:BomRow), A FILE OPEN IN EXCEL STOPS FOR CLOSE AND RETRY,
;;   ON CANCEL THE LABELS STAY AND PPPBOM WRITES THE FILES LATER WITHOUT SOLVING
;; !REVISIT! PILE SECTIONS / LENGTHS / WEIGHTS (PILE SCHEDULE + SOIL ZONES), BUSHING PILES (MCCRACKEN) ARE NOT BUILT YET

(defun PPPPilesMain
	(
		ctx
		/
		;Functions
		*error*
		CollectTrackers LayerNumber NumberTrackers CleanText CollectAreaLabels MatchAreas
		PileClass ClassLayer ClearPiles MakeLayers CottWindow OptionsDialog FillSlopes CivilData CivilSelection MatchedTrackers
		TrackerGround ContourHeights FloodData SolveTracker HitSource PileRecord PilePlace
		;Variables
		sta doc und des funcs ppp cfg prs trk una nop leg rpl old npl ars arw hgt cnt cls xrws ord tbl cum pos mot ext
		cod lbl ccn msk bom tim cvo cvf udc tol mtl upd cvd civ pls mch cwn ctl hsc inc sdx sdy mxd ofs hst sts src est why cck gap erw scm
		cvs kpt hds ntr plc gol sbd flp fld fmc smx ofa ofm anc clr fbd alw grd tgr nlo res tsv pds tre nfp stw ntd
	)
	
	(defun *error* ( msg / )
		(if (eq (type des) 'FILE)
			(close des)
		) ;; 'des' IS NEVER SET HERE: BY DYNAMIC SCOPE THIS CLOSES A FILE LEFT OPEN BY A HELPER THAT NAMES ITS HANDLE 'des' (EX. json:ReadFile WHILE THE PROJECT CONTEXT IS READ) WHEN AN ERROR OR ESC HITS PART WAY
		(if plc
			(vl-catch-all-apply 'PPP:RestoreTrackers (list plc nil))
		) ;; TRACKERS FLATTENED AT THE START AND NOT PLACED AGAIN GO BACK WHERE THEY WERE (PPP:RestoreTrackers SKIPS PLACED ONES)
		(if und
			(vla-endundomark doc)
		) ;; ENDS THE UNDO MARK SO AN INTERRUPTED RUN CAN BE UNDONE IN ONE STEP
		(PPP:ErrorReport msg "\nPPP piles cancelled, use UNDO to remove any pile labels already placed.")
	)
	
	
	;; TRACKERS ;;
	
	(defun CollectTrackers ( / ent dat pre ins hd siz mtc out )
		(setq
			una 0 ;; ARRAY BLOCKS NOT ASSIGNED
			nop 0 ;; ASSIGNED TO A PRESET THAT IS MISSING OR HAS NO PILE SPACING
			leg 0 ;; ASSIGNED BEFORE END EXPOSURE WAS ADDED (LEGACY 3 VALUE XDATA)
			rpl 0 ;; ASSIGNED BUT NOT REPLACED BY A PPP BLOCK
		)
		(vlax-for obj (vla-get-modelspace doc)
			(if (= (vla-get-objectname obj) "AcDbBlockReference")
				(progn
					(setq
						ent (vlax-vla-object->ename obj)
						dat (PPP:AssignValues ent)
					)
					(cond
						((null dat)
							(if (PPP:IsArrayBlock obj cfg)
								(setq una (1+ una))
							)
						)
						((not
								(and
									(setq pre (PPP:FindPreset prs (std:SafeNth dat 0)))
									(= 'LIST (type (setq hd (json:Items (json:Get pre "HorizontalDistances")))))
									(cdr hd)
									(vl-every 'numberp hd)
									(numberp (setq siz (json:Get pre "SizeX")))
								)
							)
							(setq nop (1+ nop))
						)
						((not (and (wcmatch (strcase (vla-get-effectivename obj)) "PPP-*") (equal 0.0 (vla-get-rotation obj) 1e-9)))
							(setq rpl (1+ rpl))
						) ;; ONLY PPP BLOCKS (BASE POINT AT THE TUBE'S NORTH END, EAST EDGE, ROTATION 0) GIVE RELIABLE PILE POSITIONS, PPPASSIGN REPLACES THE ELECTRICAL BLOCKS
						( t
							(if (< (length dat) 5)
								(setq leg (1+ leg))
							)
							(setq ins (vlax-get obj 'InsertionPoint))
							(setq out
								(cons
									(list
										ent
										(std:SafeNth dat 0) ;; PRESET NAME
										(= "EXTERIOR" (strcase (std:SafeNth dat 1)))
										(cond ((std:SafeNth dat 3)) ("NONE")) ;; EXPOSED ENDS
										(cond ((std:SafeNth dat 4)) ((vla-get-layer obj))) ;; INVERTER LAYER
										(- (car ins) (* 0.5 siz)) ;; TORQUE TUBE X
										(cadr ins) ;; TORQUE TUBE NORTH END
										hd
										(cdr
											(cond
												((assoc (std:SafeNth dat 0) mtc))
												((car (setq mtc (cons (cons (std:SafeNth dat 0) (PPP:MotorPileIndex pre)) mtc))))
											)
										) ;; MOTOR PILE, CALCULATED ONCE PER PRESET
										siz
										(vla-get-handle obj) ;; READ ONCE PER TRACKER, EVERY PILE LABEL STORES IT
										(PPP:StowDrop pre) ;; MODULE EDGE DROP AT MAX TILT (FT), NIL IF THE PRESET HAS NO MODULE SIZE / TILT
									)
									out
								)
							)
						)
					)
				)
			)
		)
		out
	)
	;; COLLECTS EVERY BLOCK WITH 'PPPASSIGN' XDATA, ON ANY LAYER, SO BLOCKS MOVED TO OTHER LAYERS LATER ARE STILL FOUND
	;; RETURNS ( ENAME PRESET EXTERIOR-P ENDS LAYER TUBE-X NORTH-Y DISTANCES MOTOR-PILE MODULE-WIDTH HANDLE STOW-DROP ) ITEMS
	;; SETS THE SKIPPED BLOCK COUNTS 'una' (NOT ASSIGNED), 'nop' (NO USABLE PRESET), 'leg' (LEGACY ASSIGNMENT), 'rpl' (NOT REPLACED) FOR THE SUMMARY
	;; ONLY REPLACED PPP BLOCKS ARE USED (SEE SwapBlock IN PPPASSIGN), SO THE TUBE IS ALWAYS HALF A MODULE WEST OF THE INSERTION POINT
	
	(defun LayerNumber ( lay / pos )
		(setq pos (strlen lay))
		(while (and (> pos 0) (wcmatch (substr lay pos 1) "#"))
			(setq pos (1- pos))
		)
		(if (< pos (strlen lay))
			(atoi (substr lay (1+ pos)))
		)
	)
	;; RETURNS THE NUMBER AT THE END OF A LAYER NAME (EX. "E-INV12" -> 12), NIL IF IT DOES NOT END IN A NUMBER
	
	(defun NumberTrackers ( trk / grp hit srt col cols tbl out )
		(foreach itm trk
			(setq grp
				(if (setq hit (assoc (strcase (nth 4 itm)) grp))
					(subst (cons (car hit) (cons itm (cdr hit))) hit grp)
					(cons (list (strcase (nth 4 itm)) itm) grp)
				)
			)
		)
		(setq grp
			(vl-sort grp
				'(lambda ( a b / na nb )
					(setq
						na (LayerNumber (car a))
						nb (LayerNumber (car b))
					)
					(cond
						((and na nb (/= na nb)) (< na nb))
						((and na (not nb)) t)
						((and nb (not na)) nil)
						( t (< (car a) (car b)))
					)
				)
			)
		) ;; AREAS IN INVERTER ORDER (E-INV1, E-INV2 ... E-INV32), NOT TEXT ORDER
		(setq tbl 0)
		(foreach area grp
			(setq
				srt (vl-sort (cdr area) '(lambda ( a b ) (< (nth 5 a) (nth 5 b))))
				cols nil
				col nil
			)
			(foreach itm srt
				(if (and col (<= (- (nth 5 itm) (nth 5 (car col))) (nth 9 itm))) ;; WITHIN ONE MODULE WIDTH OF THE PREVIOUS TRACKER == SAME COLUMN
					(setq col (cons itm col))
					(progn
						(if col
							(setq cols (cons col cols))
						)
						(setq col (list itm))
					)
				)
			)
			(if col
				(setq cols (cons col cols))
			)
			(foreach col (reverse cols)
				(foreach itm (vl-sort col '(lambda ( a b ) (> (nth 6 a) (nth 6 b)))) ;; NORTH TO SOUTH
					(setq out (cons (cons (setq tbl (1+ tbl)) itm) out))
				)
			)
		)
		(reverse out)
	)
	;; NUMBERS THE TRACKERS LIKE PVCASE: AREA BY AREA IN INVERTER LAYER ORDER, COLUMNS WEST TO EAST, NORTH TO SOUTH IN A COLUMN
	;; RETURNS ( TABLE-NUMBER . TRACKER ) ITEMS IN TABLE ORDER
	;; VERIFIED ON ORCA FALLS: MATCHES THE PVCASE FRAME NUMBERS IN ALL 32 AREAS
	
	
	;; AREAS ;;
	
	(defun CleanText ( str / pos )
		(if (setq pos (vl-string-position (ascii ";") str nil t))
			(setq str (substr str (+ pos 2)))
		) ;; DROP MTEXT FORMAT CODES (EX. "\fArial|b0;"), THE LABEL TEXT FOLLOWS THE LAST ONE
		(vl-string-trim " \t\r\n" (vl-list->string (vl-remove-if '(lambda ( ch ) (member ch '(123 125))) (vl-string->list str))))
	)
	;; RETURNS TEXT WITHOUT MTEXT FORMAT CODES OR BRACES
	
	(defun CollectAreaLabels ( / lyp txp pfx str out )
		(setq
			lyp (strcase (PPP:SettingText cfg "area_label_layer" "E-TEXT"))
			txp (strcase (PPP:SettingText cfg "area_label_pattern" "PCS-*"))
			pfx (strcase (PPP:SettingText cfg "area_label_prefix" "PCS-"))
		)
		(vlax-for obj (vla-get-modelspace doc)
			(if (and
					(member (vla-get-objectname obj) (list "AcDbMText" "AcDbText"))
					(wcmatch (strcase (vla-get-layer obj)) lyp)
					(wcmatch (setq str (strcase (CleanText (vla-get-textstring obj)))) txp)
				)
				(setq out
					(cons
						(list
							(if (and (/= "" pfx) (= pfx (substr str 1 (strlen pfx)))) (substr str (1+ (strlen pfx))) str) ;; "PCS-1A1" -> "1A1"
							(vlax-get obj 'InsertionPoint)
						)
						out
					)
				)
			)
		)
		out
	)
	;; COLLECTS THE AREA LABELS (TEXT OR MTEXT) ON THE AREA LABEL LAYER WHOSE TEXT MATCHES THE AREA LABEL PATTERN
	;; RETURNS ( AREA-CODE INSERTION-POINT ) ITEMS, THE PREFIX IS REMOVED SO THE CODE MATCHES THE BOM (EX. "1A1")
	
	(defun MatchAreas ( trk lbl / mid hit dst bst out prb lay cds )
		(foreach itm lbl
			(setq bst nil)
			(foreach tr trk
				(setq
					mid (list (nth 5 tr) (- (nth 6 tr) (* 0.5 (apply '+ (nth 7 tr))))) ;; MIDDLE OF THE TORQUE TUBE
					dst (distance (list (car (cadr itm)) (cadr (cadr itm))) mid)
				)
				(if (or (null bst) (< dst (cdr bst)))
					(setq bst (cons tr dst))
				)
			)
			(if bst
				(setq hit (cons (list (strcase (nth 4 (car bst))) (car itm) (cdr bst)) hit))
			)
		) ;; EACH LABEL BELONGS TO THE INVERTER LAYER OF ITS NEAREST TRACKER (CHECKED PER LAYER, NEAREST TRACKER PER TRACKER IS ONLY 87% RIGHT)
		(foreach tr trk
			(if (not (member (setq lay (strcase (nth 4 tr))) cds))
				(setq cds (cons lay cds))
			)
		)
		(foreach lay cds
			(setq bst (vl-sort (vl-remove-if-not '(lambda ( h ) (= lay (car h))) hit) '(lambda ( a b ) (< (caddr a) (caddr b)))))
			(cond
				((null bst)
					(setq prb (cons (strcat lay ": no area label found") prb))
				)
				((cdr bst)
					(setq
						out (cons (cons lay (cadr (car bst))) out)
						prb (cons (strcat lay ": " (itoa (length bst)) " labels (" (apply 'strcat (mapcar '(lambda ( h ) (strcat (cadr h) " ")) bst)) "), used " (cadr (car bst))) prb)
					)
				)
				( t
					(setq out (cons (cons lay (cadr (car bst))) out))
				)
			)
		)
		(list out (reverse prb))
	)
	;; MATCHES AREA LABELS TO INVERTER LAYERS: A LABEL BELONGS TO THE LAYER OF THE TRACKER NEAREST TO IT
	;; RETURNS ( ( ( LAYER . AREA-CODE ) ... ) PROBLEMS ), PROBLEMS LIST LAYERS WITH NO LABEL OR MORE THAN ONE (THE NEAREST IS USED)
	;; VERIFIED ON ORCA FALLS: ALL 32 'PCS-' LABELS MATCH THE BOM AREA CODES
	
	
	;; PILES ;;
	
	(defun PileClass ( ext pole cnt mot nse / )
		(cond
			((and mot (= pole mot))
				(if ext "HMP" "SMP")
			)
			(ext
				"HAP"
			)
			((and (member nse (list "N" "NS")) (<= pole 2))
				"SAPE"
			)
			((and (member nse (list "S" "NS")) (>= pole (1- cnt)))
				"SAPE"
			)
			((or (= pole 1) (= pole cnt))
				"SAPEND"
			)
			( t
				"SAP"
			)
		)
	)
	;; RETURNS THE PILE CLASS FOR PILE [ pole ] OF [ cnt ] ON A TRACKER (NEXTRACKER SIX PIER DESIGN, PJE-000006 SECTION 7)
	;; [ ext ] == T FOR AN EXTERIOR TRACKER, [ mot ] == MOTOR PILE NUMBER (NIL IF NONE), [ nse ] == EXPOSED ENDS ("N", "S", "NS", "NONE")
	;; HMP / HAP == HEAVY MOTOR / ARRAY, SMP == STANDARD MOTOR, SAPE == THE TWO PILES AT AN EXPOSED END, SAPEND == END PILE AT A SHELTERED END
	;; VERIFIED ON ORCA FALLS: MATCHES THE BOM PILE TYPE ON ALL 36,032 PILES (USING THE HAND-DRAWN EXPOSURE ZONES)
	;; !REVISIT! FOUR PIER DESIGN (UNDER 30 MW) HAS NO SAPE / SAPEND, EXPOSED ENDS USE HEAVY PILES INSTEAD
	
	(defun ClassLayer ( cls / )
		(strcat "PPP-PILE-" cls)
	)
	;; RETURNS THE LABEL LAYER FOR A PILE CLASS (EX. "PPP-PILE-SAPE"), ONE LAYER PER CLASS SO EACH CAN BE SHOWN OR HIDDEN
	
	(defun MakeLayers ( / lys lay )
		(setq lys (vla-get-layers doc))
		(foreach itm (list (cons "HMP" 2) (cons "HAP" 1) (cons "SMP" 3) (cons "SAP" 4) (cons "SAPE" 5) (cons "SAPEND" 30))
			(if (not (vl-catch-all-error-p (setq lay (vl-catch-all-apply 'vla-add (list lys (ClassLayer (car itm)))))))
				(vla-put-color lay (cdr itm))
			)
		)
	)
	;; CREATES THE PILE LABEL LAYERS (OR UPDATES THEIR COLOR), ARRAY PILES FOLLOW THE NEXTRACKER PIER LEGEND:
	;; HEAVY ARRAY == RED, STANDARD ARRAY == CYAN, EDGE == BLUE, END == ORANGE
	;; MOTOR PILES HAVE THEIR OWN COLORS (THE LEGEND USES A DIAMOND SYMBOL, LABELS CANNOT): HEAVY MOTOR == YELLOW, STANDARD MOTOR == GREEN
	
	(defun ClearPiles ( hds / lst del )
		(foreach itm (PPP:PileLabels doc)
			(if (or (null hds) (member (nth 10 (cdr itm)) hds)) ;; THE LABEL'S ARRAY BLOCK HANDLE
				(setq lst (cons (car itm) lst))
			)
		)
		(setq del 0)
		(foreach ent lst
			(if (entdel ent)
				(setq del (1+ del))
			)
		)
		(list del (- (length lst) del))
	)
	;; ERASES THE PILE LABELS FROM AN EARLIER RUN (MTEXT WITH 'PPPPILE' XDATA), RETURNS ( ERASED FAILED ), FAILED IS LIKELY A LOCKED LAYER
	;; [ hds ] == ARRAY BLOCK HANDLES TO CLEAR (ONLY SOME CIVIL WORKBOOKS SELECTED, OTHER AREAS KEEP THEIR LABELS), NIL CLEARS EVERY LABEL
	
	
	;; OPTIONS AND CIVIL DATA ;;
	
	(defun CottWindow ( / bkt stp )
		(cond
			((and (numberp (cdr (assoc "cott_min" cfg))) (numberp (cdr (assoc "cott_max" cfg))))
				(list (float (cdr (assoc "cott_min" cfg))) (float (cdr (assoc "cott_max" cfg))))
			) ;; PPPSETUP SAVES THE RANGE IT CALCULATED FROM THE FOUNDATION SCHEDULE BUCKETS
			((and
					(setq bkt (std:ParseNumberList (cond ((cdr (assoc "cott_buckets" cfg))) (""))))
					(setq stp (PPP:SettingNumber cfg "bucket_step" 0.5))
				)
				(list (- (apply 'min bkt) stp) (apply 'max bkt))
			)
		)
	)
	;; RETURNS THE COTT WINDOW ( MIN MAX ) IN FEET FROM THE PROJECT SETTINGS, NIL IF IT IS NOT SET
	
	(defun OptionsDialog ( / dcl ttl tmp dch sts res gks BrowseWorkbooks BrowseFlood DCL:Files DCL:Pick DCL:Flood DCL:Accept )
		(setq ttl "PPP_Piles")
		(setq dcl
			(list
				"// Temporary DCL file;"
				(strcat ttl " : dialog {")
				"	label = \"Pile Plot Plan Piles - Options\";"
				"	: boxed_column {"
				"		label = \"Civil grading output (slope tool)\";"
				"		: text { key = \"civil_path\"; width = 90; }"
				"		: list_box { label = \"Workbooks to calculate (Ctrl / Shift + click for several):\"; key = \"civil_files\"; multiple_select = true; width = 90; height = 8; }"
				"		: row {"
				"			: button { label = \"Browse...\"; key = \"browse\"; width = 14; fixed_width = true; }"
				"			: button { label = \"Select all\"; key = \"select_all\"; width = 14; fixed_width = true; }"
				"			: button { label = \"Clear\"; key = \"select_none\"; width = 10; fixed_width = true; }"
				"			: toggle { label = \"Use Civil grading output (ground, frame, COTT, slope)\"; key = \"civil_use\"; }"
				"		}"
				"		: text { key = \"civil_note\"; width = 90; }"
				"		: edit_box { label = \"Pile match tolerance (ft):\"; key = \"civil_tolerance\"; edit_width = 8; }"
				"		: text { label = \"Piles are matched by X / Y. Pile positions stay PPP's (preset spacing).\"; }"
				"	}"
				"	: boxed_column {"
				"		label = \"Heights\";"
				"		: popup_list { label = \"Optimize for:\"; key = \"optimize_for\"; edit_width = 34; }"
				"		: toggle { label = \"Trackers that cannot fit may use the COTT tolerance\"; key = \"solve_band\"; }"
				"		: row {"
				"			: text { key = \"flood_path\"; width = 64; }"
				"			: button { label = \"Flood file...\"; key = \"flood_browse\"; width = 14; fixed_width = true; }"
				"			: button { label = \"None\"; key = \"flood_clear\"; width = 8; fixed_width = true; }"
				"		}"
				"	}"
				"	: boxed_column {"
				"		label = \"Output\";"
				"		: toggle { label = \"Write the pile drive file\"; key = \"pile_drive\"; }"
				"		: text { key = \"cott_window\"; width = 90; }"
				"	}"
				"	spacer_1;"
				"	errtile;"
				"	ok_cancel;"
				"}"
			)
		)
		
		(defun BrowseWorkbooks ( / dir fls )
			(cond
				((null (setq dir (wsh:ReturnFolder "Select the Civil Grading Output Folder" 512 (cond (cvo) (ppp)))))
					(prompt "\nNo Civil folder was selected, the options are unchanged.")
				)
				((null (setq fls (PPP:CivilFiles (setq dir (vl-string-right-trim "\\" dir)) (cdr (assoc "civil_files" cfg)))))
					(prompt (strcat "\nNo .xlsx workbooks were found in " dir ", the options are unchanged."))
				)
				( t
					(setq
						cvo dir
						cvf fls
						cvs (CivilSelection) ;; WORKBOOKS SELECTED LAST TIME THAT ARE IN THIS FOLDER, ELSE EVERY WORKBOOK
						udc "1"
					)
					(prompt (strcat "\nCivil workbooks: " (itoa (length cvf)) " found in " cvo))
				)
			)
		)
		;; BROWSE BUTTON, RUNS WHILE THE OPTIONS DIALOG IS CLOSED (THE FOLDER PICKER CANNOT RUN INSIDE IT): PICK THE FOLDER, ITS WORKBOOKS FILL THE LIST
		;; THE WORKBOOKS TO CALCULATE ARE PICKED IN THE LIST (CTRL / SHIFT + CLICK), CONTENTS ARE CHECKED WHEN THEY ARE READ (SEE CivilData)
		;; NOTHING CHANGES IF THE FOLDER HAS NO WORKBOOKS OR THE PICKER IS CANCELLED
		
		(defun BrowseFlood ( / pth )
			(if (setq pth (getfiled "Select the Flood Depth CSV (QGIS Sampled Points)" (if flp (strcat (vl-filename-directory flp) "\\") (strcat ppp "\\")) "csv" 0))
				(setq flp pth)
			)
		)
		;; FLOOD FILE BUTTON, RUNS WHILE THE OPTIONS DIALOG IS CLOSED LIKE BrowseWorkbooks, A CANCELLED PICK KEEPS THE CURRENT FILE
		
		(defun DCL:Flood ( / )
			(set_tile "flood_path" (if flp (strcat "Flood: " (vl-filename-base flp) (vl-filename-extension flp)) "Flood: none (no flood depths, edge clearance still applies)"))
		)
		;; SHOWS THE FLOOD FILE IN USE
		
		(defun DCL:Files ( / )
			(set_tile "civil_path" (if cvo (strcat "Folder: " cvo) "Folder: (not found, use Browse)"))
			(PPP:FillList "civil_files" (if cvf (mapcar '(lambda ( pth ) (strcat (vl-filename-base pth) (vl-filename-extension pth))) cvf) (list "(no .xlsx files)")) nil)
			(set_tile "civil_files" (json:StringJoin (mapcar '(lambda ( pth ) (itoa (vl-position pth cvf))) cvs) " "))
			(mode_tile "civil_files" (if cvf 0 1))
			(mode_tile "select_all" (if cvf 0 1))
			(mode_tile "select_none" (if cvf 0 1))
			(if (null cvf)
				(progn
					(set_tile "civil_use" "0")
					(mode_tile "civil_use" 1)
				)
				(mode_tile "civil_use" 0)
			)
			(DCL:Pick nil)
		)
		;; SHOWS THE CIVIL FOLDER, ITS WORKBOOKS AND THE SELECTED ONES, THE CIVIL TOGGLE IS ONLY AVAILABLE WHEN FILES WERE FOUND
		
		(defun DCL:Pick ( val / )
			(if val
				(setq cvs (mapcar '(lambda ( pos ) (nth pos cvf)) (read (strcat "(" val ")"))))
			)
			(set_tile "civil_note"
				(cond
					((null cvf) "No Civil files, heights will be ESTIMATED from the Civil topo (NOT Civil data).")
					((= "0" (get_tile "civil_use")) "Turned off, heights are ESTIMATED from the Civil topo (NOT Civil data).")
					((null cvs) "Select one or more workbooks.")
					((= (length cvs) (length cvf)) (strcat "All " (itoa (length cvf)) " workbooks selected, every area is calculated."))
					( t (strcat (itoa (length cvs)) " of " (itoa (length cvf)) " workbooks selected, only their areas are calculated, other areas are left as they are."))
				)
			)
		)
		;; WORKBOOK SELECTION OR CIVIL TOGGLE CHANGED [ val ] == LIST BOX VALUE (EX. "0 2"), NIL TO ONLY REFRESH THE NOTE
		;; THE NOTE SAYS WHAT HAPPENS WITHOUT CIVIL OUTPUT, SO THE CONTOUR ESTIMATE IS NEVER A SURPRISE
		
		(defun DCL:Accept ( / tv )
			(setq tv (std:ToNumber (get_tile "civil_tolerance")))
			(cond
				((not (and tv (> tv 0.0) (<= tv 5.0)))
					(set_tile "error" "Match tolerance must be a number above 0 and up to 5 ft.")
					(mode_tile "civil_tolerance" 2)
				)
				((and (= "1" (get_tile "civil_use")) (null cvf))
					(set_tile "error" "No Civil output files found. Browse to the folder or turn Civil output off.")
				)
				((and (= "1" (get_tile "civil_use")) (null cvs))
					(set_tile "error" "Select one or more workbooks, or turn Civil output off.")
				)
				((and (= "civil" gol) (/= "1" (get_tile "civil_use")))
					(set_tile "error" "Civil COTT needs the Civil output, pick another option or turn Civil output on.")
				)
				( t
					(setq
						udc (get_tile "civil_use")
						tol (vl-string-trim " " (get_tile "civil_tolerance"))
						upd (get_tile "pile_drive")
						sbd (get_tile "solve_band")
					)
					(done_dialog 1)
				)
			)
		)
		;; VALIDATES THE TOLERANCE AND THE CIVIL CHOICE, KEEPS THE DIALOG OPEN WITH A MESSAGE IF SOMETHING IS WRONG
		
		(setq
			sts 2
			gks (list "least_grading" "fewest_piles" "civil") ;; THE "optimize_for" SETTING VALUES, IN POPUP ORDER
		)
		(if (setq res (PPP:DialogLoad dcl "PPPPILES"))
			(progn
				(setq
					tmp (car res)
					dch (cdr res)
				)
				(while (member sts (list 2 3)) ;; 2 == BROWSE, 3 == FLOOD FILE, REOPEN AFTERWARDS (THE PICKERS CANNOT RUN INSIDE THE DIALOG)
					(if (new_dialog ttl dch)
						(progn
							(DCL:Files)
							(if cvf (set_tile "civil_use" udc))
							(DCL:Pick nil) ;; THE NOTE DEPENDS ON THE CIVIL TOGGLE SET ABOVE
							(set_tile "civil_tolerance" tol)
							(set_tile "pile_drive" upd)
							(PPP:FillList "optimize_for" (list "Least grading (default)" "Fewest piles out of range" "Civil COTT as delivered (no optimization)") (cond ((vl-position gol gks)) (0)))
							(set_tile "solve_band" sbd)
							(DCL:Flood)
							(set_tile "cott_window"
								(if (CottWindow)
									(strcat "COTT window " (rtos (car (CottWindow)) 2 2) " - " (rtos (cadr (CottWindow)) 2 2) " ft (project settings, change in PPPSETUP)")
									"COTT window not set, run PPPSETUP"
								)
							)
							(action_tile "civil_files" "(DCL:Pick $value)")
							(action_tile "civil_use" "(DCL:Pick nil)")
							(action_tile "select_all" "(setq cvs cvf) (DCL:Files)")
							(action_tile "select_none" "(setq cvs nil) (DCL:Files)")
							(action_tile "browse" "(setq udc (get_tile \"civil_use\") tol (get_tile \"civil_tolerance\") upd (get_tile \"pile_drive\") sbd (get_tile \"solve_band\")) (done_dialog 2)")
							(action_tile "flood_browse" "(setq udc (get_tile \"civil_use\") tol (get_tile \"civil_tolerance\") upd (get_tile \"pile_drive\") sbd (get_tile \"solve_band\")) (done_dialog 3)")
							(action_tile "flood_clear" "(setq flp nil) (DCL:Flood)")
							(action_tile "optimize_for" "(setq gol (nth (atoi $value) gks))")
							(action_tile "accept" "(DCL:Accept)")
							(action_tile "cancel" "(done_dialog 0)")
							(setq sts (start_dialog))
							(cond
								((= 2 sts) (BrowseWorkbooks))
								((= 3 sts) (BrowseFlood))
							)
						)
						(progn
							(prompt "\nError loading dialog box.")
							(setq sts 0)
						)
					)
				)
				(setq dch (PPP:DialogUnload tmp dch))
			)
			(prompt "\nError loading dialog box.")
		)
		(= 1 sts)
	)
	;; SHOWS THE PPPPILES OPTIONS (CIVIL OUTPUT FOLDER AND WORKBOOKS, MATCH TOLERANCE, OPTIMIZE FOR, FLOOD FILE, PILE DRIVE FILE)
	;; SETS 'cvo' 'cvf' 'cvs' 'udc' 'tol' 'upd' 'gol' 'sbd' 'flp'
	;; RETURNS T IF ACCEPTED, NIL IF CANCELLED
	
	(defun FillSlopes ( rws / prv out )
		(repeat 2
			(setq prv nil out nil)
			(foreach row rws
				(if (and (null (nth 6 row)) prv (nth 6 prv) (nth 9 row) (equal (nth 9 row) (nth 9 prv)))
					(setq row (std:ReplaceNth row 6 (nth 6 prv)))
				)
				(setq
					out (cons row out)
					prv row
				)
			)
			(setq rws out) ;; COMES BACK REVERSED, SO THE SECOND PASS FILLS FROM THE OTHER DIRECTION AND RESTORES THE ORDER
		)
		rws
	)
	;; FILLS A BLANK "Table Slope (deg)" FROM THE NEIGHBORING ROW OF THE SAME TRACKER ("BlockHandle"), CIVIL WRITES ONE SLOPE PER TRACKER
	;; BUT LEAVES IT BLANK ON THE TRACKER'S LAST PILE (MCCRACKEN: 1,489 OF 22,956 PILES, EVERY TRACKER'S SLOPE IS ONE CONSTANT VALUE)
	;; [ rws ] == ONE FILE'S ROWS IN SHEET ORDER (A TRACKER'S PILES ARE NEXT TO EACH OTHER), ROWS WITHOUT A BLOCK HANDLE STAY BLANK
	
	(defun CivilSelection ( / sav out )
		(if (= 'STR (type (cdr (assoc "civil_selected" cfg))))
			(setq sav (mapcar 'strcase (json:StringSplit (cdr (assoc "civil_selected" cfg)) "|")))
		)
		(setq out
			(vl-remove-if-not
				'(lambda ( pth ) (member (strcase (strcat (vl-filename-base pth) (vl-filename-extension pth))) sav))
				cvf
			)
		)
		(cond (out) (cvf))
	)
	;; RETURNS THE WORKBOOKS SELECTED LAST TIME (SETTING "civil_selected", FILE NAMES SEPARATED BY "|") THAT ARE STILL IN 'cvf', ELSE EVERY WORKBOOK
	
	(defun MatchedTrackers ( prs / hnd grp hit out hds )
		(foreach pr (append prs (list nil)) ;; THE NIL ITEM CLOSES THE LAST TRACKER
			(if (not (and pr (= hnd (nth 10 (car pr)))))
				(progn
					(if hit
						(setq
							out (append grp out) ;; 'grp' IS IN REVERSE, THE FINAL REVERSE PUTS EVERY TRACKER BACK IN ORDER
							hds (cons hnd hds)
						)
					)
					(setq
						hnd (if pr (nth 10 (car pr)))
						grp nil
						hit nil
					)
				)
			)
			(if pr
				(setq
					grp (cons pr grp)
					hit (or hit (cddr pr)) ;; ( PILE DISTANCE . CIVIL-PILE ), NIL WHEN THE PILE HAS NO CIVIL MATCH
				)
			)
		)
		(list (reverse out) hds)
	)
	;; KEEPS THE TRACKERS WITH AT LEAST ONE PILE MATCHED TO THE SELECTED CIVIL WORKBOOKS, THE OTHERS ARE IN AREAS THAT WERE NOT SELECTED
	;; [ prs ] == ( PILE . MATCH ) PAIRS IN PILE ORDER (EVERY TRACKER'S PILES TOGETHER), PILE INDEX 10 == ARRAY BLOCK HANDLE
	;; RETURNS ( KEPT-PAIRS KEPT-HANDLES )
	
	(defun CivilData ( fls / shn hdr civ rep fnm fnd fil bad nsl itm )
		(setq
			hdr (PPP:CivilHeaders)
			shn (cond ((cdr (assoc "civil_sheet" cfg))) ("Structural"))
		)
		(foreach res (msxl:ReadTables fls shn hdr)
			(setq
				fnm (strcat (vl-filename-base (car res)) (vl-filename-extension (car res)))
				fnd (if (listp (cadr res)) (cadr (cadr res))) ;; HEADERS FOUND IN THE SHEET, NIL FOR A MISSING ONE
			)
			(cond
				((= 'STR (type (cadr res)))
					(setq rep (cons (strcat fnm ": skipped, " (cadr res)) rep))
				)
				((/= (strcase shn) (strcase (car (cadr res))))
					(setq rep (cons (strcat fnm ": skipped, no \"" shn "\" sheet (found \"" (car (cadr res)) "\")") rep))
				) ;; msxl:ReadTables FALLS BACK TO ANY SHEET NAMED *STR*, ONLY THE STRUCTURAL SHEET IS CIVIL'S PILE DATA
				((not (and (vl-every '(lambda ( i ) (nth i fnd)) (list 0 2 3 4 5)) (or (nth 1 fnd) (nth 8 fnd))))
					(setq rep
						(cons
							(strcat fnm ": skipped, missing columns: "
								(apply 'strcat
									(mapcar
										'(lambda ( i ) (if (nth i fnd) "" (strcat "\"" (nth i hdr) "\" ")))
										(append (list 0) (if (or (nth 1 fnd) (nth 8 fnd)) nil (list 1)) (list 2 3 4 5))
									)
								)
							)
							rep
						)
					)
				) ;; NOT A SLOPE TOOL OUTPUT, OR CIVIL RENAMED A COLUMN
				( t
					(setq fil nil bad 0)
					(foreach row (caddr (cadr res))
						(setq itm (mapcar 'std:ToNumber row)) ;; NUMBER TEXT TOO, NIL FOR OTHER TEXT
						(setq itm (std:ReplaceNth itm 1 (cond ((nth 1 itm)) ((nth 8 itm))))) ;; "Y", OR "Y Adjusted" WHEN THE FILE HAS NO "Y"
						(if (vl-every '(lambda ( i ) (numberp (nth i itm))) (list 0 1 2 3 4 5))
							(setq fil (cons itm fil))
							(if (vl-some 'numberp itm)
								(setq bad (1+ bad))
							) ;; BLANK ROWS ARE NOT COUNTED
						)
					)
					(setq
						fil (FillSlopes (reverse fil))
						nsl (length (vl-remove-if '(lambda ( row ) (nth 6 row)) fil))
						civ (append fil civ)
						rep
							(cons
								(strcat
									fnm " (" (car (cadr res)) "): " (itoa (length fil)) " piles"
									(if (< 0 bad) (strcat ", " (itoa bad) " rows skipped (missing numbers)") "")
									(if (< 0 nsl) (strcat ", " (itoa nsl) " without a table slope") "")
								)
								rep
							)
					)
				)
			)
		)
		(list civ (reverse rep))
	)
	;; READS THE "Structural" SHEET OF THE CIVIL SLOPE TOOL WORKBOOKS (ONE PER AREA), COLUMNS BY NAME (SEE PPP:CivilHeaders)
	;; RETURNS ( PILES REPORT ): PILES == ( X Y Z-ENTER Z-FRAME COTT REVEAL SLOPE Z-ORIGINAL Y-ADJUSTED BLOCK-HANDLE ) ITEMS, REPORT == ONE LINE PER FILE
	;; THE FIRST 6 ARE ALWAYS NUMBERS, SLOPE IS FILLED PER TRACKER (SEE FillSlopes) AND MAY STILL BE NIL, Z-FRAME IS CIVIL'S PILE TOP (SEE PPP:CivilHeaders)
	;; !REVISIT! MATCHING NOW USES CIVIL'S "Y" (0.09 FT MAX FROM THE DELIVERED MCCRACKEN BOM) INSTEAD OF "Y Adjusted" (0.46 FT MAX), CONFIRM WITH CIVIL WHAT "Y Adjusted" IS FOR
	
	(defun TrackerGround ( crs ys / a b out )
		(foreach y ys
			(while (and crs (<= (car (car crs)) y))
				(setq
					a (car crs)
					crs (cdr crs)
				)
			) ;; a == LAST CROSSING AT OR SOUTH OF THE PILE, THE PILES ARE IN SOUTH TO NORTH ORDER SO THE WALK ONLY MOVES FORWARD
			(setq b (car crs)) ;; FIRST CROSSING NORTH OF THE PILE
			(setq out
				(cons
					(cond
						((and a b)
							(cons
								(if (equal (car a) (car b) 1e-9)
									(cadr a)
									(+ (cadr a) (* (- (cadr b) (cadr a)) (/ (- y (car a)) (- (car b) (car a)))))
								)
								(if (equal (cadr a) (cadr b) 1e-9) 'FLAT t)
							)
						) ;; BETWEEN TWO CONTOUR CROSSINGS, LINEAR ALONG THE TRACKER, 'FLAT WHEN BOTH ARE THE SAME CONTOUR LEVEL (RIDGE, VALLEY, OR FLAT GROUND)
						(a (cons (cadr a) nil))
						(b (cons (cadr b) nil)) ;; PAST THE LAST CONTOUR ON ONE SIDE, THE NEAREST CONTOUR'S ELEVATION
					)
					out
				)
			)
		)
		(reverse out)
	)
	;; RETURNS ( Z . WHERE ) FOR EACH Y IN [ ys ] (SOUTH TO NORTH) ALONG ONE NORTH-SOUTH LINE, NIL WHERE THE LINE CROSSES NO CONTOUR
	;; WHERE == T BETWEEN TWO CONTOUR LEVELS, 'FLAT BETWEEN TWO CROSSINGS OF ONE LEVEL (Z IS THAT LEVEL, UP TO ONE CONTOUR INTERVAL OFF), NIL PAST THE LAST CONTOUR
	;; TESTED ON A SYNTHETIC SITE (1 FT CONTOURS): MEDIAN ERROR 0.04 FT, EVERY PILE OFF BY MORE THAN 0.3 FT WAS A 'FLAT PILE
	;; [ crs ] == THE LINE'S ( Y Z ) CONTOUR CROSSINGS SOUTH TO NORTH (SEE std:VerticalCrossings), [ ys ] == PILE Y VALUES SOUTH TO NORTH
	
	(defun ContourHeights ( pls / con ivl zro lns trs xs rng crs ys gnd nin nfl nbl n out )
		(setq
			con (PPP:ContourObjects doc cfg) ;; THE CIVIL TOPO XREF (OLDER PROJECTS: THE COPIED CONTOURS)
			ivl (PPP:ContourInterval (car con))
			zro 0 nin 0 nfl 0 nbl 0
		)
		(foreach obj (car con)
			(if (equal (std:PolylineElevation obj) 0.0 1e-8)
				(setq zro (1+ zro)) ;; FLATTENED, IT WOULD PULL THE GROUND TO 0
				(setq lns (cons (std:PolylinePoints obj) lns))
			)
		)
		(setq trs (mapcar 'reverse (std:GroupRuns pls (function (lambda ( p ) (nth 2 p)))))) ;; ONE LIST PER TRACKER (SAME TABLE NUMBER) IN 'pls' ORDER, PILES SOUTH TO NORTH
		(setq xs (vl-sort (mapcar '(lambda ( grp ) (car (car grp))) trs) '<)) ;; ONE LINE PER TRACKER COLUMN, (vl-sort ... '<) DROPS REPEATED NUMBERS (A REPEAT WOULD ONLY ADD A DUPLICATE LINE)
		(setq rng
			(mapcar
				'(lambda ( x / lo hi )
					(foreach grp trs
						(if (equal x (car (car grp)) 1e-9)
							(setq
								lo (min (cond (lo) (1e99)) (cadr (car grp)))
								hi (max (cond (hi) (-1e99)) (cadr (last grp)))
							)
						)
					)
					(list (- lo 100.0) (+ hi 100.0))
				)
				xs
			)
		) ;; ONLY CROSSINGS NEAR THE COLUMN'S TRACKERS ARE KEPT, 100 FT PAST THE ENDS SO THE END PILES STILL HAVE A CONTOUR ON BOTH SIDES
		(PPP:DebugStep "PPPPILES" (strcat "contour estimate: " (itoa (length lns)) " contours, " (itoa (length xs)) " tracker columns"))
		(setq crs (mapcar 'cons xs (std:VerticalCrossings lns xs rng)))
		(setq lns nil) ;; THE CONTOUR POINTS ARE THE LARGEST LIST, FREE THEM BEFORE THE PILES ARE BUILT
	
		(foreach grp trs
			(setq
				ys (mapcar 'cadr grp)
				gnd (TrackerGround (cdr (assoc (car (car grp)) crs)) ys) ;; ( Z . WHERE ) PER PILE, SOUTH TO NORTH (SEE TrackerGround)
				n (length grp)
			)
			(if (vl-some 'null gnd)
				(setq
					out (append (mapcar '(lambda ( p ) nil) grp) out)
					nbl (+ nbl n)
				) ;; NO CONTOUR CROSSES THIS COLUMN NEAR THE TRACKER, NOTHING TO ESTIMATE
				(setq
					nin (+ nin (length (vl-remove-if 'cdr gnd)))
					nfl (+ nfl (length (vl-remove-if-not '(lambda ( g ) (eq 'FLAT (cdr g))) gnd)))
					out (append (mapcar '(lambda ( p y g ) (list (car p) y (car g))) grp ys gnd) out)
				) ;; ROWS GO IN SOUTH TO NORTH, THE (reverse out) AT THE END PUTS EVERY TRACKER BACK IN 'pls' ORDER (NORTH TO SOUTH)
			)
		)
		(list
			(reverse out)
			(list
				(if (car con)
					(strcat
						(itoa (length (vl-remove-if 'null out))) " of " (itoa (length pls)) " piles have ground from the " (caddr con)
						(if ivl (strcat ", contour interval " (rtos ivl 2 2) " ft") "")
					)
					(strcat "No contours to estimate from: " (caddr con))
				)
				(if (< 0 zro) (strcat (itoa zro) " contours at elevation 0 skipped (flattened)") "")
				(if (< 0 nin) (strcat (itoa nin) " piles are past the last contour on one side, they use the nearest contour's elevation") "")
				(if (< 0 nfl) (strcat (itoa nfl) " piles sit between two crossings of one contour level (ridge, valley, flat), up to one contour interval off") "")
				(if (< 0 nbl) (strcat (itoa nbl) " piles have no contour crossing near their tracker, heights left blank") "")
			)
		)
	)
	;; ESTIMATES THE GROUND AT EVERY PILE FROM THE CIVIL TOPO XREF'S CONTOURS (EXISTING GRADE, NO GRADING, SEE PPP:ContourObjects)
	;; USED FOR THE PILES WITH NO CIVIL GRADING DATA: ALL OF THEM WITHOUT CIVIL OUTPUT, OR ONLY THE ONES NO CIVIL PILE MATCHED
	;; GROUND == LINEAR BETWEEN THE CONTOURS CROSSED SOUTH AND NORTH OF THE PILE ALONG ITS TRACKER LINE, THE TRACKER IS THEN SOLVED LIKE CIVIL GROUND
	;; RETURNS ( ROWS REPORT ): ROWS ALIGN WITH [ pls ], EACH ( X Y Z-ENTER ) OR NIL, REPORT == SUMMARY LINES ("" == NOTHING TO SAY)
	;; !REVISIT! CONTOURS THAT RUN NORTH-SOUTH (PARALLEL TO THE TRACKERS) ARE CROSSED LESS OFTEN, THE ESTIMATE IS COARSER THERE, A SURFACE WOULD BE BETTER (ON HOLD)
	;; [ pls ] == PILES ( X Y TABLE POLE ... ) IN TABLE ORDER, EACH TRACKER'S PILES NEXT TO EACH OTHER, NORTH TO SOUTH (A TRACKER MAY BE PARTIAL)
	
	(defun FloodData ( pth / res hdr pts rsn dmx )
		(setq
			hdr (list "X" "X (ft)" "Y" "Y (ft)" "Flood Depth" "Flood Depth (ft)")
			res (car (msxl:ReadTables (list pth) (substr (vl-filename-base pth) 1 31) hdr)) ;; A CSV OPENS AS ONE SHEET NAMED AFTER THE FILE
		)
		(cond
			((= 'STR (type (cadr res)))
				(setq rsn (cadr res))
			)
			((not (and (or (nth 0 (cadr (cadr res))) (nth 1 (cadr (cadr res)))) (or (nth 2 (cadr (cadr res))) (nth 3 (cadr (cadr res)))) (or (nth 4 (cadr (cadr res))) (nth 5 (cadr (cadr res))))))
				(setq rsn "needs X, Y and \"Flood Depth\" columns")
			)
			( t
				(foreach row (caddr (cadr res))
					(setq row (mapcar 'std:ToNumber row))
					(if (and (cond ((nth 0 row)) ((nth 1 row))) (cond ((nth 2 row)) ((nth 3 row))))
						(setq pts
							(cons
								(list (cond ((nth 0 row)) ((nth 1 row))) (cond ((nth 2 row)) ((nth 3 row))) (cond ((nth 4 row)) ((nth 5 row)) (0.0)))
								pts
							)
							dmx (max (cond (dmx) (0.0)) (caddr (car pts)))
						) ;; A BLANK DEPTH IS OUTSIDE THE FLOOD RASTER (QGIS WRITES NULL), NO FLOOD
					)
				)
			)
		)
		(list
			(reverse pts)
			(strcat
				(vl-filename-base pth) (vl-filename-extension pth) ": "
				(if rsn (strcat "not used, " rsn) (strcat (itoa (length pts)) " points, deepest " (rtos (cond (dmx) (0.0)) 2 2) " ft"))
			)
		)
	)
	;; READS THE FLOOD DEPTH PER POINT FROM THE QGIS SAMPLED CSV (THE BOM POINTS WITH THE FLOOD RASTER SAMPLED), COLUMNS BY NAME
	;; RETURNS ( ( X Y DEPTH ) ... ) AND A REPORT LINE, MATCHED TO THE PILES BY X / Y LIKE THE CIVIL DATA
	;; !REVISIT! ONE FLOOD SPEC FOR NOW (ONE DEPTH COLUMN + "flood_freeboard"), MORE SPECS (EX. 25-YR + 1 FT, 100-YR + 0 FT) TAKE THE LARGEST
	
	(defun SolveTracker ( grp / trk drop mot npl ain da ss zs ls hs lin sol a b cph ang n sy sz syy syz out s zt fl lo hit cot zf rev sts gr slo )
		(setq
			trk (nth 12 (car (car grp)))
			drop (nth 11 trk) ;; MODULE EDGE DROP AT MAX TILT, NIL IF THE PRESET HAS NO MODULE SIZE / TILT
			mot (nth 8 trk)
			npl (length grp)
			ain (cond ((= "NORTH" anc) 1) ((= "SOUTH" anc) npl) (mot) (1)) ;; ANCHOR PILE NUMBER, A TRACKER WITHOUT A MOTOR ANCHORS AT ITS NORTH PILE
			da (nth 11 (car (nth (1- ain) grp))) ;; ANCHOR DISTANCE FROM THE TUBE'S NORTH END
		)
		(setq lin
			(mapcar
				'(lambda ( itm / hit fl lo zt )
					(setq
						hit (cdr (cadr itm)) ;; CIVIL OR ESTIMATED ROW ( X Y Z-ENTER ... ), NIL IF THE PILE HAS NONE
						zt (if (and hit (numberp (nth 2 hit))) (nth 2 hit))
						fl (if (caddr itm) (nth 3 (caddr itm))) ;; FLOOD DEPTH, NIL WITHOUT FLOOD DATA
						lo (cond
								((null cwn) nil)
								((not (and fl (> fl 0.0))) (car cwn)) ;; NOT FLOODED: THE WINDOW MINIMUM, WHICH THE ENGINEERS SET FOR DRY GROUND
								(drop (max (car cwn) (+ fl fbd clr drop))) ;; MODULE EDGE AT MAX TILT CLEARS THE FLOOD SURFACE + FREEBOARD BY THE EDGE CLEARANCE
								((max (car cwn) (+ (car cwn) (- (+ fl fbd) alw)))) ;; NO MODULE DATA: COTTZILLA'S RULE, WINDOW MINIMUM + FLOOD OVER THE ALLOWED FLOOD
							)
					)
					(list (- (nth 11 (car itm)) da) zt lo fl hit)
				)
				grp
			)
		) ;; ( s Z-GROUND MIN-COTT FLOOD ROW ) PER PILE, s == ALONG THE TUBE FROM THE ANCHOR, + SOUTH
		(foreach itm lin
			(if (and (cadr itm) (caddr itm))
				(setq
					ss (cons (car itm) ss)
					zs (cons (cadr itm) zs)
					ls (cons (caddr itm) ls)
					hs (cons (cadr cwn) hs)
				)
			)
		) ;; ONLY PILES WITH GROUND TAKE PART IN THE SOLVE
		(cond
			((null ss)
				nil ;; NO GROUND OR NO COTT WINDOW, NOTHING TO SOLVE
			)
			((and (= "civil" gol) (vl-some '(lambda ( itm ) (and (cadr itm) (numberp (nth 4 (nth 4 itm))))) lin))
				(setq n 0 sy 0.0 sz 0.0 syy 0.0 syz 0.0)
				(foreach itm lin
					(if (and (cadr itm) (numberp (nth 4 (nth 4 itm))))
						(setq
							n (1+ n)
							sy (+ sy (car itm))
							sz (+ sz (+ (cadr itm) (nth 4 (nth 4 itm))))
							syy (+ syy (* (car itm) (car itm)))
							syz (+ syz (* (car itm) (+ (cadr itm) (nth 4 (nth 4 itm)))))
						)
					)
				)
				(if (< 0 n)
					(setq
						b (if (> (- syy (/ (* sy sy) n)) 1e-9) (/ (- syz (/ (* sy sz) n)) (- syy (/ (* sy sy) n))) 0.0)
						a (/ (- sz (* b sy)) n)
					)
				) ;; CIVIL'S FRAME LINE (LEAST SQUARES), ONLY FOR THE BLOCK PLACEMENT AND PILE POSITIONS, THE COTT VALUES STAY CIVIL'S
				;; A TRACKER WITH NO CIVIL COTT (EVERY PILE ESTIMATED FROM THE TOPO) IS SOLVED BELOW FOR LEAST GRADING INSTEAD
			)
			((progn
					(setq
						ss (reverse ss) zs (reverse zs) ls (reverse ls) hs (reverse hs) ;; NORTH TO SOUTH, THE FAST PATH NEEDS s IN ORDER
						sol (PPP:SolveTable ss zs ls hs smx gol) ;; THE STRICT WINDOW FIRST, SO TRACKERS THAT FIT KEEP THE TOLERANCE AS CONSTRUCTION MARGIN
					)
					(if (and sol (= "1" sbd) (< 0 (cadddr sol))) ;; A PILE MISSES ITS WINDOW BY MORE THAN 0.0005 FT, NOT JUST FLOAT NOISE
						(setq sol
							(cond
								((PPP:SolveTable ss zs (mapcar '(lambda ( v ) (- v ctl)) ls) (mapcar '(lambda ( v ) (+ v ctl)) hs) smx gol))
								(sol)
							)
						)
					) ;; ONLY A TRACKER THAT CANNOT FIT IS SOLVED AGAIN WITHIN THE OK BAND (TOLERANCE)
					sol
				)
				(setq
					a (car sol)
					b (cadr sol)
				)
			)
		)
		(if a
			(setq
				b (max -0.99 (min 0.99 b))
				cph (sqrt (- 1.0 (* b b)))
				ang (- (atan b cph)) ;; -ASIN(b): POSITIVE == NORTH END HIGHER, SAME SIGN AS CIVIL'S TABLE SLOPE
				slo (* 180.0 (/ ang pi))
			)
		)
		(setq out
			(mapcar
				'(lambda ( itm row / s zt lo fl hit cot zf rev sts gr )
					(setq
						s (car row)
						zt (cadr row)
						lo (caddr row)
						fl (nth 3 row)
						hit (nth 4 row)
						zf (cond
								((and (= "civil" gol) zt (numberp (nth 4 hit))) (+ zt (nth 4 hit))) ;; CIVIL'S OWN COTT
								(a (+ a (* b s)))
							)
						cot (if (and zf zt) (- zf zt))
						rev (cond
								((null cot) nil)
								((and (= "civil" gol) (numberp (nth 5 hit))) (nth 5 hit))
								((= 1 (nth 9 (car itm))) (- cot ofm))
								((- cot ofa))
							)
						sts (cond
								((not (and cot lo)) "")
								((> (- lo ctl cot) 5e-4) (if (>= cot (- (car cwn) ctl)) "LOW_PER_FLOOD" "LOW"))
								((> (- cot (cadr cwn) ctl) 5e-4) "HIGH")
								("OK")
							)
						gr (cond
								((not (and cot lo)) nil)
								((wcmatch sts "LOW*") (- cot (- lo ctl))) ;; NEGATIVE == CUT THE GROUND DOWN
								((= "HIGH" sts) (- cot (+ (cadr cwn) ctl))) ;; POSITIVE == FILL
								(0.0)
							) ;; GRADED TO THE EDGE OF THE OK BAND, MATCHING THE CULPEPPER BOM'S CUT(-) / FILL(+)
					)
					(list
						(car itm) ;; 0 PILE (SEE PASS 1)
						(if a (- (nth 6 trk) da (* s cph)) (cadr (car itm))) ;; 1 PLAN Y: ANCHOR + s x COS(TILT), FLAT WITHOUT A SOLVE
						zt zf cot rev lo sts gr fl ;; 2 - 9
						(cond (slo) ((and hit (numberp (nth 6 hit))) (nth 6 hit))) ;; 10 TABLE SLOPE (DEG), POSITIVE == NORTH END HIGHER
						hit (car (cadr itm)) ;; 11 ROW, 12 DISTANCE TO THE CIVIL PILE
					)
				)
				grp lin
			)
		)
		(list
			out
			(if a (list (car trk) (- a (* b da)) (- (* da (- 1.0 cph))) ang)) ;; BLOCK PLACEMENT: ENAME, DZ, DY, ANGLE (SEE PPP:PlaceTracker)
		)
	)
	;; SOLVES ONE TRACKER: MIN COTT PER PILE (WINDOW, EDGE CLEARANCE + MODULE DROP, FLOOD), THEN ONE STRAIGHT TORQUE TUBE (SEE PPP:SolveTable)
	;; "civil" KEEPS CIVIL'S COTT AND ONLY FITS ITS FRAME LINE FOR THE POSITIONS AND THE BLOCK
	;; [ grp ] == ONE TRACKER'S ( PILE MATCH FLOOD-MATCH ) ITEMS, NORTH TO SOUTH
	;; RETURNS ( RESULTS PLACEMENT ): ONE RESULT PER PILE ( PILE Y-PLAN Z-ENTER Z-FRAME COTT REVEAL MIN-COTT STATUS GRADING FLOOD SLOPE ROW DISTANCE ),
	;; PLACEMENT == ( ENAME DZ DY ANGLE ) OR NIL WHEN NOTHING WAS SOLVED
	;; THE GROUND IS SAMPLED AT THE FLAT PILE POSITION, AT MOST s x (1 - COS(TILT)) AWAY (0.33 FT ON A 375 FT TRACKER AT 2.4 DEG)
	
	(defun HitSource ( hit / )
		(cond
			((null hit) nil)
			((numberp (nth 3 hit)) "CIVIL")
			("ESTIMATE")
		)
	)
	;; RETURNS WHERE A PILE'S GROUND ROW CAME FROM: "CIVIL" (A CIVIL ROW ALWAYS HAS Z FRAME, SEE CivilData), "ESTIMATE" (A TOPO ROW IS ( X Y Z ) ONLY), NIL WITHOUT ONE
	
	(defun PileRecord ( r / pil hit hsr cot rev sts gr lo )
		(setq
			pil (car r)
			cot (nth 4 r) rev (nth 5 r) lo (nth 6 r) sts (nth 7 r) gr (nth 8 r)
			hit (nth 11 r)
			hsr (HitSource hit)
		)
		(if (= "CIVIL" hsr)
			(setq
				sdx (+ sdx (- (car pil) (car hit)))
				sdy (+ sdy (- (cadr pil) (cadr hit)))
				mxd (max mxd (nth 12 r))
				inc (if (equal (- (nth 3 hit) (nth 2 hit)) (nth 5 hit) 0.01) inc (1+ inc)) ;; CIVIL CHECK: Z FRAME (PILE TOP) - Z ENTER == PILE HEIGHT
			)
		)
		(if cot
			(setq
				ofs (if ofs (list (min (car ofs) (- cot rev)) (max (cadr ofs) (- cot rev))) (list (- cot rev) (- cot rev))) ;; COTT - REVEAL RANGE
				hst (std:AssocIncrement hst sts) ;; COTT CHECK COUNTS
				grd (+ grd (abs (cond (gr) (0.0))))
				nlo (if (and lo (> lo (+ (car cwn) 1e-6))) (1+ nlo) nlo) ;; MIN COTT RAISED BY FLOOD OR THE EDGE CLEARANCE
			)
		)
		(append
			(list
				(nth 5 pil) (nth 2 pil) (nth 3 pil) (nth 6 pil) (nth 7 pil) (if (nth 8 pil) "Exterior" "Interior") (nth 4 pil) (nth 9 pil)
				(car pil) (nth 1 r) (nth 10 pil) ;; X, Y (SLOPE CORRECTED), HANDLE OF THE ARRAY BLOCK
			)
			(if hit
				(list hsr (nth 2 r) (nth 3 r) cot rev (nth 10 r) (if (= "CIVIL" hsr) (nth 12 r)) lo (nth 9 r) gr sts)
				(list nil nil nil nil nil nil nil nil nil nil nil) ;; NO HEIGHTS, THE SAME AS PPP:PileData READS IT BACK FROM THE LABEL
			)
		)
	)
	;; RETURNS THE PILE DATA (22 ITEMS, SEE PPP:PileXdata) FOR ONE SolveTracker RESULT [ r ] AND COUNTS THE RUN STATISTICS
	;; Z FRAME == TORQUE TUBE CENTER (SAME AS THE BOM), MIN COTT == WINDOW, EDGE CLEARANCE OR FLOOD, CUT(-) / FILL(+) == TO THE OK BAND

	(defun PilePlace ( pd tre / cot rev sts )
		(setq
			cot (nth 14 pd)
			rev (nth 15 pd)
			sts (cond ((nth 21 pd)) (""))
		)
		(entmakex
			(append
				(list
				(cons 0 "MTEXT")
				(cons 100 "AcDbEntity")
				(cons 8 (ClassLayer (nth 6 pd)))
				(cons 100 "AcDbMText")
				(list 10 (nth 8 pd) (nth 9 pd) 0.0)
				(cons 40 hgt)
				(cons 71 5) ;; MIDDLE CENTER, THE LABEL IS CENTERED ON THE PILE
				(cons 1
					(strcat
						(itoa (nth 1 pd)) " - " (itoa (nth 2 pd))
						(if cot (strcat "\\PCOTT " (rtos cot 2 3) " ft.") "")
						(if rev (strcat "\\P" (rtos rev 2 3) " ft." (if (= "ESTIMATE" (nth 11 pd)) " (est)" "") (if (member sts (list "" "OK")) "" (strcat " " sts))) "")
					)
				) ;; LINE 1 == TAG (TABLE - POLE), LINE 2 == COTT (GROUND TO TORQUE TUBE CENTER == REVEAL + COTT TO PILE TOP OFFSET),
				;; LINE 3 == PILE REVEAL (TOP OF PILE ABOVE GROUND), FLAGGED "(est)" FOR A TOPO ESTIMATE AND WITH THE COTT CHECK WHEN OUT OF ITS WINDOW
				)
				(if msk
					(list
						(cons 90 19) ;; 1 + 2 == BACKGROUND MASK IN THE DRAWING WINDOW COLOR (HIDES LINEWORK BEHIND THE LABEL), 16 == TEXT FRAME (AUTOCAD 2016+)
						(cons 63 256) ;; MASK COLOR, NOT USED WITH THE WINDOW COLOR BUT AUTOCAD EXPECTS IT
						(cons 45 1.25) ;; MASK BORDER OFFSET FACTOR (x TEXT HEIGHT), THE FRAME IS DRAWN AT THE TEXT EXTENTS
						(cons 441 0) ;; MASK TRANSPARENCY, NONE
					)
				) ;; SETTING "pile_label_mask" "0" TURNS THE MASK AND FRAME OFF
				(list (PPP:PileXdata pd))
			)
		)
		(setq
			cnt (1+ cnt)
			xrws (cons (PPP:BomRow pd tre) xrws) ;; THE SAME ROW PPPBOM BUILDS FROM THE LABEL
			ccn (std:AssocIncrement ccn (nth 6 pd)) ;; PILES PER CLASS
			hsc (std:AssocIncrement hsc (cond ((nth 11 pd)) ("NONE"))) ;; PILES PER HEIGHTS SOURCE (SEE PPP:HeightsSource)
		)
	)
	;; PLACES ONE PILE LABEL (MTEXT WITH 'PPPPILE' XDATA) AND ADDS ITS BOM ROW
	;; [ pd ] == PILE DATA (PileRecord), [ tre ] == THE TRACKER'S RECOMMENDATION (PPP:TrackerRecommendation)


	(setq
		sta (null ctx) ;; T WHEN RUN AS THE PPPPILES COMMAND
		doc (vla-get-activedocument (vlax-get-acad-object))
		tim (getvar "MILLISECS")
	)
	(setq funcs
		(list
			'json:Get
			'json:Items
			'std:AssocSet
			'std:AssocIncrement
			'std:SafeNth
			'std:CreateDirectoryPath
			'std:MatchPoints
			'std:ParseNumberList
			'std:ReplaceNth
			'std:SlopeToDegrees
			'std:PolylineElevation
			'std:PolylinePoints
			'std:VerticalCrossings
			'std:GroupRuns
			'wsh:ReturnFolder
			'wsh:ConfirmationDialog
			'json:StringSplit
			'json:StringJoin
		)
	) ;; msxl:ExportTable AND msxl:ReadTables ARE OPTIONAL, PPP:WriteBom AND THE READERS REPORT IT IF LIB-MSXL IS NOT LOADED
	(cond
		((PPP:MissingFunctions funcs)
			nil ;; PPP:MissingFunctions PRINTED THE MISSING NAMES
		)
		
		;; PROJECT CONTEXT, FROM AN EARLIER STAGE OR READ FROM THE PPP-BOM ROOT WHEN RUN AS A COMMAND
		((and sta (null (setq ctx (PPP:ProjectContext))))
			nil ;; PPP:ProjectContext PRINTED THE REASON
		)
		;; TRACKERS PLACED BY AN EARLIER RUN ARE FLATTENED FIRST, PILE POSITIONS ARE READ FROM FLAT PPP BLOCKS (SEE PPP:FlattenTrackers)
		((null
				(progn
					(setq
						ppp (cdr (assoc "ppp" ctx))
						cfg (cdr (assoc "cfg" ctx))
						prs (cdr (assoc "presets" ctx))
					)
					(if sta
						(progn
							(vla-startundomark doc)
							(setq und t)
						)
					) ;; ONE UNDO GROUP FOR THE FLATTEN, THE LABELS AND THE NEW PLACEMENT
					(PPP:DebugStep "PPPPILES" "checking the X-ARRAY and Civil topo files")
					(setq
						scm (PPP:SourceCheck doc cfg t) ;; A CHANGED CIVIL TOPO IS RELOADED, A CHANGED X-ARRAY IS REPORTED
						cfg (car scm)
						scm (cadr scm)
					)
					(prompt scm) ;; SHOWN NOW, BEFORE THE OPTIONS DIALOG, AND AGAIN IN THE SUMMARY
					(setq plc (PPP:FlattenTrackers doc))
					(PPP:DebugStep "PPPPILES" "project data loaded, collecting assigned array blocks")
					(setq trk (CollectTrackers))
				)
			)
			(if plc
				(PPP:RestoreTrackers plc nil)
			)
			(prompt "\nNo assigned array blocks were found, run PPPASSIGN first.")
		)
	
		;; OPTIONS: CIVIL GRADING OUTPUT, MATCH TOLERANCE, OPTIMIZE FOR, FLOOD FILE, PILE DRIVE FILE (ONLY ASKED WHEN RUN AS THE COMMAND)
		((not
				(progn
					(setq
						cvo (cond
								((and (= 'STR (type (cdr (assoc "civil_output" cfg)))) (vl-file-directory-p (cdr (assoc "civil_output" cfg))))
									(cdr (assoc "civil_output" cfg))
								)
								((PPP:FindCivilOutput ppp))
							)
						cvf (PPP:CivilFiles cvo (cdr (assoc "civil_files" cfg)))
						cvs (CivilSelection) ;; THE WORKBOOKS (AREAS) TO CALCULATE
						udc (cond ((null cvf) "0") ((= "0" (cdr (assoc "civil_use" cfg))) "0") ("1"))
						mtl (PPP:SettingNumber cfg "civil_tolerance" 1.0) ;; CIVIL AND FLOOD MATCH DISTANCE (FT)
						tol (rtos mtl 2 2)
						upd (if (= "0" (cdr (assoc "pile_drive" cfg))) "0" "1")
						gol (PPP:SettingText cfg "optimize_for" "least_grading")
						sbd (if (= "0" (cdr (assoc "solve_band" cfg))) "0" "1")
						flp (if (and (= 'STR (type (cdr (assoc "flood_file" cfg)))) (findfile (cdr (assoc "flood_file" cfg)))) (cdr (assoc "flood_file" cfg)))
					)
					(if (not (member gol (list "least_grading" "fewest_piles" "civil")))
						(setq gol "least_grading")
					)
					(if sta (OptionsDialog) t)
				)
			)
			(if plc
				(PPP:RestoreTrackers plc nil)
			) ;; NOTHING CHANGES ON A CANCEL, THE TRACKERS GO BACK WHERE THEY WERE
			(prompt "\nPPPPiles was cancelled, nothing was changed.")
		)
	
		( t
			(setq tim (getvar "MILLISECS")) ;; RUN TIME STARTS AFTER THE OPTIONS DIALOG
			(PPP:DebugStep "PPPPILES" (strcat (itoa (length trk)) " trackers, numbering and matching areas"))
			(setq
				ord (NumberTrackers trk)
				ars (MatchAreas trk (CollectAreaLabels))
				arw (car ars)
				hgt (PPP:SettingNumber cfg "pile_label_height" 1.5)
				msk (/= "0" (PPP:SettingText cfg "pile_label_mask" "1")) ;; BACKGROUND MASK AND TEXT FRAME ON THE PILE LABELS, ON UNLESS SET TO "0"
				cwn (CottWindow)
				ctl (PPP:SettingNumber cfg "tolerance" 0.04) ;; COTT TOLERANCE OUTSIDE THE WINDOW BEFORE A PILE IS LOW / HIGH (PROJECT SETTING)
				smx (sin (* pi (/ (cond ((PPP:SettingNumber cfg "table_slope_deg" nil)) ((std:SlopeToDegrees (PPP:SettingNumber cfg "table_slope" 8.0)))) 180.0))) ;; SIN(MAX TILT)
				ofa (PPP:SettingNumber cfg "cott_reveal_offset" 0.75) ;; COTT - REVEAL, ARRAY PILES
				ofm (PPP:SettingNumber cfg "cott_reveal_offset_motor" ofa) ;; COTT - REVEAL, MOTOR PILE
				anc (strcase (PPP:SettingText cfg "anchor_pile" "Motor"))
				clr (PPP:SettingNumber cfg "edge_clearance" 2.5) ;; LOWEST MODULE EDGE AT MAX TILT ABOVE THE FLOOD SURFACE + FREEBOARD
				fbd (PPP:SettingNumber cfg "flood_freeboard" 0.0)
				alw (PPP:SettingNumber cfg "allowed_flood" 0.0) ;; FLOOD DEPTH THE WINDOW MINIMUM ALREADY CLEARS (COTTZILLA "ALLOWED FLOODING")
			)
			(if (= "civil" gol)
				(if (/= "1" udc) (setq gol "least_grading"))
			) ;; CIVIL COTT NEEDS CIVIL DATA, AN ESTIMATE IS ALWAYS SOLVED
			(setq cfg
				(std:AssocSet
					(std:AssocSet
						(std:AssocSet
							(std:AssocSet
								(std:AssocSet
									(std:AssocSet
										(std:AssocSet
											(std:AssocSet cfg "civil_output" (cond (cvo) ("")))
											"civil_selected" (json:StringJoin (mapcar '(lambda ( pth ) (strcat (vl-filename-base pth) (vl-filename-extension pth))) cvs) "|")
										)
										"civil_use" udc
									)
									"civil_tolerance" tol
								)
								"pile_drive" upd
							)
							"optimize_for" gol
						)
						"solve_band" sbd
					)
					"flood_file" (cond (flp) (""))
				)
			)
			(if sta
				(PPP:SaveSettings (strcat ppp "\\" (PPP:SettingsFile)) cfg)
			) ;; THE OPTIONS ARE KEPT FOR THE NEXT RUN (PPPSETUP KEEPS THESE KEYS TOO)
	
			;; CIVIL GRADING OUTPUT AND FLOOD DEPTHS
			(if (= "1" udc)
				(progn
					(PPP:DebugStep "PPPPILES" (strcat "reading " (itoa (length cvs)) " Civil output files (Excel)"))
					(setq
						cvd (if (std:FunctionLoaded 'msxl:ReadTables) (CivilData cvs) (list nil (list "LIB-MSXL is not loaded, Civil output was not read")))
						civ (car cvd)
					)
				)
			)
			(if flp
				(setq fld (if (std:FunctionLoaded 'msxl:ReadTables) (FloodData flp) (list nil "LIB-MSXL is not loaded, the flood file was not read")))
			)
	
			;; PASS 1: PILE POSITIONS, NUMBERS AND CLASSES (PPP'S OWN DATA, FROM THE PRESETS), FLAT FOR NOW
			(PPP:DebugStep "PPPPILES" "calculating pile positions")
			(foreach itm ord
				(setq
					tbl (car itm)
					itm (cdr itm)
					ext (nth 2 itm)
					mot (nth 8 itm)
					npl (1- (length (nth 7 itm))) ;; THE LAST DISTANCE IS THE SOUTH END OF THE TUBE, NOT A PILE
					cod (cond ((cdr (assoc (strcase (nth 4 itm)) arw))) (""))
					cum 0.0
					pos 0
				)
				(foreach dst (reverse (cdr (reverse (nth 7 itm))))
					(setq
						pos (1+ pos)
						cum (+ cum dst)
						pls
							(cons
								(list
									(nth 5 itm) (- (nth 6 itm) cum) ;; X Y FIRST, FOR std:MatchPoints
									tbl pos (PileClass ext pos npl mot (nth 3 itm)) (strcat (itoa tbl) "-" (itoa pos)) cod (nth 1 itm) ext
									(if (and mot (= pos mot)) 1 0)
									(nth 10 itm) ;; HANDLE OF THE ARRAY BLOCK
									cum ;; DISTANCE ALONG THE TUBE FROM ITS NORTH END
									itm ;; THE TRACKER (SEE CollectTrackers)
								)
								pls
							)
					)
				)
			)
			(setq pls (reverse pls))
	
			;; PASS 2: GROUND PER PILE, FROM CIVIL MATCHED BY X / Y, THEN THE CIVIL TOPO ESTIMATE (EXISTING GRADE) FOR EVERY PILE CIVIL DOES NOT COVER
			(setq
				mch (if civ
						(progn
							(PPP:DebugStep "PPPPILES" (strcat "matching " (itoa (length pls)) " piles to " (itoa (length civ)) " Civil piles"))
							(std:MatchPoints pls civ mtl)
						)
						(list (mapcar '(lambda ( p ) nil) pls) 0) ;; SAME SHAPE AS std:MatchPoints, NOTHING MATCHED
					) ;; ( MATCHES COUNT ), ONE ( DISTANCE . GROUND-ROW ) OR NIL PER PILE
				why (cond
						((and civ (< 0 (cadr mch))) nil)
						(civ (strcat "no PPP pile is within " tol " ft of a Civil pile, other layout revision?"))
						((= "1" udc) "no usable Civil piles were read from the Civil files")
						(cvf "Civil grading output was turned off in the options")
						("no Civil grading output files were found")
					)
			)
	
			;; ONLY SOME WORKBOOKS SELECTED: TRACKERS WITHOUT A CIVIL MATCH ARE IN OTHER AREAS, THEY ARE LEFT AS THEY ARE (LABELS AND PLACEMENT FROM AN EARLIER RUN ARE KEPT)
			;; NUMBERING WAS DONE ON EVERY TRACKER FIRST, SO TABLE NUMBERS MATCH A FULL RUN
			(setq ntr (length ord))
			(if (and civ (< (length cvs) (length cvf)))
				(progn
					(PPP:DebugStep "PPPPILES" "keeping the trackers in the selected areas")
					(setq
						kpt (MatchedTrackers (mapcar 'cons pls (car mch)))
						ntr (length (cadr kpt))
						hds (cond ((cadr kpt)) ((list ""))) ;; NOTHING MATCHED: "" IS NO HANDLE, SO NO EARLIER LABEL IS CLEARED
						pls (mapcar 'car (car kpt))
						mch (list (mapcar 'cdr (car kpt)) (cadr mch))
						kpt nil
					)
				)
			)
	
			;; PILES WITHOUT CIVIL GROUND (ALL OF THEM WITHOUT CIVIL OUTPUT): ESTIMATED FROM THE CIVIL TOPO XREF, EACH ONE FLAGGED "ESTIMATE"
			;; (LABEL "(est)", BOM ROW "ESTIMATE - NOT CIVIL", ESTIMATE BOM FILE NAME, WARNING POPUP), SEE PPP:HeightsWarning
			(if (setq gap (vl-remove nil (mapcar '(lambda ( p m ) (if (null m) p)) pls (car mch))))
				(progn
					(PPP:DebugStep "PPPPILES" (strcat "estimating ground for " (itoa (length gap)) " piles from the Civil topo"))
					(setq
						est (ContourHeights gap)
						erw (car est) ;; ONE ROW OR NIL PER GAP PILE, IN PILE ORDER
						mch
							(list
								(mapcar
									'(lambda ( m / row )
										(if m
											m
											(progn
												(setq row (car erw) erw (cdr erw))
												(if row (cons 0.0 row)) ;; SAME SHAPE AS A CIVIL MATCH, SO BOTH SOURCES SHARE THE CODE BELOW
											)
										)
									)
									(car mch)
								)
								(cadr mch)
							)
						gap nil
						erw nil
					)
				)
			)
			(setq fmc
				(if (car fld)
					(car (std:MatchPoints pls (car fld) mtl))
					(mapcar '(lambda ( p ) nil) pls)
				)
			) ;; FLOOD DEPTH PER PILE, MATCHED BY X / Y LIKE CIVIL ( DISTANCE X Y DEPTH ), NIL WHEN THE PILE HAS NONE
	
			;; PASS 3: ONE STRAIGHT TORQUE TUBE PER TRACKER (SEE PPP:SolveTable), THEN PILE POSITIONS, LABELS AND BOM ROWS
			(PPP:DebugStep "PPPPILES" (strcat "solving " (itoa ntr) " trackers (" gol ")"))
			(PPP:DebugStep "PPPPILES" "clearing pile labels from an earlier run")
			(setq old (ClearPiles hds))
			(PPP:DebugStep "PPPPILES" (strcat (itoa (car old)) " earlier labels erased, creating layers"))
			(MakeLayers)
			(regapp "PPPPILE") ;; RETURNS NIL IF ALREADY REGISTERED, WHICH IS FINE
			(setq cnt 0 inc 0 sdx 0.0 sdy 0.0 mxd 0.0 grd 0.0 tgr 0 nlo 0 ntd 0)
			(foreach grp (std:GroupRuns (mapcar 'list pls (car mch) fmc) (function (lambda ( itm ) (nth 10 (car itm))))) ;; ONE TRACKER'S ( PILE MATCH FLOOD ) ITEMS, NORTH TO SOUTH
				(setq ntd (1+ ntd))
				(if (= 0 (rem ntd 250))
					(PPP:DebugStep "PPPPILES" (strcat "  " (itoa ntd) " of " (itoa ntr) " trackers solved and labeled"))
				)
				(setq res (SolveTracker grp))
				(if (cadr res)
					(setq tsv (cons (cadr res) tsv))
				) ;; ( ENAME DZ DY ANGLE ) TO PLACE THE BLOCK AT THE END
				(setq
					pds (mapcar 'PileRecord (car res)) ;; ONE PILE DATA PER PILE, SEE SolveTracker AND PileRecord
					tre (PPP:TrackerRecommendation pds)
				)
				(if (= "GRADE" tre)
					(setq tgr (1+ tgr))
				)
				(foreach pd pds
					(PilePlace pd tre)
				)
			)
	
			;; TRACKER BLOCKS AT THEIR SOLVED HEIGHT AND TILT, TRACKERS NOT SOLVED THIS RUN GO BACK TO THEIR EARLIER PLACEMENT
			(PPP:DebugStep "PPPPILES" "placing tracker blocks at their heights")
			(setq nfp 0)
			(foreach itm tsv
				(if (vl-catch-all-error-p (vl-catch-all-apply 'PPP:PlaceTracker (cons (vlax-ename->vla-object (car itm)) (cdr itm))))
					(setq nfp (1+ nfp)) ;; LIKELY A LOCKED LAYER
				)
			)
			(if plc
				(PPP:RestoreTrackers plc (mapcar 'car tsv))
			)
			(setq
				pls nil ;; FREE THE MEMORY
				src (PPP:HeightsSource hsc) ;; "ESTIMATE" WHEN ANY PILE IS ESTIMATED, SO THE BOM GETS THE ESTIMATE FILE NAME
			)
			(if und
				(progn
					(vla-endundomark doc)
					(setq und nil)
				)
			)
			
			;; OUTPUT FILES (PPP:WriteBom), AN OPEN FILE CAN BE CLOSED AND RETRIED HERE, OR THE FILES WRITTEN LATER WITH PPPBOM FROM THE PILE LABELS
			(setq xrws (reverse xrws))
			(while
				(and
					(setq bom (PPP:WriteBom ppp cfg xrws upd hds src))
					sta
					(not (car bom))
					(= 4 ;; 4 == RETRY
						(wsh:ConfirmationDialog
							(strcat
								"The pile labels are placed, but the output files were not saved:\n"
								(cadr bom)
								"\n\nClose the file in Excel and click Retry.\n"
								"Or click Cancel and run PPPBOM later, it writes the files from the pile labels without solving again."
							)
							"PPP Piles - BOM Not Saved" 0 53 ;; 53 == RETRY / CANCEL + WARNING ICON
						)
					)
				)
			) ;; WRITTEN AGAIN ON EVERY RETRY
			(if (not (car bom))
				(setq bom (list nil (strcat (cadr bom) "\n  Run PPPBOM to write the output files from the pile labels (no solve needed).")))
			)
	
			(foreach itm trk
				(if (and (nth 11 itm) (not (assoc (nth 1 itm) stw)))
					(setq stw (cons (cons (nth 1 itm) (+ clr (nth 11 itm))) stw))
				)
			) ;; EDGE CLEARANCE + MODULE DROP PER PRESET, THE STOW MINIMUM COTT BEFORE FLOOD
			(setq cck
				(if hst
					(strcat
						"\n    Solved: "
						(cdr (assoc gol (list (cons "least_grading" "least grading") (cons "fewest_piles" "fewest piles out of range") (cons "civil" "Civil COTT as delivered, not optimized"))))
						(if (= "civil" gol) "" (strcat (if (= "1" sbd) ", tolerance used only where a tracker cannot fit" ", strict window only") ", max tilt " (rtos (* 180.0 (/ (atan smx (sqrt (- 1.0 (* smx smx)))) pi)) 2 2) " deg"))
						", anchor pile " (strcase (PPP:SettingText cfg "anchor_pile" "Motor") t)
						"\n    COTT - reveal: " (rtos (car ofs) 2 3) " to " (rtos (cadr ofs) 2 3) " ft"
						"\n    COTT check (window " (if cwn (strcat (rtos (car cwn) 2 2) "-" (rtos (cadr cwn) 2 2)) "not set") " ft, tolerance " (rtos ctl 2 2) " ft):"
						(apply 'strcat (mapcar '(lambda ( p ) (strcat " " (if (= "" (car p)) "no window" (car p)) " " (itoa (cdr p)))) hst))
						"\n    Grading: " (itoa tgr) " trackers, " (rtos grd 2 2) " ft in total (CUT(-) / FILL(+) per pile in the BOM)"
						(if fld (strcat "\n    Flood: " (cadr fld)) "\n    Flood: no flood file, every pile uses the window minimum")
						(if (and fld stw)
							(strcat
								"\n    Flooded piles need COTT >= flood depth + " (rtos (+ fbd clr) 2 2) " ft (freeboard + edge clearance) + module drop at max tilt: "
								(json:StringJoin (mapcar '(lambda ( p ) (strcat (car p) " " (rtos (- (cdr p) clr) 2 2))) (vl-sort stw '(lambda ( a b ) (< (car a) (car b))))) ", ")
								" ft"
							)
							""
						)
						(if (< 0 nlo) (strcat "\n    Min COTT raised above the window by flood on " (itoa nlo) " piles") "")
						"\n    Tracker blocks placed at their height and tilt: " (itoa (- (length tsv) nfp))
						(if (< 0 nfp) (strcat "\n    Warning: " (itoa nfp) " tracker blocks could not be placed (locked layer?)") "")
					)
					""
				)
			) ;; SOLVE SUMMARY, ONLY WHEN SOME PILES HAVE HEIGHTS
	
			(prompt
				(strcat
					"\nPPP piles: " (itoa cnt) " piles placed on " (itoa ntr) " trackers ("
					(rtos (/ (- (getvar "MILLISECS") tim) 1000.0) 2 1) " s)."
					scm ;; SOURCE FILE CHANGES (SEE PPP:SourceCheck)
					(if (< 0 (car old)) (strcat "\n  Replaced " (itoa (car old)) " pile labels from an earlier run.") "")
					(if (< 0 (cadr old)) (strcat "\n  Warning: " (itoa (cadr old)) " earlier pile labels could not be erased (locked layer?).") "")
					"\n  Areas: " (itoa (length arw)) " matched to area labels"
					(apply 'strcat (mapcar '(lambda ( msg ) (strcat "\n  Warning: " msg)) (cadr ars)))
					(if (= "1" udc)
						(strcat
							"\n  Civil grading output: " (cond (cvo) (""))
							(if hds (strcat "\n    Selected " (itoa (length cvs)) " of " (itoa (length cvf)) " workbooks: " (itoa (- (length ord) ntr)) " trackers in other areas were left as they are") "")
							(apply 'strcat (mapcar '(lambda ( msg ) (strcat "\n    " msg)) (cadr cvd)))
						)
						""
					)
					(if (assoc "CIVIL" hsc)
						(strcat
							"\n  Heights: Civil grading output"
							"\n    Matched: " (itoa (cdr (assoc "CIVIL" hsc))) " of " (itoa cnt) " PPP piles, " (itoa (cadr mch)) " of " (itoa (length civ)) " Civil piles"
							(if (< (cadr mch) (length civ)) (strcat "\n    Warning: " (itoa (- (length civ) (cadr mch))) " Civil piles match no PPP pile (other layout revision?)") "")
							"\n    PPP - Civil position: average X " (rtos (/ sdx (cdr (assoc "CIVIL" hsc))) 2 3) " ft, Y " (rtos (/ sdy (cdr (assoc "CIVIL" hsc))) 2 3) " ft, largest " (rtos mxd 2 3) " ft"
							(if (< 0 inc) (strcat "\n    Warning: " (itoa inc) " Civil piles where Z frame - Z enter is not the pile height (Civil data check)") "")
						)
						(strcat "\n  Heights: no Civil grading data (" (cond (why) ("no Civil pile matched")) ")")
					)
					(if est
						(strcat
							"\n  Civil topo estimate" (if civ (strcat " (piles with no Civil pile within " tol " ft)") "") ":"
							(apply 'strcat (mapcar '(lambda ( msg ) (if (= "" msg) "" (strcat "\n    " msg))) (cadr est)))
						)
						""
					)
					cck
					(PPP:HeightsWarning hsc nil)
					"\n  Pile classes:"
					(apply 'strcat
						(mapcar
							'(lambda ( cls / hit )
								(if (setq hit (assoc cls ccn))
									(strcat "\n    " (PPP:PileTypeName cls) " (" cls "): " (itoa (cdr hit)))
									""
								)
							)
							(list "HMP" "HAP" "SMP" "SAP" "SAPE" "SAPEND")
						)
					)
					(if (< 0 una) (strcat "\n  Warning: " (itoa una) " array blocks are not assigned and have no piles, run PPPASSIGN.") "")
					(if (< 0 nop) (strcat "\n  Warning: " (itoa nop) " assigned blocks use a preset that is missing or has no pile spacing.") "")
					(if (< 0 leg) (strcat "\n  Warning: " (itoa leg) " blocks were assigned before end exposure was added, run PPPASSIGN again for edge piles.") "")
					(if (< 0 rpl) (strcat "\n  Warning: " (itoa rpl) " assigned blocks are not PPP blocks (not replaced, locked layer?), run PPPASSIGN again. They have no piles.") "")
					(cadr bom)
				)
			)
			(if (and sta (= "ESTIMATE" src))
				(wsh:ConfirmationDialog
					(strcat (PPP:HeightsWarning hsc t) (if why (strcat "\n\nCivil grading data: " why ".") ""))
					"PPP Piles - Heights Not From Civil" 0 48 ;; 48 == OK + WARNING ICON
				)
			) ;; THE COMMAND LINE IS EASY TO MISS AFTER A LONG RUN, SO AN ESTIMATE ALSO STOPS FOR A WARNING
			(setq ctx
				(std:AssocSet ctx "piles"
					(list (cons "piles" cnt) (cons "trackers" ntr) (cons "heights" src))
				)
			)
		)
	)
	(if und
		(progn
			(vla-endundomark doc)
			(setq und nil)
		)
	) ;; A RUN THAT STOPPED AFTER THE TRACKERS WERE FLATTENED (EX. OPTIONS CANCELLED)
	ctx
)
;; RETURNS THE PROJECT CONTEXT WITH "piles" ADDED ( ("piles" . N) ("trackers" . N) ("heights" . "CIVIL" / "ESTIMATE" / NIL) ), SEE PPP:ProjectContext
;; A STAGE CALLING PPPPILES (NO DIALOG, NO POPUP) MUST TELL THE USER WHEN "heights" IS "ESTIMATE"
;; [ ctx ] == PROJECT CONTEXT FROM AN EARLIER STAGE, OR NIL TO READ IT FROM THE PPP-BOM ROOT



;;; PPP BOM ;;;
;; WRITES THE PILE DRIVE FILE AND THE BOM FROM THE PILE LABELS PPPPILES PLACED ('PPPPILE' XDATA), NOTHING IS SOLVED AND THE DRAWING IS NOT CHANGED
;; - REFUSES TO RUN WHEN THE DRAWING HAS NO PPPPILES PILE LABELS (RUN PPPPILES FIRST)
;; - THE FALLBACK WHEN PPPPILES COULD NOT SAVE THE FILES (EX. THE BOM WAS OPEN IN EXCEL), OR TO WRITE THEM AGAIN
;; - ALWAYS THE WHOLE DRAWING (EVERY LABEL, ALSO AFTER SELECTED AREAS RUNS), SAME FILE NAMES AND COLUMNS AS PPPPILES (PPP:WriteBom)
;; - WARNS WHEN A LABEL'S ARRAY BLOCK NO LONGER EXISTS (LABELS OUT OF DATE) AND WHEN THE HEIGHTS ARE CONTOUR ESTIMATES

(defun PPPBomMain
	(
		/
		;Functions
		*error*
		;Variables
		des doc tim funcs ctx ppp cfg pds tre rws ent hsc ntr stl res
	)

	(defun *error* ( msg / )
		(if (eq (type des) 'FILE)
			(close des)
		) ;; BY DYNAMIC SCOPE THIS CLOSES A FILE LEFT OPEN BY A HELPER THAT NAMES ITS HANDLE 'des' (EX. json:ReadFile)
		(PPP:ErrorReport msg "\nPPP BOM cancelled, nothing was changed.")
	)

	(setq
		doc (vla-get-activedocument (vlax-get-acad-object))
		tim (getvar "MILLISECS")
		funcs (list 'std:CreateDirectoryPath 'std:IsFileLocked 'std:GroupRuns 'std:AssocIncrement)
	) ;; msxl:ExportTable IS OPTIONAL, PPP:WriteBom REPORTS IT IF LIB-MSXL IS NOT LOADED
	(cond
		((PPP:MissingFunctions funcs)
			nil ;; PPP:MissingFunctions PRINTED THE MISSING NAMES
		)
		((null (setq ctx (PPP:ProjectContext)))
			nil ;; PPP:ProjectContext PRINTED THE REASON
		)
		((null (setq pds (mapcar 'cdr (PPP:PileLabels doc))))
			(prompt "\nPPPBOM: this drawing has no PPPPILES pile labels, run PPPPILES first. Nothing was written.")
		)
		( t
			(setq
				ppp (cdr (assoc "ppp" ctx))
				cfg (cdr (assoc "cfg" ctx))
				ntr 0 stl 0
			)
			(foreach grp (std:GroupRuns (vl-sort pds '(lambda ( a b ) (if (= (nth 1 a) (nth 1 b)) (< (nth 2 a) (nth 2 b)) (< (nth 1 a) (nth 1 b))))) 'cadr) ;; TABLE, THEN POLE (PPPPILES ORDER), ONE TRACKER PER RUN
				(setq
					tre (PPP:TrackerRecommendation grp)
					ntr (1+ ntr)
				)
				(if (not (and (nth 10 (car grp)) (setq ent (handent (nth 10 (car grp)))) (entget ent)))
					(setq stl (1+ stl))
				) ;; THE ARRAY BLOCK WAS ERASED OR REPLACED SINCE PPPPILES
				(foreach pd grp
					(setq
						hsc (std:AssocIncrement hsc (cond ((nth 11 pd)) ("NONE"))) ;; PILES PER HEIGHTS SOURCE (SEE PPP:HeightsSource)
						rws (cons (PPP:BomRow pd tre) rws)
					)
				)
			)
			(setq
				pds nil
				res (PPP:WriteBom ppp cfg (reverse rws) (if (= "0" (cdr (assoc "pile_drive" cfg))) "0" "1") nil (PPP:HeightsSource hsc))
			)
			(prompt
				(strcat
					"\nPPP BOM: " (itoa (length rws)) " piles on " (itoa ntr) " trackers, read from the pile labels ("
					(rtos (/ (- (getvar "MILLISECS") tim) 1000.0) 2 1) " s)."
					(PPP:HeightsWarning hsc nil)
					(if (< 0 stl) (strcat "\n  Warning: " (itoa stl) " trackers have labels but their array block no longer exists, run PPPPILES again.") "")
					(cadr res)
				)
			)
		)
	)
	(princ)
)
;; THE PPPBOM COMMAND, RUN AFTER PPPPILES (THE PILE LABELS HOLD ALL THE BOM DATA)
;; !REVISIT! LABELS EDITED OR MOVED BY HAND ARE NOT DETECTED, ONLY ERASED ARRAY BLOCKS


;;; NOTES ;;;

;; V-TOPO-MAJR, V-TOPO-MINR - LAYERS FOR CONTOUR LINES (DEFAULT IN SETTINGS DIALOG, EDITABLE PER PROJECT)


;; BOOKMARK
;; ADD 'PPPUPDATEARRAYLAYOUT' FOR UPDATED X-ARRAY FILES (PPP:SourceCheck REPORTS THEM), AN UPDATED CIVIL TOPO IS AN XREF RELOAD
;; ADD A SEPARATE FUNCTION FOR PILE COORDINATIONS


; (Load:DescriptionLog "PPPSETUP" "Sets up pile plot plan drawing from the X-ARRAY and the Civil topo xref")
; (Load:DescriptionLog "PPPASSIGN" "Assigns array blocks to tracker presets and interior / exterior row types")
; (Load:DescriptionLog "PPPPILES" "Places pile labels with pile data and writes the pile drive file and BOM")
; (Load:DescriptionLog "PPPBOM" "Writes the pile drive file and BOM from the pile labels placed by PPPPILES")
