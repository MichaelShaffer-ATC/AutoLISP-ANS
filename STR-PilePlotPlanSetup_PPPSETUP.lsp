;; AUTOMATION FOR CREATING / STANDARDIZING PPP/BOM FILES
;; COMMANDS: PPPSETUP (NEW PPP SETUP DRAWING), PPPASSIGN (ARRAY BLOCKS TO TRACKER PRESETS, ALSO RUN AT THE END OF PPPSETUP)
;; FILE MAKES OUTSIDE CALLS TO LIBRARY FILE FOR WSH FUNCTIONS (LIB - WSH WindowsScriptHosting.lsp)
;; FILE MAKES OUTSIDE CALLS TO LIBRARY FILE FOR JSON FUNCTIONS (LIB-JSON_Functions.lsp), USED FOR TRACKER PRESETS

;; STEPS:
;; - SELECT PROJECT ROOT AND ESTABLISH PPP-BOM FOLDER STRUCTURE UNDER STRUCTURAL
;; - LOAD PROJECT SETTINGS (IF THEY EXIST) AND DISPLAY SETTINGS DIALOG FOR USER INPUT
;; - MANAGE TRACKER PRESETS IN A NESTED DIALOG, SAVED TO PPP-PRESETS.json IN THE PPP-BOM ROOT (PVCASE .PVsettings FILES CAN BE IMPORTED)
;; - FIND X-ARRAY FILE FROM ELECTRICAL LOCATION (USER CONFIRMS OR SELECTS), COPY AND FLATTEN ARRAY OBJECTS TO Z = 0
;; - FIND AND COPY CONTOUR FILE FROM CIVIL LOCATION
;; - VALIDATE CONTOUR ELEVATIONS AND ARRAY / CONTOUR ALIGNMENT, OPTIONALLY IGNORE ARRAYS OUTSIDE THE CONTOURS
;; - ASSIGN ARRAY BLOCKS TO TRACKER PRESETS AND INTERIOR / EXTERIOR ROW TYPES (PPPASSIGN, SEE THE PPP ASSIGN SECTION)
;; - SET DRAWING UNITS TO FEET, SAVE DRAWING AND PROJECT SETTINGS TO PPP-BOM LOCATION
;;
;; FILE LAYOUT: COMMANDS, SHARED 'PPP:' FUNCTIONS (PRESETS, PROJECT FILES, ARRAY BLOCKS), PPP SETUP, PPP ASSIGN


;;; COMMANDS ;;;

(defun c:PPPSetup ( / )
	; (ECHO:LogWrapper "STR-PilePlotPlanSetup_PPPSETUP.lsp" "PILEPLOTPLANSETUP" PPPSetupMain)
	(PPPSetupMain)
)

(defun c:PPPAssign ( / )
	; (ECHO:LogWrapper "STR-PilePlotPlanSetup_PPPSETUP.lsp" "PPPASSIGN" (function (lambda ( ) (PPPAssignMain nil nil nil))))
	(PPPAssignMain nil nil nil)
	(princ)
)
;; NIL ARGUMENTS == RUN AS A COMMAND, PROJECT SETTINGS AND PRESETS ARE READ FROM THE PPP-BOM ROOT

(defun c:PPPDebugOn ( / )
	(setq *PPP:Trace* t)
)

(defun c:PPPDebugOff ( / )
	(setq *PPP:Trace* nil)
)
;; DEBUG FUNCTIONS, "ON" TURNS DEBUG STEPPING / TRACING ON, "OFF" TURNS IT OFF


;;; PPP SHARED FUNCTIONS - TRACKER PRESETS ;;;
;; FUNCTIONS PREFIXED WITH 'PPP:' ARE GLOBAL SO OTHER COMMANDS IN THIS FILE (EX. PPPASSIGN, PILE PLACEMENT) CAN USE THEM
;; TRACKER PRESETS ARE JSON OBJECTS (SEE LIB-JSON_Functions.lsp), KEY NAMES MATCH THE PVCASE .PVsettings TEMPLATES
;; PRESET GEOMETRY IS MEASURED ALONG THE TRACKER FROM THE NORTH END, PILE 1 IS THE NORTH PILE (MATCHES PVCASE POLE NUMBERING)

(defun PPP:PresetKeys ( / )
	(list
		"Name" "Manufacturer" "Description"
		"Columns" "SizeX" "SizeY" "ModuleGap" "JointGapSize" "MotorGapSize" "TiltAngle"
		"Packs" "PilingOverhangLeftSize" "PilingOverhangRightSize" "HorizontalDistances"
	)
)
;; RETURNS THE PRESET KEYS IN THE ORDER THEY ARE WRITTEN TO THE PRESET FILE
;; THE NAME PREFIX (INT- / EXT-) SETS THE BOM ROW TYPE, EVERY KEY AFTER "Description" IS TRACKER GEOMETRY

(defun PPP:PresetTolerance ( / )
	0.05
)
;; RETURNS THE LENGTH TOLERANCE (FT) USED FOR PRESET CHECKS (PILE SPACING TOTAL, MOTOR PILE POSITION)

(defun PPP:ParsePacks ( str / typ num itm lst bad )
	(if (= 'STR (type str))
		(foreach pck (json:StringSplit str ";")
			(if (/= "" (vl-string-trim " \t" pck))
				(progn
					(setq
						itm (json:StringSplit pck ",")
						typ (cdr (assoc (strcase (vl-string-trim " \t" (car itm))) (list (cons "MODULES" "Modules") (cons "JOINTGAP" "JointGap") (cons "MOTOR" "Motor"))))
						num (if (= 2 (length itm)) (distof (vl-string-trim " \t" (cadr itm)) 2))
					)
					(if (and typ num (>= num 1) (equal num (fix num) 1e-8))
						(setq lst (cons (cons typ (fix num)) lst))
						(setq bad t)
					)
				)
			)
		)
	)
	(if (and lst (not bad))
		(reverse lst)
	)
)
;; CONVERTS A PVCASE PACKS STRING INTO A LIST OF ( "TYPE" . COUNT ) PAIRS
;; EX. "Modules,6; Motor,1; Modules,6" -> (("Modules" . 6) ("Motor" . 1) ("Modules" . 6))
;; RETURNS NIL IF THE STRING IS EMPTY OR ANY PACK IS NOT VALID

(defun PPP:TrackerLayout ( pre / pck siz gap jnt mtr pos mid )
	(if (and
			(setq pck (PPP:ParsePacks (json:Get pre "Packs")))
			(vl-every '(lambda ( key ) (numberp (json:Get pre key))) (list "SizeY" "ModuleGap" "JointGapSize" "MotorGapSize"))
		)
		(progn
			(setq
				siz (json:Get pre "SizeY")
				gap (json:Get pre "ModuleGap")
				jnt (json:Get pre "JointGapSize")
				mtr (json:Get pre "MotorGapSize")
				pos 0.0
			)
			(foreach itm pck
				(cond
					((= "Modules" (car itm))
						(setq pos (+ pos (* (cdr itm) siz) (* (1- (cdr itm)) gap))) ;; NO MODULE GAP BETWEEN BACK TO BACK MODULE PACKS, MATCHES PVCASE
					)
					((= "JointGap" (car itm))
						(setq pos (+ pos (* (cdr itm) jnt)))
					)
					((= "Motor" (car itm))
						(if (null mid)
							(setq mid (+ pos (* 0.5 mtr)))
						)
						(setq pos (+ pos (* (cdr itm) mtr)))
					)
				)
			)
			(list pos mid)
		)
	)
)
;; RETURNS ( LENGTH MOTOR ) FOR A PRESET, BOTH IN FEET FROM THE NORTH END OF THE TRACKER
;; LENGTH == MODULES + MODULE GAPS + JOINT GAPS + MOTOR GAP, MOTOR == CENTER OF THE MOTOR GAP (NIL IF THE TRACKER HAS NO MOTOR)
;; RETURNS NIL IF THE PACKS OR MODULE DIMENSIONS ARE MISSING

(defun PPP:PilePositions ( pre / hd ol pos out )
	(setq
		hd (json:Items (json:Get pre "HorizontalDistances"))
		ol (json:Get pre "PilingOverhangLeftSize")
	)
	(if (and (= 'LIST (type hd)) (cdr hd) (vl-every 'numberp hd) (numberp ol))
		(progn
			(setq pos (- ol))
			(foreach dst (reverse (cdr (reverse hd))) ;; THE LAST DISTANCE IS THE SOUTH END MARGIN, NOT A PILE
				(setq out (cons (setq pos (+ pos dst)) out))
			)
			(reverse out)
		)
	)
)
;; RETURNS PILE OFFSETS (FT) FROM THE NORTH END OF THE TRACKER, PILE 1 FIRST
;; HORIZONTAL DISTANCES START AT THE NORTH OVERHANG LINE, SO PILE 1 SITS AT ( FIRST DISTANCE - NORTH OVERHANG )

(defun PPP:MotorPileIndex ( pre / mid pos idx dst best )
	(if (and (setq mid (cadr (PPP:TrackerLayout pre))) (setq pos (PPP:PilePositions pre)))
		(progn
			(setq idx 0)
			(foreach pnt pos
				(setq
					idx (1+ idx)
					dst (abs (- pnt mid))
				)
				(if (or (null best) (< dst (cdr best)))
					(setq best (cons idx dst))
				)
			)
			(if (<= (cdr best) (PPP:PresetTolerance))
				(car best)
			)
		)
	)
)
;; RETURNS THE 1-BASED INDEX OF THE MOTOR (GEAR RACK) PILE, THE PILE AT THE CENTER OF THE MOTOR GAP
;; MATCHES 'Gear Rack Num' IN THE PVCASE BOM, RETURNS NIL IF THE TRACKER HAS NO MOTOR OR NO PILE SITS AT THE MOTOR

(defun PPP:PairName ( nam / )
	(cond
		((/= 'STR (type nam))
			nil
		)
		((wcmatch (strcase nam) "INT-?*")
			(strcat "EXT-" (substr nam 5))
		)
		((wcmatch (strcase nam) "EXT-?*")
			(strcat "INT-" (substr nam 5))
		)
	)
)
;; RETURNS THE MATCHING INTERIOR / EXTERIOR PRESET NAME (EX. "INT-28M56" -> "EXT-28M56")
;; RETURNS NIL IF THE NAME HAS NO INT- / EXT- PREFIX

(defun PPP:FindPreset ( lst nam / )
	(if (= 'STR (type nam))
		(vl-some
			'(lambda ( pre / key )
				(if (and (= 'STR (type (setq key (json:Get pre "Name")))) (= (strcase key) (strcase nam)))
					pre
				)
			)
			lst
		)
	)
)
;; RETURNS THE PRESET WITH THE PASSED NAME FROM A LIST OF PRESETS (NOT CASE-SENSITIVE), ELSE NIL

(defun PPP:PresetProblem ( pre / nam num hd pck mdl lay tot ext )
	(setq
		nam (json:Get pre "Name")
		hd (json:Items (json:Get pre "HorizontalDistances"))
		pck (PPP:ParsePacks (json:Get pre "Packs"))
		mdl (apply '+ (mapcar '(lambda ( itm ) (if (= "Modules" (car itm)) (cdr itm) 0)) pck))
	)
	(cond
		((not (and (= 'STR (type nam)) (wcmatch (strcase nam) "INT-?*,EXT-?*")))
			"Preset name must start with INT- or EXT-, the prefix sets the BOM row type."
		)
		((not (and (numberp (setq num (json:Get pre "Columns"))) (>= num 1) (equal num (fix num) 1e-8)))
			"Modules per tracker must be a whole number of 1 or more."
		)
		((not (vl-every '(lambda ( key / val ) (and (numberp (setq val (json:Get pre key))) (> val 0))) (list "SizeX" "SizeY")))
			"Module length and width must be greater than zero."
		)
		((not (vl-every '(lambda ( key / val ) (and (numberp (setq val (json:Get pre key))) (>= val 0))) (list "ModuleGap" "JointGapSize" "MotorGapSize" "PilingOverhangLeftSize" "PilingOverhangRightSize")))
			"Gaps and overhangs must be numbers of zero or greater."
		)
		((not (and (numberp (setq num (json:Get pre "TiltAngle"))) (<= 0 num 90)))
			"Max tilt must be between 0 and 90 degrees."
		)
		((null pck)
			"Module layout is empty or not valid."
		)
		((/= mdl (fix (json:Get pre "Columns")))
			(strcat "Module layout has " (itoa mdl) " modules, but modules per tracker is " (itoa (fix (json:Get pre "Columns"))) ".")
		)
		((< 1 (apply '+ (mapcar '(lambda ( itm ) (if (= "Motor" (car itm)) (cdr itm) 0)) pck)))
			"Only one motor per tracker is supported."
		)
		((not
				(and
					(= 'LIST (type hd))
					(< 2 (length hd))
					(vl-every '(lambda ( dst ) (and (numberp dst) (>= dst 0))) hd)
					(vl-every '(lambda ( dst ) (> dst 0)) (cdr (reverse (cdr hd)))) ;; SPACING BETWEEN PILES MUST BE POSITIVE, END MARGINS MAY BE ZERO
				)
			)
			"Pile spacing needs a north margin, at least one spacing between piles, and a south margin."
		)
		((< (PPP:PresetTolerance)
				(abs
					(-
						(setq tot (apply '+ hd))
						(setq ext (+ (car (setq lay (PPP:TrackerLayout pre))) (json:Get pre "PilingOverhangLeftSize") (json:Get pre "PilingOverhangRightSize")))
					)
				)
			)
			(strcat "Pile spacing totals " (rtos tot 2 3) " ft, but tracker length plus overhangs is " (rtos ext 2 3) " ft.")
		)
		((and (cadr lay) (null (PPP:MotorPileIndex pre)))
			"No pile sits at the center of the motor gap, check the pile spacing around the motor."
		)
	)
)
;; CHECKS A PRESET FOR MISSING OR INCONSISTENT VALUES
;; PILE SPACING MUST TOTAL THE TRACKER LENGTH PLUS BOTH OVERHANGS, AND A PILE MUST SIT AT THE MOTOR GAP CENTER (BOTH WITHIN PPP:PresetTolerance)
;; RETURNS A MESSAGE DESCRIBING THE FIRST PROBLEM FOUND, NIL IF THE PRESET IS VALID

(defun PPP:LoadPresets ( pth / dat arr )
	(if (and
			(findfile pth)
			(setq dat (json:ReadFile pth))
			(listp (setq arr (json:Get dat "Presets")))
			(eq 'ARRAY (car arr))
			(vl-every '(lambda ( pre ) (and (listp pre) (eq 'OBJECT (car pre)))) (json:Items arr))
		)
		arr
	)
)
;; READS TRACKER PRESETS FROM A PPP PRESET FILE
;; RETURNS THE TAGGED "Presets" ARRAY (USE json:Items FOR THE LIST OF PRESETS), (ARRAY) FOR A FILE WITH NO PRESETS
;; RETURNS NIL IF THE FILE IS MISSING OR IS NOT A VALID PRESET FILE, CHECK (findfile pth) TO TELL THE TWO APART

(defun PPP:SavePresets ( pth lst / )
	(json:WriteFile pth
		(json:Object
			(list
				(cons "Version" 1)
				(cons "Presets" (json:Array lst))
			)
		)
		"\t"
	)
)
;; WRITES TRACKER PRESETS TO A PPP PRESET FILE, EXISTING FILES ARE OVERWRITTEN
;; RETURNS T IF SUCCESSFUL, ELSE NIL (SEE json:LastError)



;;; PPP SHARED FUNCTIONS - PROJECT FILES ;;;
;; PROJECT FILES LIVE IN THE PPP-BOM ROOT, THE PPP SETUP DRAWING IS SAVED ONE LEVEL BELOW IT (SEE 'sav' IN PPPSetupMain)

(defun PPP:SettingsFile ( / )
	"PPP-SETTINGS.cfg"
)
;; RETURNS THE PROJECT SETTINGS FILE NAME, SAVED IN THE PPP-BOM ROOT

(defun PPP:PresetsFile ( / )
	"PPP-PRESETS.json"
)
;; RETURNS THE TRACKER PRESETS FILE NAME, SAVED IN THE PPP-BOM ROOT

(defun PPP:LoadSettings ( pth / des lin val lst )
	(if (and (setq pth (findfile pth)) (setq des (open pth "r")))
		(progn
			(vl-catch-all-apply
				'(lambda ( )
					(while (setq lin (read-line des))
						(if (and
								(/= "" (vl-string-trim " \t" lin))
								(/= ";" (substr (vl-string-left-trim " \t" lin) 1 1)) ;; SKIP COMMENT LINES
								(not (vl-catch-all-error-p (setq val (vl-catch-all-apply 'read (list lin)))))
								(= 'LIST (type val))
								(= 'STR (type (car val)))
							)
							(setq lst (cons val lst))
						)
					)
				)
			) ;; A FAILED READ STILL FALLS THROUGH TO CLOSE, SO THE FILE IS NEVER LEFT OPEN
			(close des)
			(reverse lst)
		)
	)
)
;; READS A PROJECT SETTINGS FILE WHERE EACH LINE IS A ( "KEY" . VALUE ) PAIR
;; RETURNS AN ASSOCIATION LIST IF THE FILE EXISTS, ELSE NIL

(defun PPP:SaveSettings ( pth lst / des err )
	(if (setq des (open pth "w"))
		(progn
			(setq err
				(vl-catch-all-apply
					'(lambda ( )
						(write-line ";; PPPSETUP PROJECT SETTINGS - GENERATED FILE, ONE ( \"KEY\" . VALUE ) PAIR PER LINE" des)
						(write-line (strcat ";; LAST UPDATED: " (menucmd "m=$(edtime,$(getvar,date),YYYY-MO-DD HH:MM)")) des)
						(foreach pair lst
							(write-line (vl-prin1-to-string pair) des)
						)
					)
				)
			)
			(close des)
			(not (vl-catch-all-error-p err))
		)
	)
)
;; WRITES PROJECT SETTINGS TO FILE, ONE PAIR PER LINE SO THE FILE STAYS READABLE AND HAND-EDITABLE
;; RETURNS T IF SUCCESSFUL, ELSE NIL



;;; PPP SHARED FUNCTIONS - ARRAY BLOCKS ;;;

(defun PPP:IsArrayBlock ( obj cfg / nam )
	(and
		(= (vla-get-objectname obj) "AcDbBlockReference")
		(wcmatch (strcase (vla-get-layer obj)) (strcase (cdr (assoc "array_layers" cfg))))
		(not (vl-catch-all-error-p (setq nam (vl-catch-all-apply 'vla-get-effectivename (list obj))))) ;; EFFECTIVE NAME HANDLES DYNAMIC BLOCKS
		(wcmatch (strcase nam) (strcase (cdr (assoc "array_blocks" cfg))))
	)
)
;; CHECKS IF AN OBJECT IS A PV ARRAY BLOCK USING THE ARRAY LAYER AND BLOCK NAME PATTERNS FROM THE PROJECT SETTINGS [ cfg ]
;; FILTERS OUT DUPLICATE LAYOUTS (EX. E-LBD TABLES) AND NON-ARRAY BLOCKS (EX. LBDGP-15A)
;; RETURNS T IF THE OBJECT IS AN ARRAY BLOCK, ELSE NIL



;;; PPP SETUP ;;;

(defun PPPSetupMain
	(
		/
		;Functions
		*error*
		FindSubDirectory FindNewestFile CopyDrawingContents
		CreateDirectoryPath CreateProjectFolders ParseProjectFolder DefaultSettings
		MergeSettings
		ParseNumberList CottRange SlopeToDegrees
		NumberString PresetText ReplaceNth RemoveNth PacksToLayout LayoutToPacks PacksString
		SpacingToString StringToSpacing PresetGeometryEqual PairMismatch ImportPresetFile
		SettingsDialog
		CollectNewObjects FlattenArrayObjects ContourElevation ContourExtents FlagIgnoredObjects ValidateDrawingData
		IsFileLocked SaveProjectDrawing
		;Variables
		doc rt elc cvl str sav subs cfn pfn xfn dir fn ppp cfg prs ius svd ent arr cnt flt ign asn
		funcs
		;DCL Functions
		DCL:RunDialogChecks DCL:Cancel DCL:TileValue DCL:TileNumber
		DCL:UpdateCottRange DCL:UpdateSlope DCL:ValidFileName DCL:UpdatePresetInfo
		DCL:PresetsDialog DCL:PresetRefresh DCL:PresetFill DCL:PresetModes DCL:PresetRead DCL:PresetCommit
		DCL:PresetSelect DCL:PresetNew DCL:PresetDuplicate DCL:PresetPair DCL:PresetDelete DCL:PresetImport
		DCL:PresetSummary DCL:PresetAccept
		;DCL Variables
		ttl dcl tmp des dch res keys pops wrk idx mfr
	)
	
	(defun *error* ( msg / cxl )
		(setq cxl (wcmatch (strcase msg) "*CANCEL*,QUIT / EXIT ABORT")) ;; ESC OR (EXIT), NOT A PROGRAM ERROR
		(if (and dch (< 0 dch))
			(unload_dialog dch)
		)
		(if (eq (type des) 'FILE)
			(close des)
		)
		(if (and (eq (type tmp) 'STR) (findfile tmp))
			(vl-file-delete tmp)
		)
		(if (and dbx (eq (type dbx) 'VLA-OBJECT) (not (vlax-object-released-p dbx)))
			(std:ObjectRelease (list dbx)) ;; IF DBX WAS SET THIS SHOULD BE LOADED
		)
		(if (and ius (not svd))
			(setvar "INSUNITS" ius)
		) ;; RESTORE ORIGINAL UNITS IF THE DRAWING WAS NOT SAVED
		(if (not cxl)
			(vl-bt)
		) ;; FOR DEBUGGING
		(ECHO:Log-Verbose (if cxl "info" "ERROR") (if cxl "cancelled by user" msg) nil (list (cons "CMDNAME" *ECHO:Log-Command*)))
		(ECHO:Log-Flush)
		(princ (if cxl (strcat "\nPPP setup cancelled, the drawing was not saved." (if cfg "\n  Tracker presets were already saved when the settings dialog was accepted." "")) (strcat "\nError: " msg)))
		(princ)
	)
	;; CLEANUP RUNS FOR BOTH ERRORS AND USER CANCELS, A CANCEL IS LOGGED AS INFO AND SHOWN WITHOUT THE 'Error:' PREFIX
	;; THE DRAWING AND SETTINGS SAVES ARE THE LAST STEPS, SO A CANCEL LEAVES THEM UNSAVED (UNITS ARE RESTORED ABOVE)
	;; TRACKER PRESETS AND THE PPP-BOM FOLDERS ARE KEPT ON PURPOSE, PRESETS ARE SAVED AS SOON AS THE SETTINGS DIALOG IS ACCEPTED
	
	;; THE DIRECTORIES WILL NEED TO BE DYNAMICALLY CONFIGURED BASED ON PROJECT DIRECTORY LOCATION
	;; C:\_ACC\ACCDocs\ANS Team\100203 - Mill Creek Renewables - Orca Falls\Project Files\02 ENGINEERING\05 STRUCTURAL\ENGINEERING\PPP-BOM\1. PVCase
	
	(defun FindSubDirectory ( dir sub lvl / SubDirectoryNames que pth res tmp )
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
	;; [ lvl ] LIMITS HOW MANY LEVELS BELOW [ dir ] THE BRIDGE MAY START, KEEPING THE SCAN BOUNDED
	;; RETURNS A CLEANED (NO TRAILING "\\") FULL PATH IF FOUND, ELSE NIL

	(defun FindNewestFile ( dir ptn / FileTimeStamp nam res tim tmp )
		(defun FileTimeStamp ( pth / tm )
			(if (setq tm (vl-file-systime pth))
				(apply '+
					(mapcar '*
						(list (nth 0 tm) (nth 1 tm) (nth 3 tm) (nth 4 tm) (nth 5 tm) (nth 6 tm)) ;; INDEX 2 IS DAY OF WEEK, NOT USED FOR SORTING
						(list 1e10 1e8 1e6 1e4 1e2 1e0)
					)
				)
			)
		)
		;; COLLAPSES THE MODIFICATION TIME LIST INTO A SINGLE COMPARABLE VALUE (YYYYMMDDHHMMSS)

		(if (vl-file-directory-p (setq dir (vl-string-right-trim "\\" dir)))
			(foreach nam (vl-directory-files dir ptn 1) ;; 1 = FILES ONLY, NO SUB-DIRECTORIES
				(if (and (setq tmp (FileTimeStamp (strcat dir "\\" nam))) (or (null tim) (> tmp tim)))
					(setq tim tmp res nam)
				)
			)
		)
		res
	)
	;; SEARCHES TARGET DIRECTORY FOR THE MOST RECENTLY MODIFIED FILE MATCHING THE WILDCARD PATTERN (EX. "*.dwg")
	;; RETURNS THE FILE NAME ONLY (NO PATH) IF FOUND, ELSE NIL

	(defun CopyDrawingContents ( dir fn / dbx )
		(if (and (= (strcase (vl-filename-extension fn)) ".DWG") (setq dbx (std:CreateObjectDBX)))
			(progn
				(vla-open dbx (strcat dir "\\" fn) :vlax-true)
				(std:CopyDBXObjects dbx 7 nil nil '(0 0 0)) ;; 7 = IGNORE ANY CONSTRUCTION LINES OR NODES WHEN COPYING DETAILS OVER
				(std:ObjectRelease (list dbx))
				t
			)
		)
	)
	;; COPIES DRAWING CONTENTS TO CURRENT DRAWING FROM TARGET DIRECTORY AND FILE NAME
	;; RETURNS T IF SUCCESSFUL, ELSE NIL
	
	(defun CreateDirectoryPath ( pth / par )
		(setq pth (vl-string-right-trim "\\" pth))
		(cond
			((vl-file-directory-p pth)
				pth
			)
			((and
					(setq par (vl-filename-directory pth))
					(/= par "")
					(/= par pth)
					(CreateDirectoryPath par) ;; BUILD PARENT FOLDERS FIRST, VL-MKDIR ONLY CREATES ONE LEVEL AT A TIME
					(vl-mkdir pth)
					(vl-file-directory-p pth)
				)
				pth
			)
		)
	)
	;; CREATES THE PASSED DIRECTORY PATH INCLUDING ANY MISSING PARENT FOLDERS
	;; RETURNS THE CLEANED (NO TRAILING "\\") PATH IF IT EXISTS OR WAS CREATED, ELSE NIL
	
	(defun CreateProjectFolders ( pth lst / )
		(vl-every '(lambda ( sub ) (CreateDirectoryPath (strcat pth "\\" sub))) lst)
	)
	;; CREATES THE STANDARD PPP/BOM SUB-DIRECTORIES WITHIN THE PASSED PPP-BOM LOCATION
	;; RETURNS T IF ALL FOLDERS EXIST OR WERE CREATED, ELSE NIL
	
	(defun ParseProjectFolder ( pth / pos nam )
		(setq pth (vl-string-right-trim "\\" pth))
		(setq nam (if (setq pos (vl-string-position 92 pth nil t)) (substr pth (+ pos 2)) pth)) ;; 92 = "\\", SEARCHED FROM THE END
		(if (setq pos (vl-string-search " - " nam))
			(list (substr nam 1 pos) (substr nam (+ pos 4)))
			(list nam "")
		)
	)
	;; SPLITS THE PROJECT ROOT FOLDER NAME INTO PROJECT NUMBER AND PROJECT NAME
	;; EX. "100203 - Mill Creek Renewables - Orca Falls" -> ("100203" "Mill Creek Renewables - Orca Falls")
	
	(defun DefaultSettings ( dir / prj )
		(setq prj (ParseProjectFolder dir))
		(list
			(cons "project_number" (car prj))
			(cons "project_name" (cadr prj))
			(cons "milestone" "30%")
			(cons "file_name" (strcat (car prj) " - PPP SETUP"))
			(cons "units" "Feet")
			(cons "contour_layers" "V-TOPO-MAJR,V-TOPO-MINR")
			(cons "array_layers" "E-INV*")
			(cons "array_blocks" "*PVBlock")
			(cons "manufacturer" "Nextracker")
			(cons "exterior_rows" "2")
			(cons "stagger_allowance" "0.0") ;; END OFFSET (FT) ALLOWED BETWEEN NEIGHBORING ARRAYS BEFORE THEY ARE AN EDGE, RAISE ONLY FOR STAGGERED (DIAGONAL) LAYOUTS
			(cons "table_slope" "8.0")
			(cons "cott_buckets" "6.0,6.5,7.0")
			(cons "tolerance" "0.04")
			(cons "allowed_flood" "0.5")
			(cons "decimals" "3")
			(cons "eor_confirmed" "0")
		)
	)
	;; RETURNS DEFAULT PROJECT SETTINGS AS AN ASSOCIATION LIST OF ( "KEY" . "VALUE" ) STRING PAIRS
	;; KEYS MATCH THE DCL TILE KEYS, SO THE ORDER OF THIS LIST DEFINES WHICH TILES ARE LOADED AND SAVED
	
	(defun MergeSettings ( lod def / )
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
	;; OVERLAYS LOADED SETTINGS ONTO THE DEFAULT LIST, KEYS NOT FOUND IN THE LOADED FILE KEEP THEIR DEFAULT VALUE
	;; RETURNS AN ASSOCIATION LIST WITH THE SAME KEYS AND ORDER AS THE DEFAULTS
	
	(defun ParseNumberList ( str / pos lst )
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
	
	(defun CottRange ( lst / srt stp )
		(if (> (length (setq srt (vl-sort lst '<))) 1)
			(progn
				(setq stp (apply 'min (mapcar '- (cdr srt) srt))) ;; SMALLEST GAP BETWEEN CONSECUTIVE BUCKETS
				(if (> stp 1e-8)
					(list (- (car srt) stp) (last srt) stp)
				)
			)
		)
	)
	;; CALCULATES COTT RANGE FROM FOUNDATION SCHEDULE BUCKET HEIGHTS
	;; MINIMUM = LOWEST BUCKET MINUS BUCKET STEP, MAXIMUM = HIGHEST BUCKET (EX. 6.0, 6.5, 7.0 -> 5.5 TO 7.0)
	;; RETURNS ( MIN MAX STEP ) IF TWO OR MORE UNIQUE BUCKETS ARE PASSED, ELSE NIL
	
	(defun SlopeToDegrees ( pct / )
		(* 180.0 (/ (atan (/ pct 100.0)) pi))
	)
	;; CONVERTS A PERCENT SLOPE TO DEGREES (EX. 8.0 -> 4.574)
	
	(defun NumberString ( num / str )
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
	;; FORMATS A NUMBER FOR AN EDIT BOX WITHOUT TRAILING ZEROS (EX. 6.804 -> "6.804"), "" IF NOT A NUMBER
	
	(defun PresetText ( val / )
		(if (= 'STR (type val))
			val
			""
		)
	)
	;; RETURNS THE VALUE IF IT IS A STRING, ELSE ""
	
	(defun ReplaceNth ( lst idx itm / pos )
		(setq pos -1)
		(mapcar '(lambda ( x ) (if (= idx (setq pos (1+ pos))) itm x)) lst)
	)
	;; RETURNS A COPY OF THE LIST WITH THE ITEM AT ZERO-BASED [ idx ] REPLACED BY [ itm ]
	
	(defun RemoveNth ( lst idx / pos )
		(setq pos -1)
		(vl-remove-if '(lambda ( x ) (= idx (setq pos (1+ pos)))) lst)
	)
	;; RETURNS A COPY OF THE LIST WITHOUT THE ITEM AT ZERO-BASED [ idx ]
	
	(defun PacksToLayout ( pck / tok prv pos out )
		(foreach itm pck
			(cond
				((/= "Modules" (car itm))
					(repeat (cdr itm)
						(setq tok (cons (if (= "Motor" (car itm)) "M" "J") tok))
					)
				)
				((= "Modules" prv)
					(setq tok (cons (strcat (car tok) "+" (itoa (cdr itm))) (cdr tok))) ;; BACK TO BACK MODULE PACKS WITH NO JOINT GAP
				)
				( t
					(setq tok (cons (itoa (cdr itm)) tok))
				)
			)
			(setq prv (car itm))
		)
		(setq
			tok (reverse tok)
			pos -1
		)
		(foreach itm tok
			(setq pos (1+ pos))
			(if (not
					(and
						(= "J" itm)
						(< 0 pos (1- (length tok)))
						(not (member (nth (1- pos) tok) (list "J" "M")))
						(not (member (nth (1+ pos) tok) (list "J" "M")))
					)
				)
				(setq out (cons itm out))
			) ;; A SINGLE JOINT GAP BETWEEN TWO MODULE GROUPS IS IMPLIED BY THE COMMA
		)
		(json:StringJoin (reverse out) ",")
	)
	;; CONVERTS PACKS INTO THE SHORT MODULE LAYOUT SHOWN IN THE PRESETS DIALOG (DCL EDIT BOXES ARE LIMITED TO 256 CHARACTERS)
	;; NUMBERS == MODULES, COMMA == JOINT GAP, M == MOTOR, + == NO JOINT GAP, J == EXTRA JOINT GAP
	;; EX. "Modules,6; JointGap,1; Modules,7; Motor,1; Modules,6" -> "6,7,M,6"
	
	(defun LayoutToPacks ( str / AddPack lst prv bad num )
		(defun AddPack ( typ cnt / )
			(if (and (/= "Modules" typ) (= typ (car (car lst))))
				(setq lst (cons (cons typ (+ cnt (cdr (car lst)))) (cdr lst)))
				(setq lst (cons (cons typ cnt) lst))
			)
		)
		;; ADDS A PACK TO THE FRONT OF 'lst', BACK TO BACK JOINT GAPS OR MOTORS ARE COMBINED INTO ONE COUNT
		
		(foreach tok (json:StringSplit (vl-string-trim " \t" str) ",")
			(setq tok (strcase (vl-string-trim " \t" tok)))
			(cond
				((= "M" tok)
					(AddPack "Motor" 1)
					(setq prv "M")
				)
				((= "J" tok)
					(AddPack "JointGap" 1)
					(setq prv "J")
				)
				((vl-every
						'(lambda ( grp ) (and (setq num (distof (vl-string-trim " \t" grp) 2)) (>= num 1) (equal num (fix num) 1e-8)))
						(json:StringSplit tok "+")
					)
					(if (= "N" prv)
						(AddPack "JointGap" 1)
					) ;; A COMMA BETWEEN TWO MODULE GROUPS == ONE JOINT GAP
					(foreach grp (json:StringSplit tok "+")
						(AddPack "Modules" (fix (distof (vl-string-trim " \t" grp) 2)))
					)
					(setq prv "N")
				)
				( t
					(setq bad t)
				)
			)
		)
		(if (and lst (not bad))
			(reverse lst)
		)
	)
	;; CONVERTS THE SHORT MODULE LAYOUT BACK INTO PACKS (SEE PacksToLayout)
	;; RETURNS NIL IF THE LAYOUT IS EMPTY OR CONTAINS AN INVALID ENTRY
	
	(defun PacksString ( pck / )
		(json:StringJoin (mapcar '(lambda ( itm ) (strcat (car itm) "," (itoa (cdr itm)))) pck) "; ")
	)
	;; CONVERTS PACKS INTO A PVCASE PACKS STRING (EX. "Modules,6; Motor,1; Modules,6")
	
	(defun SpacingToString ( lst / out val cnt )
		(while lst
			(setq
				val (car lst)
				cnt 0
			)
			(while (and lst (equal val (car lst) 1e-6))
				(setq
					cnt (1+ cnt)
					lst (cdr lst)
				)
			)
			(setq out (cons (if (> cnt 1) (strcat (itoa cnt) "@" (NumberString val)) (NumberString val)) out))
		)
		(json:StringJoin (reverse out) ",")
	)
	;; CONVERTS PILE DISTANCES INTO THE SHORT FORM SHOWN IN THE PRESETS DIALOG, REPEATED VALUES USE n@s
	;; EX. (6.804 26.644 30.997 30.997 30.997) -> "6.804,26.644,3@30.997"
	
	(defun StringToSpacing ( str / pos cnt num lst bad )
		(foreach tok (json:StringSplit (vl-string-translate ";" "," str) ",")
			(setq tok (vl-string-trim " \t" tok))
			(cond
				((setq pos (vl-string-search "@" tok))
					(if (and
							(setq cnt (distof (vl-string-trim " \t" (substr tok 1 pos)) 2))
							(>= cnt 1)
							(equal cnt (fix cnt) 1e-8)
							(setq num (distof (vl-string-trim " \t" (substr tok (+ pos 2))) 2))
						)
						(repeat (fix cnt)
							(setq lst (cons num lst))
						)
						(setq bad t)
					)
				)
				((setq num (distof tok 2))
					(setq lst (cons num lst))
				)
				( t
					(setq bad t)
				)
			)
		)
		(if (and lst (not bad))
			(reverse lst)
		)
	)
	;; CONVERTS THE SHORT PILE SPACING FORM BACK INTO A LIST OF DISTANCES (SEE SpacingToString)
	;; RETURNS NIL IF THE STRING IS EMPTY OR CONTAINS AN INVALID ENTRY
	
	(defun PresetGeometryEqual ( pre par / )
		(vl-every
			'(lambda ( key )
				(if (= "Packs" key)
					(equal (PPP:ParsePacks (json:Get pre key)) (PPP:ParsePacks (json:Get par key))) ;; COMPARE PARSED PACKS SO SPACING IN THE STRING DOES NOT MATTER
					(equal (json:Get pre key) (json:Get par key) 1e-6)
				)
			)
			(vl-remove-if '(lambda ( key ) (member key (list "Name" "Manufacturer" "Description"))) (PPP:PresetKeys))
		)
	)
	;; RETURNS T IF TWO PRESETS HAVE THE SAME TRACKER GEOMETRY, ELSE NIL
	
	(defun PairMismatch ( lst / par )
		(vl-some
			'(lambda ( pre )
				(if (and
						(setq par (PPP:FindPreset lst (PPP:PairName (json:Get pre "Name"))))
						(not (PresetGeometryEqual pre par))
					)
					(strcat (json:Get pre "Name") " and " (json:Get par "Name") " have different geometry. Select the correct one and use Copy to Pair.")
				)
			)
			lst
		)
	)
	;; RETURNS A MESSAGE FOR THE FIRST INT / EXT PAIR WITH DIFFERENT GEOMETRY, NIL IF ALL PAIRS MATCH
	;; BOTH PRESETS OF A PAIR DESCRIBE THE SAME TRACKER, ONLY THE NAME (BOM ROW TYPE) AND DESCRIPTION MAY DIFFER
	
	(defun ImportPresetFile ( pth mfr / src dat lst uns )
		(setq src (strcat (vl-filename-base pth) (vl-filename-extension pth)))
		(cond
			((null (setq dat (json:ReadFile pth)))
				(strcat "Could not read " src (if (json:LastError) (strcat ": " (json:LastError)) "."))
			)
			((json:Has dat "BlockTemplatesContainer")
				(setq uns 0)
				(foreach tpl (json:Items (json:GetPath dat (list "BlockTemplatesContainer" "Templates")))
					(if (or
							(not (equal 1 (json:Get tpl "Rows") 1e-8))
							(json:True-p (json:Get tpl "Landscape"))
							(json:True-p (json:Get tpl "IsMultipleRowsTracker"))
						)
						(setq uns (1+ uns)) ;; ONLY SINGLE ROW PORTRAIT (1P) TRACKERS ARE SUPPORTED FOR NOW
						(setq lst
							(cons
								(json:Object
									(mapcar
										'(lambda ( key )
											(cons key
												(cond
													((= "Manufacturer" key) mfr)
													((= "Description" key) (strcat "Imported from " src))
													((= "HorizontalDistances" key) (json:GetPath tpl (list "PilingData" "FullTemplate" "HorizontalDistances")))
													( t (json:Get tpl key))
												)
											)
										)
										(PPP:PresetKeys)
									)
								)
								lst
							)
						)
					)
				)
				(list (reverse lst) uns)
			)
			((json:Has dat "Presets")
				(list (json:Items (json:Get dat "Presets")) 0)
			)
			( t
				(strcat src " does not contain PVcase templates or PPP presets.")
			)
		)
	)
	;; READS PRESETS FROM A PVCASE .PVsettings FILE OR ANOTHER PPP PRESET FILE (EX. A SHARED REPOSITORY FILE)
	;; PVCASE HAS NO TRACKER MANUFACTURER FIELD, [ mfr ] IS USED FOR IMPORTED TEMPLATES
	;; RETURNS ( PRESETS UNSUPPORTED-COUNT ), OR A MESSAGE STRING IF THE FILE COULD NOT BE READ
	
	(defun SettingsDialog ( cfg / sts )
		(setq ttl "PPP_Setup")
		(setq keys (mapcar 'car cfg))
		(setq pops
			(list
				(list "milestone" "30%" "60%" "90%" "IFC")
				(list "units" "Feet" "US Survey Feet")
				(list "manufacturer" "Nextracker" "Array Technologies (ATI)" "Other")
			)
		) ;; POPUP LIST TILE KEYS AND THEIR DISPLAYED VALUES
		(setq dcl
			(list
				"// Temporary DCL file;"
				(strcat ttl " : dialog {")
				"	label = \"Pile Plot Plan Setup Dialog Control\";"
				"	: row {"
				"		: boxed_column {"
				"			label = \"Project\";"
				"			: edit_box {"
				"				label = \"Project number:\";"
				"				key = \"project_number\";"
				"				edit_width = 28;"
				"			}"
				"			: edit_box {"
				"				label = \"Project name:\";"
				"				key = \"project_name\";"
				"				edit_width = 28;"
				"			}"
				"			: popup_list {"
				"				label = \"Milestone:\";"
				"				key = \"milestone\";"
				"				edit_width = 26;"
				"			}"
				"			: edit_box {"
				"				label = \"Drawing name:\";"
				"				key = \"file_name\";"
				"				edit_width = 28;"
				"			}"
				"			: popup_list {"
				"				label = \"Drawing units:\";"
				"				key = \"units\";"
				"				edit_width = 26;"
				"			}"
				"			: edit_box {"
				"				label = \"Contour layers:\";"
				"				key = \"contour_layers\";"
				"				edit_width = 28;"
				"			}"
				"			: edit_box {"
				"				label = \"Array layers:\";"
				"				key = \"array_layers\";"
				"				edit_width = 28;"
				"			}"
				"			: edit_box {"
				"				label = \"Array block names:\";"
				"				key = \"array_blocks\";"
				"				edit_width = 28;"
				"			}"
				"		}"
				"		: boxed_column {"
				"			label = \"Tracker\";"
				"			: popup_list {"
				"				label = \"Manufacturer:\";"
				"				key = \"manufacturer\";"
				"				edit_width = 26;"
				"			}"
				"			: edit_box {"
				"				label = \"Exterior rows per side:\";"
				"				key = \"exterior_rows\";"
				"				edit_width = 10;"
				"			}"
				"			: edit_box {"
				"				label = \"Stagger allowance (ft):\";"
				"				key = \"stagger_allowance\";"
				"				edit_width = 10;"
				"			}"
				"			: text {"
				"				label = \"End offset allowed between neighboring arrays.\";"
				"			}"
				"			: text {"
				"				label = \"0 = any offset is an edge (staggered layouts only).\";"
				"			}"
				"			: edit_box {"
				"				label = \"Max table slope (%):\";"
				"				key = \"table_slope\";"
				"				edit_width = 10;"
				"			}"
				"			: text {"
				"				key = \"slope_deg\";"
				"				width = 36;"
				"			}"
				"			: row {"
				"				: text {"
				"					key = \"presets_info\";"
				"					width = 30;"
				"				}"
				"				: button {"
				"					label = \"Presets...\";"
				"					key = \"presets\";"
				"					fixed_width = true;"
				"				}"
				"			}"
				"		}"
				"	}"
				"	: boxed_column {"
				"		label = \"COTT Range\";"
				"		: edit_box {"
				"			label = \"Foundation schedule bucket heights (ft):\";"
				"			key = \"cott_buckets\";"
				"			edit_width = 30;"
				"		}"
				"		: text {"
				"			key = \"cott_range\";"
				"			width = 60;"
				"		}"
				"		: row {"
				"			: edit_box {"
				"				label = \"Tolerance (ft):\";"
				"				key = \"tolerance\";"
				"				edit_width = 8;"
				"			}"
				"			: edit_box {"
				"				label = \"Allowed flood (ft):\";"
				"				key = \"allowed_flood\";"
				"				edit_width = 8;"
				"			}"
				"			: edit_box {"
				"				label = \"Decimals:\";"
				"				key = \"decimals\";"
				"				edit_width = 4;"
				"			}"
				"		}"
				"		: toggle {"
				"			label = \"COTT range confirmed with engineer of record\";"
				"			key = \"eor_confirmed\";"
				"		}"
				"	}"
				"	spacer_1;"
				"	errtile;"
				"	ok_cancel;"
				"}"
				"PPP_Presets : dialog {"
				"	label = \"Tracker Presets\";"
				"	: row {"
				"		: boxed_column {"
				"			label = \"Presets\";"
				"			: list_box {"
				"				key = \"preset_list\";"
				"				width = 26;"
				"				height = 24;"
				"			}"
				"			: row {"
				"				: button {"
				"					label = \"New\";"
				"					key = \"preset_new\";"
				"				}"
				"				: button {"
				"					label = \"Duplicate\";"
				"					key = \"preset_copy\";"
				"				}"
				"			}"
				"			: row {"
				"				: button {"
				"					label = \"Copy to Pair\";"
				"					key = \"preset_pair\";"
				"				}"
				"				: button {"
				"					label = \"Delete\";"
				"					key = \"preset_delete\";"
				"				}"
				"			}"
				"			: button {"
				"				label = \"Import...\";"
				"				key = \"preset_import\";"
				"			}"
				"		}"
				"		: column {"
				"			: boxed_column {"
				"				label = \"Preset\";"
				"				: edit_box {"
				"					label = \"Name (INT- / EXT-):\";"
				"					key = \"Name\";"
				"					edit_width = 30;"
				"				}"
				"				: edit_box {"
				"					label = \"Manufacturer:\";"
				"					key = \"Manufacturer\";"
				"					edit_width = 30;"
				"				}"
				"				: edit_box {"
				"					label = \"Description:\";"
				"					key = \"Description\";"
				"					edit_width = 30;"
				"					edit_limit = 256;"
				"				}"
				"			}"
				"			: boxed_column {"
				"				label = \"Modules\";"
				"				: row {"
				"					: column {"
				"						: edit_box {"
				"							label = \"Modules per tracker:\";"
				"							key = \"Columns\";"
				"							edit_width = 8;"
				"						}"
				"						: edit_box {"
				"							label = \"Module length, E-W (ft):\";"
				"							key = \"SizeX\";"
				"							edit_width = 8;"
				"						}"
				"						: edit_box {"
				"							label = \"Module width, N-S (ft):\";"
				"							key = \"SizeY\";"
				"							edit_width = 8;"
				"						}"
				"						: edit_box {"
				"							label = \"Max tilt (deg):\";"
				"							key = \"TiltAngle\";"
				"							edit_width = 8;"
				"						}"
				"					}"
				"					: column {"
				"						: edit_box {"
				"							label = \"Module gap (ft):\";"
				"							key = \"ModuleGap\";"
				"							edit_width = 8;"
				"						}"
				"						: edit_box {"
				"							label = \"Joint gap (ft):\";"
				"							key = \"JointGapSize\";"
				"							edit_width = 8;"
				"						}"
				"						: edit_box {"
				"							label = \"Motor gap (ft):\";"
				"							key = \"MotorGapSize\";"
				"							edit_width = 8;"
				"						}"
				"					}"
				"				}"
				"			}"
				"			: boxed_column {"
				"				label = \"Layout (north to south)\";"
				"				: edit_box {"
				"					label = \"Module layout:\";"
				"					key = \"layout\";"
				"					edit_width = 48;"
				"					edit_limit = 256;"
				"				}"
				"				: text {"
				"					label = \"Numbers = modules, comma = joint gap, M = motor, + = no joint gap, J = extra joint gap\";"
				"				}"
				"				: edit_box {"
				"					label = \"Pile spacing (ft):\";"
				"					key = \"spacing\";"
				"					edit_width = 48;"
				"					edit_limit = 256;"
				"				}"
				"				: text {"
				"					label = \"First and last values are measured to the overhang lines, n@s repeats a spacing (ex. 5@30.997)\";"
				"				}"
				"				: row {"
				"					: edit_box {"
				"						label = \"Overhang north (ft):\";"
				"						key = \"PilingOverhangLeftSize\";"
				"						edit_width = 8;"
				"					}"
				"					: edit_box {"
				"						label = \"Overhang south (ft):\";"
				"						key = \"PilingOverhangRightSize\";"
				"						edit_width = 8;"
				"					}"
				"				}"
				"			}"
				"			: text {"
				"				key = \"preset_summary\";"
				"				width = 70;"
				"			}"
				"		}"
				"	}"
				"	spacer_1;"
				"	errtile;"
				"	ok_cancel;"
				"}"
			)
		)
		
		(defun DCL:TileValue ( key / val )
			(setq val (vl-string-trim " " (get_tile key)))
			(cond
				((assoc key pops)
					(nth (atoi val) (cdr (assoc key pops))) ;; POPUP TILES RETURN AN INDEX, STORE THE DISPLAYED TEXT INSTEAD
				)
				((and (= key "file_name") (wcmatch (strcase val) "*.DWG"))
					(substr val 1 (- (strlen val) 4)) ;; STRIP EXTENSION IF THE USER TYPED IT
				)
				( t
					val
				)
			)
		)
		;; RETURNS THE STORED STRING VALUE FOR A TILE KEY
		
		(defun DCL:TileNumber ( key mn mx int / num )
			(if (and
					(setq num (distof (vl-string-trim " " (get_tile key)) 2))
					(>= num mn)
					(or (null mx) (<= num mx))
					(or (not int) (equal num (fix num) 1e-8))
				)
				num
			)
		)
		;; RETURNS THE NUMERIC VALUE OF AN EDIT BOX IF IT FALLS WITHIN [ mn ] TO [ mx ] (NIL MX = NO LIMIT), ELSE NIL
		;; [ int ] == T TO REQUIRE A WHOLE NUMBER
		
		(defun DCL:ValidFileName ( str / )
			(and
				(/= "" (vl-string-trim " " str))
				(not (vl-some '(lambda ( chr ) (vl-string-search chr str)) (list "\\" "/" ":" "*" "?" "\"" "<" ">" "|")))
			)
		)
		;; RETURNS T IF THE STRING IS A VALID WINDOWS FILE NAME, ELSE NIL
		
		(defun DCL:UpdateCottRange ( str / rng )
			(set_tile "cott_range"
				(if (setq rng (CottRange (ParseNumberList str)))
					(strcat
						"COTT range: " (rtos (car rng) 2 3) " to " (rtos (cadr rng) 2 3)
						" ft (bucket step " (rtos (caddr rng) 2 3) " ft)"
					)
					"Enter two or more unique bucket heights, separated by commas."
				)
			)
		)
		;; DISPLAYS THE CALCULATED COTT RANGE BELOW THE BUCKET HEIGHTS EDIT BOX
		
		(defun DCL:UpdateSlope ( str / num )
			(set_tile "slope_deg"
				(if (and (setq num (distof (vl-string-trim " " str) 2)) (> num 0))
					(strcat "Max table slope = " (rtos (SlopeToDegrees num) 2 3) " degrees")
					"Enter table slope as a percent."
				)
			)
		)
		;; DISPLAYS THE MAX TABLE SLOPE CONVERTED TO DEGREES
		
		(defun DCL:RunDialogChecks ( / lst rng )
			(cond
				((= "" (vl-string-trim " " (get_tile "project_number")))
					(set_tile "error" "Error: Project number cannot be blank.")
				)
				((not (DCL:ValidFileName (get_tile "file_name")))
					(set_tile "error" "Error: Drawing name is blank or contains invalid characters.")
				)
				((= "" (vl-string-trim " " (get_tile "contour_layers")))
					(set_tile "error" "Error: Contour layers cannot be blank.")
				)
				((= "" (vl-string-trim " " (get_tile "array_layers")))
					(set_tile "error" "Error: Array layers cannot be blank.")
				)
				((= "" (vl-string-trim " " (get_tile "array_blocks")))
					(set_tile "error" "Error: Array block names cannot be blank.")
				)
				((null (setq rng (CottRange (setq lst (ParseNumberList (get_tile "cott_buckets"))))))
					(set_tile "error" "Error: COTT buckets must be two or more unique numbers.")
				)
				((not (vl-every '(lambda ( x ) (> x 0)) lst))
					(set_tile "error" "Error: COTT bucket heights must be greater than zero.")
				)
				((not (vl-every '(lambda ( key ) (DCL:TileNumber key 0.0 nil nil)) (list "allowed_flood" "tolerance")))
					(set_tile "error" "Error: Flood and tolerance values must be numbers of zero or greater.")
				)
				((not (DCL:TileNumber "table_slope" 1e-6 nil nil))
					(set_tile "error" "Error: Max table slope must be a number greater than zero.")
				)
				((not (DCL:TileNumber "decimals" 0 8 t))
					(set_tile "error" "Error: Decimals must be a whole number from 0 to 8.")
				)
				((not (DCL:TileNumber "exterior_rows" 0 99 t))
					(set_tile "error" "Error: Exterior rows must be a whole number.")
				)
				((not (DCL:TileNumber "stagger_allowance" 0.0 nil nil))
					(set_tile "error" "Error: Stagger allowance must be a number of zero or greater.")
				)
				( t
					(setq res
						(append
							(mapcar '(lambda ( key ) (cons key (DCL:TileValue key))) keys)
							(list
								(cons "cott_min" (car rng))
								(cons "cott_max" (cadr rng))
								(cons "cott_step" (caddr rng))
								(cons "table_slope_deg" (SlopeToDegrees (DCL:TileNumber "table_slope" 1e-6 nil nil)))
							)
						)
					) ;; TILE VALUES MUST BE READ BEFORE DONE_DIALOG, CALCULATED VALUES ARE STORED FOR LATER STAGES
					(done_dialog 1)
				)
			)
		)
		;; RUNS INNER FUNCTION DIALOG BOX CHECKS FOR ERRORS, SETS 'res' TO THE SETTINGS LIST IF ALL CHECKS PASS
		
		(defun DCL:Cancel ( / )
			(done_dialog 0)
			(if (findfile tmp)
				(vl-file-delete tmp)
			)
		)
		;; CLEANS UP BACKEND IF PROGRAM IS CANCELED
		
		(defun DCL:UpdatePresetInfo ( / cnt )
			(setq cnt (length (vl-remove-if '(lambda ( pre ) (PPP:FindPreset prs (PPP:PairName (json:Get pre "Name")))) prs)))
			(set_tile "presets_info"
				(cond
					((null prs)
						"No tracker presets defined."
					)
					((= 0 cnt)
						(strcat (itoa (length prs)) " tracker preset" (if (= 1 (length prs)) "" "s"))
					)
					( t
						(strcat (itoa (length prs)) " presets, " (itoa cnt) " without an INT/EXT pair")
					)
				)
			)
		)
		;; DISPLAYS THE PRESET COUNT IN THE SETTINGS DIALOG, PRESETS WITHOUT A MATCHING INT / EXT PRESET ARE COUNTED SEPARATELY
		
		(defun DCL:PresetsDialog ( / sts )
			(setq
				mfr (DCL:TileValue "manufacturer") ;; READ FROM THE SETTINGS DIALOG BEFORE THE PRESETS DIALOG BECOMES CURRENT
				wrk prs
				idx nil
			)
			(if (new_dialog "PPP_Presets" dch)
				(progn
					(DCL:PresetRefresh nil) ;; SELECTS THE FIRST PRESET IN SORTED ORDER
					
					(action_tile "preset_list" "(DCL:PresetSelect $value)")
					(action_tile "preset_new" "(DCL:PresetNew)")
					(action_tile "preset_copy" "(DCL:PresetDuplicate)")
					(action_tile "preset_pair" "(DCL:PresetPair)")
					(action_tile "preset_delete" "(DCL:PresetDelete)")
					(action_tile "preset_import" "(DCL:PresetImport)")
					(foreach key (list "Name" "Columns" "SizeX" "SizeY" "ModuleGap" "JointGapSize" "MotorGapSize" "TiltAngle" "layout" "spacing" "PilingOverhangLeftSize" "PilingOverhangRightSize")
						(action_tile key "(DCL:PresetSummary)")
					) ;; LIVE SUMMARY WHEN A GEOMETRY VALUE CHANGES
					(action_tile "accept" "(DCL:PresetAccept)")
					(action_tile "cancel" "(done_dialog 0)") ;; NOT DCL:Cancel, THAT DELETES THE DCL FILE THE SETTINGS DIALOG IS STILL USING
					
					(setq sts (start_dialog))
					(DCL:UpdatePresetInfo) ;; THE SETTINGS DIALOG IS CURRENT AGAIN ONCE START_DIALOG RETURNS
					sts
				)
				(set_tile "error" "Error: Tracker presets dialog could not be loaded.")
			)
		)
		;; OPENS THE NESTED TRACKER PRESETS DIALOG ON TOP OF THE SETTINGS DIALOG
		;; EDITS ARE MADE TO A WORKING COPY ('wrk'), 'prs' IS ONLY UPDATED WHEN THE PRESETS DIALOG IS ACCEPTED
		
		(defun DCL:PresetRefresh ( nam / ord )
			(setq ord (vl-sort-i (mapcar '(lambda ( pre ) (strcase (PresetText (json:Get pre "Name")))) wrk) '<))
			(setq wrk (mapcar '(lambda ( pos ) (nth pos wrk)) ord))
			(start_list "preset_list")
			(foreach pre wrk
				(add_list (PresetText (json:Get pre "Name")))
			)
			(end_list)
			(setq idx
				(if wrk
					(cond
						((vl-position (strcase (PresetText nam)) (mapcar '(lambda ( pre ) (strcase (PresetText (json:Get pre "Name")))) wrk)))
						(0)
					)
				)
			)
			(if idx
				(progn
					(set_tile "preset_list" (itoa idx))
					(DCL:PresetFill (nth idx wrk))
				)
				(DCL:PresetFill nil)
			)
			(DCL:PresetModes)
		)
		;; SORTS THE WORKING PRESETS BY NAME, RELOADS THE LIST, AND SELECTS THE PRESET NAMED [ nam ] (FIRST PRESET IF NOT FOUND)
		
		(defun DCL:PresetFill ( pre / pck hd )
			(foreach key (list "Name" "Manufacturer" "Description")
				(set_tile key (PresetText (json:Get pre key)))
			)
			(foreach key (list "Columns" "SizeX" "SizeY" "ModuleGap" "JointGapSize" "MotorGapSize" "TiltAngle" "PilingOverhangLeftSize" "PilingOverhangRightSize")
				(set_tile key (NumberString (json:Get pre key)))
			)
			(set_tile "layout"
				(if (setq pck (PPP:ParsePacks (json:Get pre "Packs")))
					(PacksToLayout pck)
					(PresetText (json:Get pre "Packs"))
				)
			)
			(setq hd (json:Items (json:Get pre "HorizontalDistances")))
			(set_tile "spacing"
				(if (and (= 'LIST (type hd)) (vl-every 'numberp hd))
					(SpacingToString hd)
					""
				)
			)
			(set_tile "error" "")
			(DCL:PresetSummary)
		)
		;; LOADS A PRESET INTO THE EDIT TILES, NIL CLEARS THEM
		
		(defun DCL:PresetModes ( / mde )
			(setq mde (if idx 0 1))
			(foreach key (list "Name" "Manufacturer" "Description" "Columns" "SizeX" "SizeY" "ModuleGap" "JointGapSize" "MotorGapSize" "TiltAngle" "layout" "spacing" "PilingOverhangLeftSize" "PilingOverhangRightSize" "preset_copy" "preset_pair" "preset_delete")
				(mode_tile key mde)
			)
		)
		;; DISABLES THE EDIT TILES AND PRESET BUTTONS WHILE NO PRESET IS SELECTED
		
		(defun DCL:PresetRead ( / lbl nam num pck hd dup pre )
			(setq lbl
				(list
					(cons "Columns" "Modules per tracker")
					(cons "SizeX" "Module length")
					(cons "SizeY" "Module width")
					(cons "ModuleGap" "Module gap")
					(cons "JointGapSize" "Joint gap")
					(cons "MotorGapSize" "Motor gap")
					(cons "TiltAngle" "Max tilt")
					(cons "PilingOverhangLeftSize" "Overhang north")
					(cons "PilingOverhangRightSize" "Overhang south")
				)
			) ;; NUMERIC TILE KEYS AND THEIR LABELS FOR ERROR MESSAGES
			(setq nam (vl-string-trim " " (get_tile "Name")))
			(if (wcmatch (strcase nam) "INT-*,EXT-*")
				(setq nam (strcat (strcase (substr nam 1 4)) (substr nam 5)))
			) ;; STANDARD UPPERCASE PREFIX, THE BOM READS THE ROW TYPE FROM IT
			(setq num (mapcar '(lambda ( itm ) (cons (car itm) (distof (vl-string-trim " " (get_tile (car itm))) 2))) lbl))
			(cond
				((= "" nam)
					"Preset name cannot be blank."
				)
				((and (setq dup (PPP:FindPreset wrk nam)) (/= idx (vl-position dup wrk)))
					(strcat "A preset named " nam " already exists.")
				)
				((setq dup (vl-some '(lambda ( itm ) (if (null (cdr itm)) (car itm))) num))
					(strcat (cdr (assoc dup lbl)) " must be a number.")
				)
				((null (setq pck (LayoutToPacks (get_tile "layout"))))
					"Module layout is not valid, enter module counts separated by commas and M for the motor."
				)
				((null (setq hd (StringToSpacing (get_tile "spacing"))))
					"Pile spacing is not valid, enter distances separated by commas (n@s repeats a spacing)."
				)
				( t
					(setq pre
						(json:Object
							(mapcar
								'(lambda ( key / val )
									(cons key
										(cond
											((= "Name" key) nam)
											((member key (list "Manufacturer" "Description")) (vl-string-trim " " (get_tile key)))
											((= "Packs" key) (PacksString pck))
											((= "HorizontalDistances" key) (json:Array hd))
											((and (= "Columns" key) (equal (setq val (cdr (assoc key num))) (fix val) 1e-8)) (fix val)) ;; WHOLE NUMBERS ARE STORED AS INTEGERS
											( t (cdr (assoc key num)))
										)
									)
								)
								(PPP:PresetKeys)
							)
						)
					)
					(cond
						((PPP:PresetProblem pre))
						(pre)
					)
				)
			)
		)
		;; BUILDS A PRESET FROM THE EDIT TILES
		;; RETURNS THE PRESET IF IT IS VALID, ELSE A MESSAGE STRING DESCRIBING THE FIRST PROBLEM
		
		(defun DCL:PresetCommit ( / res )
			(cond
				((null idx)
					t
				)
				((= 'STR (type (setq res (DCL:PresetRead))))
					(set_tile "error" (strcat "Error: " res))
					(set_tile "preset_list" (itoa idx)) ;; KEEP THE PRESET WITH THE PROBLEM SELECTED
					nil
				)
				( t
					(setq wrk (ReplaceNth wrk idx res))
					t
				)
			)
		)
		;; SAVES THE EDIT TILES TO THE SELECTED PRESET IN THE WORKING COPY
		;; RETURNS T IF SUCCESSFUL (OR NOTHING IS SELECTED), NIL AND SHOWS THE PROBLEM IF THE VALUES ARE NOT VALID
		
		(defun DCL:PresetSelect ( val / nam )
			(setq nam (PresetText (json:Get (nth (atoi val) wrk) "Name")))
			(if (and (/= (atoi val) idx) (DCL:PresetCommit))
				(DCL:PresetRefresh nam)
			)
		)
		;; LIST SELECTION CHANGED, SAVES THE CURRENT PRESET BEFORE SWITCHING (THE NAME MAY HAVE CHANGED THE SORT ORDER)
		
		(defun DCL:PresetNew ( / nam cnt )
			(if (DCL:PresetCommit)
				(progn
					(setq cnt 1)
					(while (PPP:FindPreset wrk (setq nam (strcat "INT-NEW" (itoa cnt))))
						(setq cnt (1+ cnt))
					)
					(setq wrk (cons (json:Object (list (cons "Name" nam) (cons "Manufacturer" mfr))) wrk))
					(DCL:PresetRefresh nam)
					(mode_tile "Name" 2)
				)
			)
		)
		;; ADDS AN EMPTY PRESET, IT MUST BE COMPLETED (OR DELETED) BEFORE ANOTHER PRESET CAN BE SELECTED
		
		(defun DCL:PresetDuplicate ( / pre nam cnt )
			(if (and idx (DCL:PresetCommit))
				(progn
					(setq
						pre (nth idx wrk)
						cnt 1
					)
					(while (PPP:FindPreset wrk (setq nam (strcat (json:Get pre "Name") "-COPY" (if (= 1 cnt) "" (itoa cnt)))))
						(setq cnt (1+ cnt))
					)
					(setq wrk (cons (json:Set pre "Name" nam) wrk))
					(DCL:PresetRefresh nam)
					(mode_tile "Name" 2)
				)
			)
		)
		;; COPIES THE SELECTED PRESET UNDER A NEW NAME
		
		(defun DCL:PresetPair ( / pre par pos )
			(if (and idx (DCL:PresetCommit))
				(progn
					(setq
						pre (nth idx wrk)
						pos (vl-position (PPP:FindPreset wrk (PPP:PairName (json:Get pre "Name"))) wrk)
						par (json:Set pre "Name" (if pos (json:Get (nth pos wrk) "Name") (PPP:PairName (json:Get pre "Name"))))
					)
					(if pos
						(setq par (json:Set par "Description" (json:Get (nth pos wrk) "Description")))
					) ;; AN EXISTING PAIR KEEPS ITS OWN DESCRIPTION
					(setq wrk (if pos (ReplaceNth wrk pos par) (cons par wrk)))
					(DCL:PresetRefresh (json:Get pre "Name"))
					(set_tile "error" (strcat (if pos "Updated " "Created ") (json:Get par "Name") " with the same geometry."))
				)
			)
		)
		;; CREATES OR UPDATES THE MATCHING INT / EXT PRESET WITH THE SELECTED PRESET'S GEOMETRY
		
		(defun DCL:PresetDelete ( / pos )
			(if (and idx (= 6 (wsh:ConfirmationDialog (strcat "Delete tracker preset " (PresetText (json:Get (nth idx wrk) "Name")) "?") "Tracker Presets" 0 36)))
				(progn
					(setq
						pos idx
						wrk (RemoveNth wrk idx)
						idx nil ;; THE DELETED PRESET IS NOT SAVED BY THE REFRESH
					)
					(DCL:PresetRefresh (if wrk (json:Get (nth (min pos (1- (length wrk))) wrk) "Name")))
				)
			)
		)
		;; DELETES THE SELECTED PRESET AFTER CONFIRMATION, THE NEXT PRESET IN THE LIST IS SELECTED
		
		(defun DCL:PresetImport ( / cur pth res add skp bad nam )
			(if (and
					(DCL:PresetCommit)
					(setq pth (getfiled "Import Tracker Presets" "" "PVsettings;json" 0))
				)
				(if (= 'STR (type (setq res (ImportPresetFile pth mfr))))
					(set_tile "error" (strcat "Error: " res))
					(progn
						(setq
							cur (if idx (json:Get (nth idx wrk) "Name"))
							add 0
							skp 0
							bad (cadr res)
						)
						(foreach pre (car res)
							(cond
								((PPP:PresetProblem pre)
									(setq bad (1+ bad))
								)
								((PPP:FindPreset wrk (json:Get pre "Name"))
									(setq skp (1+ skp)) ;; EXISTING PRESETS ARE NOT OVERWRITTEN, DELETE ONE FIRST TO REPLACE IT
								)
								( t
									(setq
										wrk (append wrk (list pre))
										add (1+ add)
									)
									(if (null nam)
										(setq nam (json:Get pre "Name"))
									)
								)
							)
						)
						(DCL:PresetRefresh (cond (nam) (cur)))
						(set_tile "error"
							(strcat
								"Imported " (itoa add) " preset" (if (= 1 add) "" "s")
								(if (> skp 0) (strcat ", " (itoa skp) " skipped (name already exists)") "")
								(if (> bad 0) (strcat ", " (itoa bad) " skipped (not valid or not supported)") "")
								"."
							)
						)
					)
				)
			)
		)
		;; IMPORTS PRESETS FROM A PVCASE .PVsettings FILE OR A PPP PRESET FILE, SELECTS THE FIRST IMPORTED PRESET
		
		(defun DCL:PresetSummary ( / res lay mot par )
			(set_tile "preset_summary"
				(cond
					((null idx)
						"No presets. Use New or Import... to add a tracker preset."
					)
					((= 'STR (type (setq res (DCL:PresetRead))))
						(strcat "Check: " res)
					)
					( t
						(setq
							lay (PPP:TrackerLayout res)
							mot (PPP:MotorPileIndex res)
							par (PPP:PairName (json:Get res "Name"))
						)
						(strcat
							"Tracker length " (rtos (car lay) 2 3) " ft, "
							(itoa (length (PPP:PilePositions res))) " piles, "
							(if mot (strcat "motor pile " (itoa mot)) "no motor") ", "
							(if (PPP:FindPreset wrk par) (strcat "pair " par) (strcat "no " (substr par 1 4) " pair yet"))
						)
					)
				)
			)
		)
		;; DISPLAYS THE CALCULATED TRACKER LENGTH, PILE COUNT, MOTOR PILE, AND INT / EXT PAIR, OR THE FIRST PROBLEM FOUND
		
		(defun DCL:PresetAccept ( / msg )
			(cond
				((not (DCL:PresetCommit))
					nil
				)
				((setq msg (vl-some '(lambda ( pre / prb ) (if (setq prb (PPP:PresetProblem pre)) (strcat (PresetText (json:Get pre "Name")) ": " prb))) wrk))
					(set_tile "error" (strcat "Error: " msg)) ;; CATCHES LOADED PRESETS THAT WERE NEVER SELECTED (EX. A HAND-EDITED FILE)
				)
				((setq msg (PairMismatch wrk))
					(set_tile "error" (strcat "Error: " msg))
				)
				( t
					(setq prs wrk)
					(done_dialog 1)
				)
			)
		)
		;; SAVES THE CURRENT PRESET, CHECKS EVERY PRESET AND INT / EXT PAIR, AND UPDATES 'prs' IF ALL CHECKS PASS

		
		(setq res nil)
		(if
			(and
				dcl
				(setq tmp (vl-filename-mktemp "PPPSETUP" nil ".dcl"))
				(setq des (open tmp "w"))
				(foreach line dcl (write-line line des))
				(not (setq des (close des)))
				(> (setq dch (load_dialog tmp)) 0)
				(new_dialog ttl dch)
			)
			(progn
				(foreach pop pops
					(start_list (car pop))
					(foreach itm (cdr pop) (add_list itm))
					(end_list)
				)
				;; INITIALIZE POPUP LISTS
				
				(foreach key keys
					(set_tile key
						(if (assoc key pops)
							(itoa (cond ((vl-position (cdr (assoc key cfg)) (cdr (assoc key pops)))) (0)))
							(cdr (assoc key cfg))
						)
					)
				)
				;; LOAD SAVED OR DEFAULT VALUES INTO EVERY TILE
				
				(DCL:UpdateCottRange (get_tile "cott_buckets"))
				(DCL:UpdateSlope (get_tile "table_slope"))
				(DCL:UpdatePresetInfo)
				
				(action_tile "accept" "(DCL:RunDialogChecks)")
				(action_tile "cancel" "(DCL:Cancel)")
				
				(action_tile "cott_buckets" "(DCL:UpdateCottRange $value)")
				(action_tile "table_slope" "(DCL:UpdateSlope $value)")
				(action_tile "presets" "(DCL:PresetsDialog)") ;; NESTED DIALOG, OPENS ON TOP OF THE SETTINGS DIALOG
				
				(setq sts (start_dialog))
				(unload_dialog dch)
				(setq dch nil)
				(if (findfile tmp)
					(vl-file-delete tmp)
				)
			)
			(prompt "\nError loading dialog box.")
		)
		(if (= 1 sts) res)
	)
	;; DISPLAYS PROJECT SETTINGS DIALOG PRE-FILLED WITH THE PASSED SETTINGS
	;; RETURNS THE UPDATED SETTINGS LIST IF ACCEPTED, NIL IF CANCELLED OR THE DIALOG FAILED TO LOAD
	
	(defun CollectNewObjects ( ent / lst )
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
	;; PASS THE RESULT OF (ENTLAST) BEFORE A COPY, NIL MEANS THE DRAWING WAS EMPTY AND ALL OBJECTS ARE RETURNED
	
	(defun FlattenArrayObjects ( lst / nam pnt adj skp )
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
	;; FLATTENS BLOCK REFERENCES AND LIGHTWEIGHT POLYLINES TO Z = 0, ARRAY ELEVATIONS WILL BE CALCULATED FROM THE CONTOUR SURFACE
	;; RETURNS ( ADJUSTED SKIPPED ) OBJECT COUNTS
	
	(defun ContourElevation ( obj / nam )
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
	;; RETURNS THE ELEVATION OF A CONTOUR POLYLINE, NIL IF THE OBJECT TYPE IS NOT SUPPORTED
	
	(defun ContourExtents ( lst / pmn pmx mn mx )
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
	;; RETURNS THE COMBINED BOUNDING BOX ( MINPOINT MAXPOINT ) OF THE PASSED OBJECTS, ELSE NIL
	
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
	
	(defun ValidateDrawingData ( arr cnt / lyr elv crv skp zro blk ext pnt obs msg ans )
		(setq lyr (strcase (cdr (assoc "contour_layers" cfg))) skp 0 zro 0)
		(foreach obj cnt
			(if (wcmatch (strcase (vla-get-layer obj)) lyr)
				(if (setq elv (ContourElevation obj))
					(progn
						(setq crv (cons obj crv))
						(if (equal elv 0.0 1e-8) (setq zro (1+ zro)))
					)
					(setq skp (1+ skp)) ;; OBJECT ON A CONTOUR LAYER BUT NOT A SUPPORTED POLYLINE TYPE
				)
			)
		)
		(setq blk (vl-remove-if-not '(lambda ( obj ) (PPP:IsArrayBlock obj cfg)) arr))
		(if (setq ext (ContourExtents crv))
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
				(prompt (strcat "\nError: No contour polylines were found on layers [" lyr "]."))
				nil
			)
			((= zro (length crv))
				(prompt "\nError: All contours have an elevation of zero. The contour file appears to be flattened.")
				nil
			)
			((null blk)
				(prompt
					(strcat
						"\nError: No array blocks matching [" (cdr (assoc "array_blocks" cfg))
						"] on layers [" (cdr (assoc "array_layers" cfg)) "] were found in the X-ARRAY file."
					)
				)
				nil
			)
			((= (length obs) (length blk))
				(prompt "\nError: No array blocks fall within the contour extents. Check that X-ARRAY and the contour file share the same coordinate system and units.")
				nil
			)
			( t
				(prompt
					(strcat
						"\nValidation: " (itoa (length crv)) " contours found"
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
	;; [ arr ] == OBJECTS COPIED FROM X-ARRAY, [ cnt ] == OBJECTS COPIED FROM THE CONTOUR FILE
	;; RETURNS T IF VALIDATION PASSES OR THE USER CHOOSES TO CONTINUE PAST WARNINGS, ELSE NIL
	
	(defun IsFileLocked ( pth / hnd )
		(if (setq hnd (open pth "a"))
			(progn
				(close hnd)
				nil
			)
			t
		)
	)
	;; CHECKS IF A FILE IS LOCKED AND UNABLE TO BE APPENDED TO
	;; RETURNS T IF THE FILE IS LOCKED, ELSE NIL
	
	(defun SaveProjectDrawing ( doc pth / err )
		(cond
			((and (findfile pth) (/= 6 (wsh:ConfirmationDialog (strcat "A drawing already exists at:\n\n" pth "\n\nOverwrite it?") "PPP Setup" 0 52)))
				(prompt "\nSave cancelled: existing drawing was not overwritten.")
				nil
			)
			((and (findfile pth) (IsFileLocked pth))
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
	;; "C:\_ACC\ACCDocs\ANS Team\100203 - Mill Creek Renewables - Orca Falls\Project Files\02 ENGINEERING\02 ELECTRICAL\^DWG\E-1000 OVERALL SITE PLAN.dwg"
	(setq cvl "01 CIVIL\\ENGINEERING\\PVCASE\\ISSUED\\") ;; INCONSISTENT NAMING FOR CIVIL DRAWINGS, THIS SHOULD LEAD TO THE CONTOUR DRAWING FILE
	(setq str "05 STRUCTURAL\\ENGINEERING\\") ;; PPP-BOM FOLDER IS CREATED UNDER THIS LOCATION
	(setq subs (list "1. PVCase" "2. BOM Processor" "3. Wind" "4. COTTzilla")) ;; STANDARD PPP-BOM SUB-DIRECTORIES
	(setq sav "1. PVCase") ;; SUB-DIRECTORY THE SETUP DRAWING IS SAVED TO, MATCHES WHERE THE PVCASE WORKING DRAWING IS SAVED TODAY
	(setq cfn (PPP:SettingsFile)) ;; PROJECT SETTINGS FILE, SAVED IN THE PPP-BOM ROOT
	(setq pfn (PPP:PresetsFile)) ;; TRACKER PRESETS FILE, SAVED IN THE PPP-BOM ROOT
	(setq xfn "X-ARRAY.dwg") ;; DEFAULT ARRAY FILE, USER CONFIRMS OR SELECTS A DIFFERENT FILE
	(setq ius (getvar "INSUNITS")) ;; ORIGINAL UNITS, RESTORED IF THE DRAWING IS NOT SAVED
	
	(setq funcs
		(list
			'std:CreateObjectDBX
			'std:ObjectRelease
			'std:CopyDBXObjects
			'wsh:ReturnFolder
			'wsh:ConfirmationDialog
			'json:ReadFile
			'json:WriteFile
			'json:Object
			'json:Array
			'json:Items
			'json:Has
			'json:Get
			'json:GetPath
			'json:Set
			'json:True-p
			'json:LastError
			'json:StringSplit
			'json:StringJoin
		)
	)
	
	(cond
		((/= (getvar "DWGTITLED") 0) ;; CHECK DRAWING IS NOT SAVED CURRENTLY
			(alert "Function must be run in a clean and unsaved drawing file.")
			;; WE'LL FIX THIS UP TO WORK WITH EXISTING FILES (UPDATED DRAWING) LATER
		)
		((null (vl-every 'boundp funcs))
			(prompt "\nRequired library functions are not loaded properly. Check support path.")
		)
		((null (setq dir (wsh:ReturnFolder "Select Project Root Location" 512 rt))) ;; UP TO THE USER TO SELECT THE APPROPRIATE PROJECT FOLDER ROOT LOCATION
			(prompt "\nNo project folder was selected.")
		)
		
		;; ESTABLISH PPP-BOM FOLDER STRUCTURE
		;; C:\_ACC\ACCDocs\ANS Team\XXXXXX - Client - Project Name\Project Files\02 ENGINEERING\05 STRUCTURAL\ENGINEERING\PPP-BOM
		((null (or (setq str (FindSubDirectory dir str 3)) (setq str (wsh:ReturnFolder "Select Structural Engineering Folder Location" 512 dir))))
			(prompt "\nStructural engineering directory was not established.")
		)
		((null (setq ppp (CreateDirectoryPath (strcat str "\\PPP-BOM"))))
			(prompt "\nFailed to create PPP-BOM directory.")
		)
		((null (CreateProjectFolders ppp subs))
			(prompt "\nFailed to create one or more PPP-BOM sub-directories.")
		)
		
		;; TRACKER PRESETS, AN UNREADABLE FILE STOPS SETUP SO IT IS NOT OVERWRITTEN WITH AN EMPTY LIST
		((and (null (setq prs (PPP:LoadPresets (strcat ppp "\\" pfn)))) (findfile (strcat ppp "\\" pfn)))
			(prompt (strcat "\nError: Tracker preset file could not be read, fix or rename it and run setup again:\n  " ppp "\\" pfn))
		)
		
		;; PROJECT SETTINGS, SAVED VALUES FROM A PREVIOUS RUN TAKE PRIORITY OVER DEFAULTS
		((null (progn (setq prs (json:Items prs)) (setq cfg (SettingsDialog (MergeSettings (PPP:LoadSettings (strcat ppp "\\" cfn)) (DefaultSettings dir))))))
			(prompt "\nSetup was cancelled.")
		)
		((null (PPP:SavePresets (strcat ppp "\\" pfn) prs))
			(prompt (strcat "\nError: Tracker presets could not be saved to " ppp "\\" pfn))
		)
		;; PRESETS ARE SAVED AS SOON AS THE SETTINGS DIALOG IS ACCEPTED SO PRESET WORK IS KEPT IF A LATER STEP STOPS SETUP
		
		;; FIND AND COPY X-ARRAY DATA
		;; C:\_ACC\ACCDocs\ANS Team\XXXXXX - Client - Project Name\Project Files\02 ENGINEERING\02 ELECTRICAL\^DWG\XREF\X-ARRAY.DWG
		;; 'or' RETURNS THE FIRST NON-NIL VALUE, SO THE BROWSE DIALOG ONLY RUNS WHEN THE SEARCH COMES BACK EMPTY
		((null (or (setq elc (FindSubDirectory dir elc 3)) (setq elc (wsh:ReturnFolder "Select X-ARRAY.dwg File Location" 512 dir))))
			(prompt "\nX-ARRAY file directory was not established.")
		)
		;; X-ARRAY.DWG IS THE DEFAULT, BUT SOME PROCESSES USE A DIFFERENT OR SLIGHTLY RENAMED FILE, LET THE USER CONFIRM OR OVERRIDE IT
		((and
				(or
					(null (setq fn (if (findfile (strcat elc "\\" xfn)) xfn))) ;; NO DEFAULT FILE FOUND, SKIP STRAIGHT TO FILE SELECTION
					(/= 6 (wsh:ConfirmationDialog (strcat "Use the X-ARRAY file found in the electrical location?\n\n" elc "\\" fn) "X-ARRAY File" 0 36))
				)
				(null (setq fn (getfiled "Select X-ARRAY Drawing File" (strcat elc "\\") "dwg" 16)))
			)
			(prompt "\nX-ARRAY file was not selected.")
		)
		;; 'fn' IS EITHER THE DEFAULT FILE NAME OR A FULL PATH RETURNED BY GETFILED, SAME HANDLING AS THE CONTOUR FILE BELOW
		((null (progn (setq ent (entlast)) (CopyDrawingContents (if (= "" (vl-filename-directory fn)) elc (vl-filename-directory fn)) (strcat (vl-filename-base fn) (vl-filename-extension fn))))) ;; MARK THE LAST ENTITY SO COPIED OBJECTS CAN BE COLLECTED
			(prompt "\nFailed to copy X-ARRAY file from electrical location.")
		)
		((null (setq arr (CollectNewObjects ent)))
			(prompt "\nNo objects were copied from the X-ARRAY file.")
		)
		((null (setq flt (FlattenArrayObjects arr)))
			(prompt "\nAn error occured while flattening data.")
		)
		;; ARRAYS ARE FLATTENED TO Z = 0, ELEVATIONS WILL BE CALCULATED FROM THE CONTOUR SURFACE IN LATER STAGES
		
		;; FIND AND COPY CONTOUR DATA
		;; C:\_ACC\ACCDocs\ANS Team\XXXXXX - Client - Project Name\Project Files\02 ENGINEERING\01 CIVIL\ENGINEERING\PVCASE\ISSUED\???.DWG
		((null (or (setq cvl (FindSubDirectory dir cvl 3)) (setq cvl (wsh:ReturnFolder "Select Civil Contour File Location" 512 dir))))
			(prompt "\nContour file directory was not established.")
		)
		;; FILE NAMING IS INCONSISTENT HERE, PULL THE MOST RECENTLY MODIFIED DRAWING AND LET THE USER CONFIRM OR OVERRIDE IT
		((null (setq fn (FindNewestFile cvl "*.dwg")))
			(prompt "\nNo contour drawing files were found in the Civil location.")
		)
		((and
				(/= 6 (wsh:ConfirmationDialog (strcat "Use the most recently modified contour file?\n\n" fn) "Civil Contour File" 0 36))
				(null (setq fn (getfiled "Select Civil Contour Drawing File" (strcat cvl "\\") "dwg" 16)))
			)
			(prompt "\nContour file was not selected.")
		)
		;; 'fn' IS EITHER A FILE NAME FROM THE DIRECTORY SCAN OR A FULL PATH RETURNED BY GETFILED (VL-FILENAME-DIRECTORY RETURNS "" WHEN THERE IS NO PATH)
		((null (progn (setq ent (entlast)) (CopyDrawingContents (if (= "" (vl-filename-directory fn)) cvl (vl-filename-directory fn)) (strcat (vl-filename-base fn) (vl-filename-extension fn)))))
			(prompt "\nFailed to copy contour file from Civil location.")
		)
		((null (setq cnt (CollectNewObjects ent)))
			(prompt "\nNo objects were copied from the contour file.")
		)
		
		;; VALIDATE, SET UNITS, AND SAVE
		((null (ValidateDrawingData arr cnt))
			(prompt "\nSetup stopped: drawing data did not pass validation.")
		)
		
		;; ASSIGN ARRAY BLOCKS TO TRACKER PRESETS BEFORE THE DRAWING IS SAVED
		;; THIS STEP NEVER STOPS SETUP, IF IT IS CANCELLED OR FAILS PPPASSIGN CAN BE RUN IN THE SAVED DRAWING
		((progn
				(setq asn (vl-catch-all-apply 'PPPAssignMain (list ppp cfg prs)))
				(cond
					((vl-catch-all-error-p asn)
						(prompt (strcat "\nWarning: PPPAssign stopped with an error: " (vl-catch-all-error-message asn)))
						(setq asn nil)
					)
					(asn
						(setq cfg (cdr (assoc "cfg" asn))) ;; KEEPS ANY EDGE SETTING CHANGES MADE IN THE ASSIGN DIALOG
					)
				)
				nil
			)
		)
		((null (setvar "INSUNITS" (if (= "US Survey Feet" (cdr (assoc "units" cfg))) 21 2))) ;; 2 = FEET, 21 = US SURVEY FEET
			(prompt "\nFailed to set drawing units.")
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
					"\n  Arrays flattened: " (itoa (car flt)) (if (> (cadr flt) 0) (strcat " (" (itoa (cadr flt)) " skipped, check for locked layers)") "")
					(if (and ign (> ign 0)) (strcat "\n  Array blocks ignored (outside contours, shown in red): " (itoa ign)) "")
					"\n  COTT range: " (rtos (cdr (assoc "cott_min" cfg)) 2 3) " to " (rtos (cdr (assoc "cott_max" cfg)) 2 3) " ft"
					"\n  Max table slope: " (rtos (cdr (assoc "table_slope_deg" cfg)) 2 3) " degrees"
					"\n  Settings saved to: " ppp "\\" cfn
					"\n  Tracker presets: " (itoa (length prs)) " saved to " ppp "\\" pfn
					(if asn
						(strcat "\n  Array blocks assigned: " (itoa (cdr (assoc "assigned" asn))) " of " (itoa (cdr (assoc "blocks" asn))) " (" (itoa (cdr (assoc "exterior" asn))) " exterior)")
						"\n  Array blocks were not assigned, run PPPASSIGN in this drawing."
					)
					(if (= "0" (cdr (assoc "eor_confirmed" cfg))) "\n  Note: COTT range has not been confirmed with the engineer of record." "")
				)
			)
		)
	)
	(if (and ius (not svd))
		(setvar "INSUNITS" ius)
	) ;; RESTORE ORIGINAL UNITS IF THE RUN STOPPED BEFORE SAVING
	
	(princ)
)



;;; PPP ASSIGN ;;;
;; ASSIGNS EVERY ARRAY BLOCK A TRACKER PRESET AND AN INTERIOR / EXTERIOR ROW TYPE, STORED ON THE BLOCK AS 'PPPASSIGN' XDATA
;; CALLED AT THE END OF PPPSETUP (BEFORE THE DRAWING IS SAVED) AND RUN BY THE USER AS PPPASSIGN FOR REPLACEMENTS / UPDATES
;;
;; BLOCK TYPE -> TRACKER:
;;  EACH ARRAY BLOCK NAME IS ASSIGNED A TRACKER (A PRESET NAME WITHOUT ITS INT- / EXT- PREFIX, EX. "28M56") IN THE ASSIGN DIALOG
;;  THE BLOCK LENGTH GIVES THE MODULE COUNT, THE MOTOR OBJECT IN THE BLOCK (PVCASE LAYER E-TRACKER-MOTOR) PICKS BETWEEN
;;  TRACKERS WITH THE SAME LENGTH (EX. 28M56 / 56M28), ANYTHING NOT DETECTED IS CHOSEN BY THE USER IN THE ASSIGN DIALOG
;;  CHOICES ARE SAVED TO PPP-ASSIGN.json IN THE PPP-BOM ROOT AND PRE-FILLED THE NEXT TIME
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
;;  PRESET NAME ("INT-28M56"), ROW TYPE ("interior" / "exterior", MATCHES THE BOM 'Row type'), EXPOSED SIDES ("W", "E", "WE", "NONE")
;;  BLOCKS THAT ARE IGNORED, UNSUPPORTED, OR LEFT UNASSIGNED HAVE ANY OLD 'PPPASSIGN' XDATA REMOVED

(defun PPPAssignMain
	(
		ppp cfg prs
		/
		;Functions
		*error* DebugStep
		FindProjectRoot SettingValue SetSetting IsIgnored TrackerPairs FindPair MotorText TrackerLabel
		LoadAssignFile SaveAssignFile SavedTracker TypeSource DefinitionData CollectArrayBlocks SuggestTracker
		AssignDialog ClassifyTrackers WriteAssignment ClearAssignment
		;Variables
		sta doc und afn funcs pairs col typs sugs map sel skp nrw alw nmp cls
		trk sid pre row cnt ext una fal clr stats svm svc out
		;DCL Functions
		DCL:BlockRow DCL:ListRefresh DCL:Mismatch DCL:ShowBlock DCL:BlockSelect DCL:TrackerSelect DCL:Summary
		DCL:Show DCL:ShowExample DCL:Accept
		;DCL Variables
		ttl dcl tmp des dch res idx opt ers est
	)
	
	(defun *error* ( msg / cxl )
		(setq cxl (wcmatch (strcase msg) "*CANCEL*,QUIT / EXIT ABORT")) ;; ESC OR (EXIT), NOT A PROGRAM ERROR
		(if (and dch (< 0 dch))
			(unload_dialog dch)
		)
		(if (eq (type des) 'FILE)
			(close des)
		)
		(if (and (eq (type tmp) 'STR) (findfile tmp))
			(vl-file-delete tmp)
		)
		(if und
			(vla-endundomark doc)
		) ;; CLOSES THE UNDO GROUP SO A SINGLE UNDO REVERTS ANY BLOCKS ALREADY UPDATED
		(if (not cxl)
			(vl-bt)
		) ;; FOR DEBUGGING
		(ECHO:Log-Verbose (if cxl "info" "ERROR") (if cxl "cancelled by user" msg) nil (list (cons "CMDNAME" *ECHO:Log-Command*)))
		(ECHO:Log-Flush)
		(princ (if cxl "\nPPP assign cancelled, use UNDO to revert any blocks already updated." (strcat "\nError: " msg)))
		(princ)
	)
	;; ONLY ACTIVE WHEN RUN AS THE PPPASSIGN COMMAND, PPPSETUP CALLS THIS FUNCTION THROUGH VL-CATCH-ALL-APPLY SO AN ERROR HERE DOES NOT STOP SETUP
	;; THE DIALOG IS LOADED AND UNLOADED WITHIN AssignDialog, SO NO DIALOG IS LEFT OPEN WHEN BLOCKS ARE BEING UPDATED
	
	
	(defun DebugStep ( msg / )
		(if (and (boundp '*PPP:Trace*) *PPP:Trace*)
			(progn
				(prompt (strcat "\n[PPPASSIGN " (itoa (getvar "MILLISECS")) "] " msg))
				(princ) ;; PRINT NOW, SO THE LAST STEP IS VISIBLE IF AUTOCAD STOPS
			)
		)
	)
	;; DEBUG STEP MESSAGES, ONLY PRINTED WHEN THE USER SETS (setq *PPP:Trace* T) AT THE COMMAND LINE, (setq *PPP:Trace* nil) TURNS THEM OFF
	
	
	;; PROJECT DATA ;;
	
	(defun FindProjectRoot ( / dir )
		(setq dir (vl-filename-directory (vl-string-right-trim "\\" (getvar "DWGPREFIX"))))
		(cond
			((findfile (strcat dir "\\" (PPP:SettingsFile)))
				dir
			)
			((setq dir (wsh:ReturnFolder "Select PPP-BOM Folder Location" 512 dir))
				(setq dir (vl-string-right-trim "\\" dir))
				(if (findfile (strcat dir "\\" (PPP:SettingsFile)))
					dir
				)
			)
		)
	)
	;; RETURNS THE PPP-BOM ROOT: ONE LEVEL ABOVE THE DRAWING, OR A FOLDER SELECTED BY THE USER IF THE SETTINGS FILE IS NOT THERE
	;; !REVISIT! THE PROJECT FILE LOCATION MAY CHANGE WITH THE TEAM PROCESS
	;; RETURNS NIL IF NO FOLDER WITH A SETTINGS FILE WAS FOUND OR SELECTED
	
	(defun SettingValue ( key def / val num )
		(if (and (= 'STR (type (setq val (cdr (assoc key cfg))))) (setq num (distof (vl-string-trim " " val) 2)) (>= num 0))
			num
			def
		)
	)
	;; RETURNS A NUMERIC PROJECT SETTING OF ZERO OR GREATER, [ def ] IF THE SETTING IS MISSING OR NOT VALID
	
	(defun SetSetting ( key val / old )
		(setq cfg
			(if (setq old (assoc key cfg))
				(subst (cons key val) old cfg)
				(append cfg (list (cons key val)))
			)
		)
	)
	;; SETS A PROJECT SETTING IN 'cfg', MISSING KEYS ARE ADDED AT THE END
	
	(defun IsIgnored ( ent / xdt )
		(and
			(setq xdt (cdr (assoc -3 (entget ent (list "PPPSETUP")))))
			(member (cons 1000 "IGNORE-OUTSIDE-CONTOURS") (cdr (assoc "PPPSETUP" xdt)))
		)
	)
	;; RETURNS T IF PPPSETUP FLAGGED THE BLOCK AS OUTSIDE THE CONTOURS (SEE FlagIgnoredObjects IN PPPSetupMain), ELSE NIL
	
	
	;; TRACKERS ;;
	
	(defun TrackerPairs ( / nam par lay nmn fnd out )
		(foreach pre prs
			(setq nam (json:Get pre "Name"))
			(if (and
					(= 'STR (type nam))
					(wcmatch (strcase nam) "INT-?*")
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
								(substr nam 5) ;; 0 TRACKER NAME, THE PRESET NAME WITHOUT ITS PREFIX
								(apply '+ (json:Items (json:Get pre "HorizontalDistances"))) ;; 1 LENGTH BETWEEN THE OVERHANG LINES, MATCHES THE BLOCK LENGTH
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
				(LoadAssignFile pth)
			)
		) ;; ENTRIES FOR BLOCK NAMES NOT IN THIS DRAWING ARE KEPT
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
	
	(defun DefinitionData ( nam / mn mx box ext ivs mrg )
		(vlax-for obj (vla-item (vla-get-blocks doc) nam)
			(if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-getboundingbox (list obj 'mn 'mx))))
				(setq box (cons (list (vlax-safearray->list mn) (vlax-safearray->list mx) (strcase (vla-get-layer obj))) box))
			)
		)
		(if box
			(progn
				(setq ext
					(list
						(apply 'min (mapcar 'caar box))
						(apply 'min (mapcar 'cadar box))
						(apply 'max (mapcar 'caadr box))
						(apply 'max (mapcar 'cadadr box))
					)
				) ;; ( MINX MINY MAXX MAXY ) IN BLOCK COORDINATES
				(foreach itm box
					(if (wcmatch (caddr itm) "*MOTOR*") ;; PVCASE DRAWS THE MOTOR ON LAYER E-TRACKER-MOTOR
						(setq ivs (cons (list (- (nth 3 ext) (cadadr itm)) (- (nth 3 ext) (cadar itm))) ivs)) ;; DISTANCES FROM THE NORTH EDGE
					)
				)
				(foreach itm (vl-sort ivs '(lambda ( a b ) (< (car a) (car b))))
					(if (and mrg (<= (car itm) (+ (cadr (car mrg)) 1e-6)))
						(setq mrg (cons (list (car (car mrg)) (max (cadr (car mrg)) (cadr itm))) (cdr mrg)))
						(setq mrg (cons itm mrg))
					)
				) ;; OVERLAPPING MOTOR OBJECTS (OUTLINE AND SYMBOL) MERGE INTO ONE MOTOR
				(list
					(- (nth 3 ext) (nth 1 ext))
					(- (nth 2 ext) (nth 0 ext))
					ext
					(mapcar '(lambda ( iv ) (* 0.5 (+ (car iv) (cadr iv)))) (reverse mrg))
				)
			)
		)
	)
	;; MEASURES AN ARRAY BLOCK DEFINITION, CALCULATED ONCE PER BLOCK NAME
	;; RETURNS ( LENGTH WIDTH EXTENTS MOTORS ), NIL IF THE BLOCK HAS NO MEASURABLE OBJECTS
	;; MOTORS == CENTER OF EACH MOTOR FROM THE NORTH EDGE OF THE BLOCK (THE NORTH OVERHANG LINE), NIL IF NO MOTOR OBJECT WAS FOUND
	;; VERIFIED ON ORCA FALLS '1P84@60DEG F TR ID8 PVBlock': MOTOR CENTER 253.086 FT == 56M28, MOTOR HEIGHT 5.922 FT == MOTOR GAP
	
	(defun CollectArrayBlocks ( / ent nam rot dfs dfd ins trk ign uns )
		(vlax-for obj (vla-get-modelspace doc)
			(setq ent (vlax-vla-object->ename obj))
			(cond
				((not (PPP:IsArrayBlock obj cfg))
					nil
				)
				((IsIgnored ent)
					(setq ign (cons ent ign))
				)
				((not
						(and
							(equal 0.0 (sin (setq rot (vla-get-rotation obj))) 1e-6)
							(> (cos rot) 0)
							(equal 1.0 (vla-get-xscalefactor obj) 1e-6)
							(equal 1.0 (vla-get-yscalefactor obj) 1e-6)
						)
					)
					(setq uns (cons ent uns)) ;; ONLY NORTH-SOUTH TRACKERS AT UNIT SCALE ARE SUPPORTED, THE NORTH END MUST BE THE TOP OF THE BLOCK
				)
				((null
						(setq dfd
							(cond
								((assoc (setq nam (vla-get-effectivename obj)) dfs)
									(cdr (assoc nam dfs))
								)
								( t
									(DebugStep (strcat "reading block definition: " nam))
									(setq dfd (vl-catch-all-apply 'DefinitionData (list nam)))
									(if (vl-catch-all-error-p dfd)
										(setq dfd nil)
									)
									(setq dfs (cons (cons nam dfd) dfs))
									(DebugStep (if dfd (strcat "  motors found: " (itoa (length (nth 3 dfd)))) "  not measurable"))
									dfd
								)
							)
						)
					)
					(setq uns (cons ent uns))
				)
				( t
					(setq ins (vlax-get obj 'InsertionPoint))
					(setq trk
						(cons
							(list
								ent
								nam
								(+ (car ins) (* 0.5 (+ (nth 0 (caddr dfd)) (nth 2 (caddr dfd))))) ;; TRACKER CENTER LINE
								(+ (cadr ins) (nth 1 (caddr dfd))) ;; SOUTH EDGE
								(+ (cadr ins) (nth 3 (caddr dfd))) ;; NORTH EDGE
								(cadr dfd) ;; WIDTH
							)
							trk
						)
					)
				)
			)
		) ;; WALKS MODEL SPACE DIRECTLY LIKE PPPSETUP VALIDATION, SSGET "_X" MISSED ONE BLOCK WHEN RUN FROM PPPSETUP
		(list trk ign uns (vl-remove-if-not 'cdr dfs))
	)
	;; COLLECTS THE ARRAY BLOCKS IN MODEL SPACE, EVERY MODEL SPACE OBJECT IS CHECKED WITH PPP:IsArrayBlock (LAYER AND BLOCK NAME PATTERNS)
	;; RETURNS ( TRACKERS IGNORED UNSUPPORTED DEFINITIONS )
	;; TRACKERS == ( ENAME BLOCK-NAME X SOUTH NORTH WIDTH ) ITEMS, IGNORED / UNSUPPORTED == ENAME LISTS, DEFINITIONS == ( BLOCK-NAME . DefinitionData ) PAIRS
	
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
	;; SUGGESTS A TRACKER FOR A BLOCK DEFINITION [ dfd ] (SEE DefinitionData)
	;; RETURNS ( TRACKER-NAME-OR-NIL HINT-MESSAGE DETECTED ), DETECTED IS T ONLY WHEN THE LENGTH AND MOTOR POSITION BOTH MATCH ONE TRACKER
	
	
	;; DIALOG ;;
	
	(defun AssignDialog ( / sts )
		(setq ttl "PPP_Assign")
		(setq dcl
			(list
				"// Temporary DCL file;"
				(strcat ttl " : dialog {")
				"	label = \"Pile Plot Plan Assign - Array Blocks to Tracker Presets\";"
				"	: boxed_column {"
				"		label = \"Array block types\";"
				"		: list_box {"
				"			label = \"Block name / Blocks / Tracker / Set by\";"
				"			key = \"block_list\";"
				"			width = 96;"
				"			height = 12;"
				"			tabs = \"46 54 64\";"
				"		}"
				"		: row {"
				"			: popup_list {"
				"				label = \"Tracker preset:\";"
				"				key = \"tracker\";"
				"				edit_width = 56;"
				"			}"
				"			: button {"
				"				label = \"Show in drawing\";"
				"				key = \"show\";"
				"				width = 18;"
				"				fixed_width = true;"
				"			}"
				"		}"
				"		: text {"
				"			key = \"block_info\";"
				"			width = 96;"
				"		}"
				"		: text {"
				"			key = \"block_hint\";"
				"			width = 96;"
				"		}"
				"	}"
				"	: boxed_column {"
				"		label = \"Edge classification\";"
				"		: row {"
				"			: edit_box {"
				"				label = \"Exterior rows per side:\";"
				"				key = \"exterior_rows\";"
				"				edit_width = 8;"
				"			}"
				"			: edit_box {"
				"				label = \"Stagger allowance (ft):\";"
				"				key = \"stagger_allowance\";"
				"				edit_width = 8;"
				"			}"
				"		}"
				"		: text {"
				"			label = \"Exterior = first trackers in from the east and west sides of each group, north and south ends do not count.\";"
				"		}"
				"		: text {"
				"			label = \"Stagger allowance = end offset allowed between neighboring trackers before the exposed part is an edge.\";"
				"		}"
				"	}"
				"	: text {"
				"		key = \"summary\";"
				"		width = 96;"
				"	}"
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
			(start_list "block_list")
			(repeat (length typs)
				(add_list (DCL:BlockRow (setq pos (1+ pos))))
			)
			(end_list)
			(set_tile "block_list" (itoa idx))
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
			(start_list "tracker")
			(foreach itm (cons "(not assigned)" (mapcar 'TrackerLabel opt))
				(add_list itm)
			)
			(end_list)
			(set_tile "tracker" (itoa (if trk (1+ (vl-position trk opt)) 0)))
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
				rws (distof (vl-string-trim " " (get_tile "exterior_rows")) 2)
				stg (distof (vl-string-trim " " (get_tile "stagger_allowance")) 2)
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
			ers (cond ((cdr (assoc "exterior_rows" cfg))) ("2"))
			est (cond ((cdr (assoc "stagger_allowance" cfg))) ("0.0"))
		)
		(if
			(and
				dcl
				(setq tmp (vl-filename-mktemp "PPPASSIGN" nil ".dcl"))
				(setq des (open tmp "w"))
				(foreach line dcl (write-line line des))
				(not (setq des (close des)))
				(> (setq dch (load_dialog tmp)) 0)
			)
			(progn
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
				(unload_dialog dch)
				(setq dch nil)
				(if (findfile tmp)
					(vl-file-delete tmp)
				)
			)
			(prompt "\nError loading dialog box.")
		)
		(if (= 1 sts) res)
	)
	;; DISPLAYS THE BLOCK TYPE ASSIGNMENT DIALOG, PRE-FILLED WITH 'sel'
	;; RETURNS ( SELECTIONS EXTERIOR-ROWS STAGGER-ALLOWANCE EXTERIOR-ROWS-TEXT STAGGER-ALLOWANCE-TEXT ) IF ACCEPTED, NIL IF CANCELLED
	
	
	;; CLASSIFICATION ;;
	
	(defun ClassifyTrackers ( trk nrw alw / SubtractSpan GroupByIndex Exposed tol scn srt cnd cnt lst rst a b i j dx mnd key itm pit mxd nbe ord enb wnb )
		(defun SubtractSpan ( ivs lo hi / out )
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
		;; RETURNS THE ( START END ) INTERVALS [ ivs ] WITH THE SPAN [ lo ] TO [ hi ] REMOVED
		
		(defun GroupByIndex ( prs num / pos cur out )
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
		
		(defun Exposed ( pos nbl lo hi lvl / nbs ivs )
			(setq
				nbs (nth pos nbl)
				ivs (list (list lo hi))
			)
			(foreach nbr nbs
				(setq ivs (SubtractSpan ivs (- (nth 3 (nth nbr srt)) alw) (+ (nth 4 (nth nbr srt)) alw)))
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
		
		(DebugStep (strcat "classify: sorting " (itoa (length trk)) " trackers"))
		(setq
			tol (PPP:PresetTolerance)
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
				(progn
					(setq key (fix (+ 0.5 (* 10.0 mnd)))) ;; NEAREST 0.1 FT
					(setq cnt
						(if (setq itm (assoc key cnt))
							(subst (cons key (1+ (cdr itm))) itm cnt)
							(cons (cons key 1) cnt)
						)
					)
				)
			)
			(setq
				lst (cdr lst)
				i (1+ i)
			)
		)
		;; EVERY OVERLAPPING PAIR OF TRACKERS WITHIN THE SEARCH DISTANCE, AND A COUNT OF EACH TRACKER'S NEAREST EAST SPACING
		
		(DebugStep (strcat "classify: " (itoa (length cnd)) " overlapping pairs, " (itoa (length cnt)) " spacings"))
		(setq
			pit (if cnt (/ (car (car (vl-sort cnt '(lambda ( p q ) (> (cdr p) (cdr q)))))) 10.0)) ;; MOST COMMON SPACING == ROW PITCH
			mxd (if pit (* 1.5 pit) 0.0) ;; !REVISIT! NEIGHBOR DISTANCE RULE, 1.5 x PITCH BREAKS A GROUP AT ANY MISSING ROW
			nbe (reverse (vl-remove-if '(lambda ( itm ) (> (caddr itm) mxd)) cnd)) ;; ( WEST EAST DX ), SORTED BY THE WEST INDEX
			ord (vl-sort-i (mapcar 'cadr nbe) '<)
			enb (GroupByIndex (mapcar '(lambda ( itm ) (cons (car itm) (cadr itm))) nbe) (length srt))
			wnb (GroupByIndex (mapcar '(lambda ( pos / itm ) (setq itm (nth pos nbe)) (cons (cadr itm) (car itm))) ord) (length srt))
			i -1
		)
		(DebugStep (strcat "classify: pitch " (if pit (rtos pit 2 1) "none") ", " (itoa (length nbe)) " neighbor pairs, checking exposure"))
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
	;; IF NO PITCH IS FOUND (EX. A SINGLE ROW) NO TRACKER HAS NEIGHBORS AND ALL ARE EXTERIOR
	;; VALIDATED AGAINST THE ORCA FALLS PVCASE BOM: ALL 3,304 TRACKERS MATCH WITH 2 EXTERIOR ROWS AND A STAGGER ALLOWANCE OF 2.5 FT OR MORE
	
	
	;; XDATA ;;
	
	(defun WriteAssignment ( ent pre row sid / )
		(entmod (append (entget ent) (list (list -3 (list "PPPASSIGN" (cons 1000 pre) (cons 1000 row) (cons 1000 (if (= "" sid) "NONE" sid)))))))
	)
	;; WRITES THE 'PPPASSIGN' XDATA TO A BLOCK, OTHER APPLICATIONS' XDATA (EX. 'PPPSETUP') IS KEPT
	;; RETURNS THE UPDATED ENTITY LIST, NIL IF THE BLOCK COULD NOT BE UPDATED (EX. LOCKED LAYER)
	
	(defun ClearAssignment ( ent / )
		(if (assoc -3 (entget ent (list "PPPASSIGN")))
			(entmod (append (entget ent) (list (list -3 (list "PPPASSIGN")))))
			t
		)
	)
	;; REMOVES ANY 'PPPASSIGN' XDATA FROM A BLOCK (AN APPLICATION NAME WITH NO VALUES DELETES THAT APPLICATION'S XDATA)
	;; RETURNS NIL IF THE BLOCK HAD XDATA THAT COULD NOT BE REMOVED, ELSE NON-NIL
	
	
	(setq
		sta (null ppp) ;; T WHEN RUN AS THE PPPASSIGN COMMAND, NIL WHEN CALLED BY PPPSETUP WITH ITS PROJECT DATA
		doc (vla-get-activedocument (vlax-get-acad-object))
	)
	(setq funcs
		(list
			'wsh:ReturnFolder
			'wsh:ConfirmationDialog
			'json:ReadFile
			'json:WriteFile
			'json:Object
			'json:Array
			'json:Items
			'json:Get
			'json:StringSplit
			'json:StringJoin
		)
	)
	
	(DebugStep (if sta "start (PPPASSIGN command)" "start (called by PPPSETUP)"))
	(cond
		((null (vl-every 'boundp funcs))
			(prompt "\nRequired library functions are not loaded properly. Check support path.")
		)
		
		;; PROJECT DATA, PASSED IN BY PPPSETUP OR READ FROM THE PPP-BOM ROOT WHEN RUN AS A COMMAND
		((and sta (= 0 (getvar "DWGTITLED")))
			(prompt "\nPPPAssign must be run in a saved PPP setup drawing, run PPPSetup first.")
		)
		((and sta (null (setq ppp (FindProjectRoot))))
			(prompt (strcat "\nError: " (PPP:SettingsFile) " was not found in the PPP-BOM folder, run PPPSetup first."))
		)
		((and sta (null (setq cfg (PPP:LoadSettings (strcat ppp "\\" (PPP:SettingsFile))))))
			(prompt (strcat "\nError: Project settings could not be read from " ppp "\\" (PPP:SettingsFile)))
		)
		((and sta (null (setq prs (PPP:LoadPresets (strcat ppp "\\" (PPP:PresetsFile))))))
			(prompt (strcat "\nError: Tracker presets could not be read from " ppp "\\" (PPP:PresetsFile)))
		)
		((null
				(progn
					(if sta
						(setq prs (json:Items prs))
					) ;; PPPSETUP PASSES THE PRESET LIST, THE FILE RETURNS THE TAGGED ARRAY
					(DebugStep "project data loaded, reading tracker presets")
					(setq pairs (TrackerPairs))
				)
			)
			(prompt "\nError: No valid INT- / EXT- tracker preset pairs were found. Add presets in the PPPSetup presets dialog.")
		)
		
		;; ARRAY BLOCKS
		((null
				(progn
					(DebugStep (strcat (itoa (length pairs)) " trackers, collecting array blocks"))
					(car (setq col (CollectArrayBlocks)))
				)
			)
			(prompt
				(strcat
					"\nError: No array blocks matching [" (cdr (assoc "array_blocks" cfg)) "] on layers [" (cdr (assoc "array_layers" cfg)) "] were found."
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
							) ;; ( BLOCK-NAME COUNT DefinitionData ) FOR EACH BLOCK NAME IN USE
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
					(DebugStep (strcat (itoa (length typs)) " block types, " (if skp "all detected, skipping the dialog" "opening the dialog")))
					(if skp
						(setq res
							(list
								sel
								(fix (SettingValue "exterior_rows" 2))
								(SettingValue "stagger_allowance" 0.0)
								(cond ((cdr (assoc "exterior_rows" cfg))) ("2"))
								(cond ((cdr (assoc "stagger_allowance" cfg))) ("0.0"))
							)
						) ;; FROM PPPSETUP, EVERY BLOCK TYPE DETECTED WITH NO CONFLICTING SAVED ASSIGNMENT, NO NEED TO ASK
						(AssignDialog)
					)
				)
			)
			(prompt "\nPPP assign was cancelled, no blocks were changed.")
		)
		
		;; CLASSIFY AND WRITE XDATA
		( t
			(setq
				sel (nth 0 res)
				nrw (nth 1 res)
				alw (nth 2 res)
				nmp (mapcar '(lambda ( typ nam ) (cons (car typ) nam)) typs sel) ;; BLOCK NAME -> TRACKER NAME
			)
			(if sta
				(progn
					(vla-startundomark doc)
					(setq und t)
				)
			)
			(DebugStep (strcat "dialog done, " (itoa nrw) " exterior rows, " (rtos alw 2 3) " ft stagger"))
			(regapp "PPPASSIGN") ;; RETURNS NIL IF ALREADY REGISTERED, WHICH IS FINE
			(setq
				cls (ClassifyTrackers (car col) nrw alw)
				cnt 0
				ext 0
				una 0
				fal 0
				clr 0
			)
			(DebugStep "classify done, writing xdata")
			(mapcar
				'(lambda ( itm sid / itm2 )
					(if (= 0 (rem (+ cnt una fal) 500))
						(DebugStep (strcat "  " (itoa (+ cnt una fal)) " blocks processed"))
					)
					(if (setq trk (FindPair (cdr (assoc (cadr itm) nmp))))
						(progn
							(setq
								pre (if (= "" sid) (nth 6 trk) (nth 7 trk))
								row (if (= "" sid) "interior" "exterior")
							)
							(if (WriteAssignment (car itm) pre row sid)
								(progn
									(setq
										cnt (1+ cnt)
										ext (if (= "" sid) ext (1+ ext))
										stats
											(if (setq itm2 (assoc pre stats))
												(subst (cons pre (1+ (cdr itm2))) itm2 stats)
												(cons (cons pre 1) stats)
											)
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
			)
			(DebugStep "clearing ignored and unsupported blocks")
			(foreach ent (append (cadr col) (caddr col))
				(if (ClearAssignment ent)
					(setq clr (1+ clr))
					(setq fal (1+ fal))
				)
			) ;; IGNORED AND UNSUPPORTED BLOCKS DROP ANY ASSIGNMENT FROM AN EARLIER RUN
			(if und
				(progn
					(vla-endundomark doc)
					(setq und nil)
				)
			)
			
			;; SAVE BLOCK ASSIGNMENTS AND EDGE SETTINGS
			(DebugStep "saving PPP-ASSIGN.json")
			(setq svm (SaveAssignFile afn nmp))
			(DebugStep "saving settings and reporting")
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
					(if (> una 0) (strcat "\n  Not assigned (no tracker chosen): " (itoa una)) "")
					(if (cadr col) (strcat "\n  Ignored (outside contours, flagged by PPPSetup): " (itoa (length (cadr col)))) "")
					(if (caddr col) (strcat "\n  Skipped (rotated, scaled, or empty block): " (itoa (length (caddr col)))) "")
					(if (> fal 0) (strcat "\n  Warning: " (itoa fal) " blocks could not be updated, check for locked layers.") "")
					(if svm (strcat "\n  Block assignments saved to: " afn) (strcat "\n  Warning: Block assignments could not be saved to " afn))
					(if svc "" "\n  Warning: Edge settings could not be saved to the project settings file.")
				)
			)
			(setq out
				(list
					(cons "cfg" cfg)
					(cons "assigned" cnt)
					(cons "exterior" ext)
					(cons "blocks" (length (car col)))
				)
			)
		)
	)
	out
)
;; RETURNS ( ("cfg" . SETTINGS) ("assigned" . N) ("exterior" . N) ("blocks" . N) ) IF BLOCKS WERE ASSIGNED, ELSE NIL
;; [ ppp cfg prs ] == PPP-BOM ROOT, PROJECT SETTINGS, AND PRESET LIST FROM PPPSETUP, OR NIL NIL NIL TO READ THEM FROM THE PPP-BOM ROOT
;; THE RETURNED SETTINGS INCLUDE ANY EDGE SETTING CHANGES MADE IN THE DIALOG, PPPSETUP SAVES THEM WITH THE REST OF ITS SETTINGS



;;; REFERENCES ;;;
; (progn
  ; (setq lst '())
  ; (vlax-map-collection mdl '(lambda ( x ) (setq lst (cons x lst))))
  ; (setq count (length (vl-remove-if-not '(lambda ( x ) (= (vla-get-objectname x) "AcDbPoint")) lst)))
; )


;; V-TOPO-MAJR, V-TOPO-MINR - LAYERS FOR CONTOUR LINES (DEFAULT IN SETTINGS DIALOG, EDITABLE PER PROJECT)


;; BOOKMARK
;; ADD 'PPPUPDATESURFACE' AND 'PPPUPDATEARRAYLAYOUT' FOR UPDATED CONTOURS AND UPDATED X-ARRAY FILES ???
;; ADD SEPARATE FUNCTIONS FOR BOM DATA AND PILE COORDINATIONS


; (Load:DescriptionLog "PPPSETUP" "Sets up pile plot plan drawing from X-ARRAY and civil contour files")
; (Load:DescriptionLog "PPPASSIGN" "Assigns array blocks to tracker presets and interior / exterior row types")
