;; SHARED 'PPP:' FUNCTIONS FOR THE PILE PLOT PLAN (PPP) / BOM COMMANDS (PPPSETUP, PPPASSIGN, PPPPILES ...)
;; NOT A GENERIC LIBRARY: GENERIC FUNCTIONS GO IN THE LIB- FILES, FUNCTIONS THAT KNOW ABOUT PPP PRESETS / PROJECTS GO HERE
;; THE SETTINGS / TRACKER PRESETS DIALOG (PPP:SettingsDialog) IS HERE BECAUSE PPPSETUP AND PPPSETTINGS BOTH SHOW IT
;; MUST BE LOADED BEFORE ANY PPP COMMAND RUNS (ADD TO THE AUTOLOADER WITH THE LIB- FILES), THE COMMANDS CHECK FOR IT
;; FILE MAKES OUTSIDE CALLS TO LIB-JSON_Functions.lsp (PRESETS), LIB-WSH_Functions.lsp (FOLDER PICKER), SEE PPP:RequiredFunctions
;; FILE MAKES OPTIONAL CALLS TO LIB-GEO_Functions.lsp (PPP:ProjectContext LOADS THE GEOLOCATION ONLY IF IT IS LOADED)
;; THIS FILE IS INTENTIONALLY PURE ASCII: AUTOCAD'S LISP READER ABORTS SILENTLY ON NON-ASCII BYTES
;;
;; FILE LAYOUT: LOADING AND ERRORS, TRACKER PRESETS, PROJECT FILES, PROJECT CONTEXT, SETTINGS AND DEBUG, ARRAY BLOCKS, HEIGHTS AND TRACKER PLACEMENT, DIALOGS (INCLUDING THE SETTINGS DIALOG)
;; PPP:FillList IS THE LAST FUNCTION IN THE FILE, PPPUtilitiesLoaded CHECKS FOR IT SO A LOAD THAT STOPPED PART WAY IS CAUGHT


;;; PPP SHARED FUNCTIONS - LOADING AND ERRORS ;;;

(defun PPP:RequiredFunctions ( / )
	(list
		'json:StringSplit 'json:StringJoin 'json:ReadFile 'json:WriteFile 'json:Object 'json:Array 'json:Items 'json:Get 'json:Set
		'json:Has 'json:GetPath 'json:True-p 'json:LastError ;; PPP:SettingsDialog (PRESET IMPORT)
		'wsh:ReturnFolder 'wsh:ConfirmationDialog
		'std:FindSubDirectory ;; PPP:FindCivilOutput
		'std:AssocIncrement ;; PPP:ScanArrayBlocks
		'std:AssocMerge 'std:CountList 'std:NumberString 'std:ParseLength 'std:ParseNumberList 'std:SlopeToDegrees 'std:ValidFileName ;; PPP:SettingsDialog
		'std:ReplaceNth 'std:RemoveNth 'std:InsertNth ;; PPP:SettingsDialog (PRESET AND LAYOUT LISTS)
		'std:TranslateObject 'std:RotateAboutX ;; PPP:PlaceTracker, PPP:FlattenTracker
		'std:CreateDirectoryPath 'std:IsFileLocked ;; PPP:WriteBom (msxl:ExportTable IS OPTIONAL, IT REPORTS A MISSING LIB-MSXL)
		'std:ToNumber 'std:FunctionLoaded 'std:AssocSet ;; SETTINGS, DIALOG TILES, OPTIONAL LIBRARIES
		'std:FileTimeStamp 'std:ReturnCollectionItem 'std:PolylineElevation ;; PPP:SourceStamp, PPP:TopoXref, PPP:ContourObjects
	)
)
;; RETURNS THE LIBRARY FUNCTIONS THIS FILE CALLS, CHECKED BY PPPUtilitiesLoaded BEFORE ANY PPP COMMAND STARTS
;; LIB-GEO IS OPTIONAL AND NOT LISTED, KEEP THIS LIST IN STEP WHEN A 'PPP:' FUNCTION STARTS USING ANOTHER LIBRARY FUNCTION

(defun PPP:MissingFunctions ( lst / mis )
	(setq mis (vl-remove-if '(lambda ( fn ) (and (boundp fn) (eval fn))) lst))
	(if mis
		(prompt
			(strcat
				"\nError: Required library functions are not loaded, check the support path / autoloader: "
				(apply 'strcat (mapcar '(lambda ( fn ) (strcat (vl-symbol-name fn) " ")) mis))
			)
		)
	)
	mis
)
;; CHECKS THAT EVERY FUNCTION SYMBOL IN [ lst ] IS DEFINED, PRINTS THE MISSING NAMES
;; RETURNS THE MISSING SYMBOLS, NIL IF ALL ARE LOADED, SO A COMMAND CAN STOP WITH ((PPP:MissingFunctions funcs) nil)
;; DOES NOT USE ANY LIBRARY FUNCTION, IT HAS TO WORK WHEN A LIBRARY IS MISSING

(defun PPP:ErrorReport ( msg cxm / cxl )
	(setq cxl (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*")) ;; ESC OR (EXIT), NOT A PROGRAM ERROR
	(if (not cxl)
		(vl-bt)
	) ;; FOR DEBUGGING
	(if (and (boundp 'ECHO:Log-Verbose) ECHO:Log-Verbose (boundp 'ECHO:Log-Flush) ECHO:Log-Flush)
		(progn
			(ECHO:Log-Verbose (if cxl "info" "ERROR") (if cxl "cancelled by user" msg) nil (list (cons "CMDNAME" *ECHO:Log-Command*)))
			(ECHO:Log-Flush)
		)
	) ;; ONLY WHEN THE ECHO LOGGER IS LOADED, AN ERROR INSIDE *error* WOULD HIDE THE REAL ERROR
	(princ (if cxl cxm (strcat "\nError: " msg)))
	(princ)
)
;; SHARED END OF EVERY PPP COMMAND'S *error*: BACKTRACE, ECHO LOG, AND THE MESSAGE, A CANCEL IS LOGGED AS INFO AND SHOWN AS [ cxm ]
;; EACH COMMAND'S OWN *error* DOES ITS CLEANUP FIRST (FILES, DIALOGS, UNDO, UNITS), THEN CALLS THIS
;; [ msg ] == ERROR MESSAGE PASSED TO *error*, [ cxm ] == MESSAGE SHOWN WHEN THE USER CANCELLED (EX. "\nPPP assign cancelled.")


;;; PPP SHARED FUNCTIONS - TRACKER PRESETS ;;;
;; FUNCTIONS PREFIXED WITH 'PPP:' ARE GLOBAL SO EVERY PPP COMMAND (EX. PPPSETUP, PPPASSIGN, PPPPILES) CAN USE THEM
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
	0.005
)
;; RETURNS THE LENGTH TOLERANCE (FT) FOR PRESET GEOMETRY, ABOUT 1/16 INCH
;; USED TO MATCH CALCULATED PILE SPACING AND TO FLAG A MOTOR PILE THAT IS NOT CENTERED IN THE MOTOR GAP

(defun PPP:ParsePacks ( str / typ num itm lst bad )
	(if (= 'STR (type str))
		(foreach pck (json:StringSplit str ";")
			(if (/= "" (vl-string-trim " \t" pck))
				(progn
					(setq
						itm (json:StringSplit pck ",")
						typ (cdr (assoc (strcase (vl-string-trim " \t" (car itm))) (list (cons "MODULES" "Modules") (cons "JOINTGAP" "JointGap") (cons "MOTOR" "Motor"))))
						num (if (= 2 (length itm)) (std:ToNumber (cadr itm)))
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

(defun PPP:PackSpans ( pck siz gap jnt mtr / pos prv len out )
	(setq pos 0.0)
	(foreach itm pck
		(if (and (= "Modules" (car itm)) (= "Modules" prv))
			(setq pos (+ pos gap))
		) ;; BACK TO BACK MODULE PACKS (EX. "Modules,3; Modules,4") ARE ONE MODULE GAP APART, MATCHES PVCASE (ORCA FALLS FRAME_PRESETS, 10/09)
		(setq
			len
				(cond
					((= "Modules" (car itm)) (+ (* (cdr itm) siz) (* (1- (cdr itm)) gap)))
					((= "JointGap" (car itm)) (* (cdr itm) jnt))
					( t (* (cdr itm) mtr))
				)
			out (cons (list (car itm) (cdr itm) pos (+ pos len)) out)
			pos (+ pos len)
			prv (car itm)
		)
	)
	(reverse out)
)
;; RETURNS ONE ( TYPE COUNT START END ) SPAN PER PACK, START / END IN FEET FROM THE NORTH EDGE OF THE MODULES
;; EX. (("Modules" 6 0.0 13.9) ("Motor" 1 13.9 19.8) ("Modules" 6 19.8 33.7))
;; THE ONE PLACE THE MODULE LAYOUT IS WALKED: PPP:TrackerLayout, THE PRESETS DIALOG (PILES AT EVERY JOINT), AND THE PPP BLOCK DRAWING USE IT
;; [ pck ] == PACKS (SEE PPP:ParsePacks), [ siz gap jnt mtr ] == MODULE WIDTH (N-S), MODULE GAP, JOINT GAP, MOTOR GAP (ALL FT)

(defun PPP:TrackerLayout ( pre / pck val spn mot )
	(if (and
			(setq pck (PPP:ParsePacks (json:Get pre "Packs")))
			(vl-every 'numberp (setq val (mapcar '(lambda ( key ) (json:Get pre key)) (list "SizeY" "ModuleGap" "JointGapSize" "MotorGapSize"))))
		)
		(progn
			(setq
				spn (apply 'PPP:PackSpans (cons pck val))
				mot (assoc "Motor" spn)
			)
			(list
				(last (last spn)) ;; END OF THE LAST SPAN
				(if mot (+ (caddr mot) (* 0.5 (last val)))) ;; CENTER OF THE MOTOR GAP
			)
		)
	)
)
;; RETURNS ( LENGTH MOTOR ) FOR A PRESET, BOTH IN FEET FROM THE NORTH EDGE OF THE MODULES
;; LENGTH == MODULES + MODULE GAPS (INCLUDING BETWEEN BACK TO BACK PACKS) + JOINT GAPS + MOTOR GAP, MOTOR == CENTER OF THE MOTOR GAP (NIL IF THE TRACKER HAS NO MOTOR)
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
;; RETURNS PILE OFFSETS (FT) FROM THE NORTH EDGE OF THE MODULES, PILE 1 FIRST (NEGATIVE == NORTH OF THE MODULES)
;; HORIZONTAL DISTANCES START AT THE NORTH END OF THE TORQUE TUBE, THE NORTH OVERHANG IS TORQUE TUBE END TO MODULES,
;; SO PILE 1 SITS AT ( FIRST DISTANCE - NORTH OVERHANG ) FROM THE MODULES

(defun PPP:PileSpacing ( pre / hd )
	(setq hd (json:Items (json:Get pre "HorizontalDistances")))
	(if (and (= 'LIST (type hd)) (cddr hd) (vl-every 'numberp hd))
		(reverse (cdr (reverse (cdr hd))))
	)
)
;; RETURNS THE SPACING BETWEEN PILES (FT) NORTH TO SOUTH, WITHOUT THE PVCASE END DISTANCES
;; RETURNS NIL IF THE PRESET HAS FEWER THAN TWO PILES OR A DISTANCE IS NOT A NUMBER

(defun PPP:CleanLength ( num / )
	(if (and (numberp num) (equal num 0.0 1e-9))
		0.0
		num
	)
)
;; RETURNS 0.0 FOR A LENGTH THAT IS ZERO EXCEPT FOR ROUNDING NOISE (EX. -1.4E-14), ELSE THE LENGTH

(defun PPP:TubeLength ( pre / hd )
	(setq hd (json:Items (json:Get pre "HorizontalDistances")))
	(if (and (= 'LIST (type hd)) (cddr hd) (vl-every 'numberp hd))
		(apply '+ hd)
	)
)
;; RETURNS THE TORQUE TUBE LENGTH (FT): NORTH END TO PILE 1 + SPACING BETWEEN PILES + LAST PILE TO SOUTH END
;; "TRACKER LENGTH" IN PVCASE IS THE TORQUE TUBE LENGTH, RETURNS NIL IF THE PILE SPACING IS NOT VALID

(defun PPP:TubeOverhangs ( pre / ttl lay ovn )
	(if (and
			(setq ttl (PPP:TubeLength pre))
			(setq lay (PPP:TrackerLayout pre))
			(numberp (setq ovn (json:Get pre "PilingOverhangLeftSize")))
		)
		(list ovn (PPP:CleanLength (- ttl (car lay) ovn)))
	)
)
;; RETURNS ( NORTH SOUTH ) OVERHANGS (FT): TORQUE TUBE END TO THE FIRST / LAST MODULE
;; THE NORTH OVERHANG IS ENTERED, THE SOUTH OVERHANG IS THE DIFFERENCE: TORQUE TUBE LENGTH - MODULES LENGTH - NORTH OVERHANG
;; MODULES NEVER EXTEND PAST THE TORQUE TUBE, SO NEITHER OVERHANG SHOULD BE NEGATIVE (SEE PPP:PresetProblem)
;; THE PILE SPACING NEVER CHANGES WITH THE OVERHANG, THE OVERHANG ONLY PLACES THE MODULES ON THE TORQUE TUBE
;; RETURNS NIL IF THE MODULE LAYOUT, PILE SPACING, OR NORTH OVERHANG IS NOT VALID

(defun PPP:PresetNormalize ( pre / ovh )
	(if (setq ovh (PPP:TubeOverhangs pre))
		(json:Set pre "PilingOverhangRightSize" (cadr ovh))
		pre
	)
)
;; RETURNS THE PRESET WITH ITS SOUTH OVERHANG ('PilingOverhangRightSize') RECALCULATED AS THE DIFFERENCE (SEE PPP:TubeOverhangs)
;; THE PILE SPACING AND NORTH OVERHANG DO NOT CHANGE, SO A PRESET CAN NEVER DISAGREE WITH ITSELF
;; RETURNS THE PRESET UNCHANGED IF ITS MODULE LAYOUT, PILE SPACING, OR NORTH OVERHANG IS NOT VALID

(defun PPP:MotorPileNearest ( pre / lay pos idx best )
	(if (and (setq lay (PPP:TrackerLayout pre)) (cadr lay) (setq pos (PPP:PilePositions pre)))
		(progn
			(setq idx 0)
			(foreach pnt pos
				(setq idx (1+ idx))
				(if (or (null best) (< (abs (- pnt (cadr lay))) (abs (cdr best))))
					(setq best (cons idx (- pnt (cadr lay))))
				)
			)
			best
		)
	)
)
;; RETURNS ( INDEX . OFFSET ) FOR THE PILE NEAREST THE MOTOR GAP CENTER, OFFSET (FT) IS POSITIVE WHEN THE PILE IS SOUTH OF CENTER
;; RETURNS NIL IF THE TRACKER HAS NO MOTOR OR THE PILE SPACING IS NOT VALID

(defun PPP:MotorPileIndex ( pre / best )
	(if (and
			(setq best (PPP:MotorPileNearest pre))
			(<= (abs (cdr best)) (max (* 0.5 (json:Get pre "MotorGapSize")) (PPP:PresetTolerance)))
		)
		(car best)
	)
)
;; RETURNS THE 1-BASED INDEX OF THE MOTOR (GEAR RACK) PILE, THE PILE NEAREST THE MOTOR GAP CENTER IF IT SITS WITHIN THE MOTOR GAP
;; MATCHES 'Gear Rack Num' IN THE PVCASE BOM, RETURNS NIL IF THE TRACKER HAS NO MOTOR OR NO PILE SITS IN THE MOTOR GAP
;; A MOTOR PILE OFF CENTER IS ALLOWED, PPP:PresetWarnings REPORTS IT

(defun PPP:MotorPileOffset ( pre / )
	(if (PPP:MotorPileIndex pre)
		(cdr (PPP:MotorPileNearest pre))
	)
)
;; RETURNS HOW FAR (FT) THE MOTOR PILE IS FROM THE MOTOR GAP CENTER, POSITIVE == SOUTH OF CENTER, NIL IF THERE IS NO MOTOR PILE

(defun PPP:RowPrefix ( nam / pfx )
	(if (and
			(= 'STR (type nam))
			(member (setq pfx (strcase (substr nam 1 3))) (list "INT" "EXT"))
			(/= "" (PPP:TrackerName nam))
		)
		pfx
	)
)
;; RETURNS "INT" OR "EXT" FROM THE START OF A PRESET NAME (NOT CASE-SENSITIVE), NIL IF THE NAME HAS NEITHER OR NOTHING AFTER IT
;; THE HYPHEN IS OPTIONAL: ORCA FALLS (PVCASE) USES "INT-28M56", MCCRACKEN USES "INT90-45M45", BOTH ARE INTERIOR
;; THE PREFIX SETS THE BOM ROW TYPE

(defun PPP:TrackerName ( nam / )
	(if (= 'STR (type nam))
		(vl-string-left-trim "-" (substr nam 4)) ;; DROPS THE PREFIX AND THE HYPHEN AFTER IT, IF ANY
	)
)
;; RETURNS THE PRESET NAME WITHOUT ITS INT / EXT PREFIX (EX. "INT-28M56" -> "28M56", "INT90-45M45" -> "90-45M45")
;; ONLY MEANINGFUL WHEN PPP:RowPrefix FINDS A PREFIX, BOTH PRESETS OF A PAIR GIVE THE SAME TRACKER NAME

(defun PPP:PairName ( nam / pfx )
	(if (setq pfx (PPP:RowPrefix nam))
		(strcat (if (= "INT" pfx) "EXT" "INT") (substr nam 4))
	)
)
;; RETURNS THE MATCHING INTERIOR / EXTERIOR PRESET NAME, ONLY THE PREFIX CHANGES (EX. "INT-28M56" -> "EXT-28M56", "INT90-45M45" -> "EXT90-45M45")
;; RETURNS NIL IF THE NAME HAS NO INT / EXT PREFIX

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

(defun PPP:PresetProblem ( pre / nam num pck mdl spc hd ovh )
	(setq
		nam (json:Get pre "Name")
		pck (PPP:ParsePacks (json:Get pre "Packs"))
		mdl (apply '+ (mapcar '(lambda ( itm ) (if (= "Modules" (car itm)) (cdr itm) 0)) pck))
	)
	(cond
		((null (PPP:RowPrefix nam))
			"Name must start with INT or EXT (sets the BOM row type)."
		)
		((not (and (numberp (setq num (json:Get pre "Columns"))) (>= num 1) (equal num (fix num) 1e-8)))
			"Modules per tracker must be a whole number of 1 or more."
		)
		((not (vl-every '(lambda ( key / val ) (and (numberp (setq val (json:Get pre key))) (> val 0))) (list "SizeX" "SizeY")))
			"Module length and width must be greater than zero."
		)
		((not (vl-every '(lambda ( key / val ) (and (numberp (setq val (json:Get pre key))) (>= val 0))) (list "ModuleGap" "JointGapSize" "MotorGapSize")))
			"Module, joint, and motor gaps must be numbers of zero or greater."
		)
		((not (and (numberp (setq num (json:Get pre "PilingOverhangLeftSize"))) (>= num 0)))
			"North overhang must be zero or greater."
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
		((null (setq spc (PPP:PileSpacing pre)))
			"Pile spacing needs a north end, one or more spacings, and a south end."
		)
		((not (vl-every '(lambda ( dst ) (> dst 0)) spc))
			"Spacing between piles must be greater than zero."
		)
		((not (vl-every '(lambda ( dst ) (>= dst 0)) (list (car (setq hd (json:Items (json:Get pre "HorizontalDistances")))) (last hd))))
			"Torque tube end distances must be zero or greater."
		)
		((minusp (cadr (setq ovh (PPP:TubeOverhangs pre))))
			(strcat
				"Modules run " (rtos (- (cadr ovh)) 2 3) " ft past the south end of the torque tube, lower the north overhang."
			)
		)
		((and (cadr (PPP:TrackerLayout pre)) (null (PPP:MotorPileIndex pre)))
			(strcat
				"No pile in the motor gap, nearest is " (rtos (abs (cdr (PPP:MotorPileNearest pre))) 2 3)
				" ft from its center. Check spacing or overhang."
			)
		)
	)
)
;; CHECKS A PRESET FOR MISSING OR IMPOSSIBLE VALUES, RETURNS A MESSAGE DESCRIBING THE FIRST PROBLEM FOUND, NIL IF THE PRESET IS VALID
;; THE SOUTH OVERHANG IS NOT CHECKED AGAINST A TOTAL, IT IS ALWAYS THE DIFFERENCE (SEE PPP:TubeOverhangs)
;; MODULES NEVER EXTEND PAST THE TORQUE TUBE, SO A NEGATIVE CALCULATED SOUTH OVERHANG IS AN ERROR (A PILE MAY STILL SIT OUTSIDE THE MODULES)
;; A MOTOR TRACKER NEEDS A PILE IN THE MOTOR GAP, IT DOES NOT HAVE TO BE CENTERED (SEE PPP:PresetWarnings)

(defun PPP:PresetWarnings ( pre / ovh spc lim off out )
	(if (and (setq ovh (PPP:TubeOverhangs pre)) (setq spc (PPP:PileSpacing pre)))
		(progn
			(setq lim (* 0.5 (apply 'max spc)))
			(mapcar
				'(lambda ( sde val )
					(if (> val lim)
						(setq out
							(cons
								(strcat
									sde " overhang " (rtos val 2 3) " ft"
									" is more than half the longest pile spacing, check the overhang and pile spacing."
								)
								out
							)
						)
					)
				)
				(list "North" "South")
				ovh
			)
			(if (and (setq off (PPP:MotorPileOffset pre)) (> (abs off) (PPP:PresetTolerance)))
				(setq out
					(cons
						(strcat
							"Motor pile " (itoa (PPP:MotorPileIndex pre)) " is " (rtos (abs off) 2 4) " ft "
							(if (minusp off) "north" "south") " of the motor gap center."
						)
						out
					)
				)
			)
			(reverse out)
		)
	)
)
;; RETURNS MESSAGES FOR VALUES THAT ARE ALLOWED BUT UNUSUAL, NIL IF NONE (WARNINGS DO NOT BLOCK SAVING)
;; AN OVERHANG OVER HALF THE LONGEST PILE SPACING USUALLY MEANS A WRONG OVERHANG OR A SPACING MISSED OR ENTERED TWICE
;; A MOTOR PILE MORE THAN PPP:PresetTolerance FROM THE MOTOR GAP CENTER IS REPORTED, IT IS STILL THE GEAR RACK PILE

(defun PPP:PresetGeometryEqual ( pre par / )
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
;; BOTH PRESETS OF AN INT / EXT PAIR DESCRIBE THE SAME TRACKER, ONLY THE NAME (BOM ROW TYPE) AND DESCRIPTION MAY DIFFER
;; USED BY THE PRESETS DIALOG (INT / EXT PAIR CHECK) AND PPPSETTINGS (PRESETS IN USE WITH NEW GEOMETRY)

(defun PPP:LoadPresets ( pth / dat arr )
	(if (and
			(findfile pth)
			(setq dat (json:ReadFile pth))
			(listp (setq arr (json:Get dat "Presets")))
			(eq 'ARRAY (car arr))
			(vl-every '(lambda ( pre ) (and (listp pre) (eq 'OBJECT (car pre)))) (json:Items arr))
		)
		(json:Array (mapcar 'PPP:PresetNormalize (json:Items arr)))
	)
)
;; READS TRACKER PRESETS FROM A PPP PRESET FILE, EACH PRESET IS NORMALIZED (SEE PPP:PresetNormalize, PILE POSITIONS DO NOT CHANGE)
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

(defun PPP:GeoFile ( / )
	"PPP-GEOLOCATION.txt"
)
;; RETURNS THE GEOLOCATION FILE NAME, SAVED IN THE PPP-BOM ROOT (SEE geo:SaveGeoFile IN LIB-GEO_Functions.lsp)

(defun PPP:DefaultSettings ( dir / ParseProjectFolder prj )
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
	
	(setq prj (if (= 'STR (type dir)) (ParseProjectFolder dir) (list "" "")))
	(list
		(cons "project_number" (car prj))
		(cons "project_name" (cadr prj))
		(cons "milestone" "30%")
		(cons "file_name" (strcat (car prj) " - PPP SETUP"))
		(cons "units" "Feet")
		(cons "contour_layers" "V-TOPO-MAJR,V-TOPO-MINR")
		(cons "array_layers" "E-INV*")
		(cons "array_blocks" "*PVBlock") ;; OPTIONAL NAME PATTERN, PRE-SELECTS BLOCKS IN THE ARRAY BLOCKS DIALOG, THE PICKED NAMES ARE SAVED AS "array_block_names" (SEE PPP:IsArrayName)
		(cons "manufacturer" "Nextracker")
		(cons "exterior_rows" "2")
		(cons "stagger_allowance" "10.0") ;; END OFFSET (FT) ALLOWED BETWEEN NEIGHBORING ARRAYS BEFORE THEY ARE AN EDGE, TEAM DEFAULT (STAGGERED / DIAGONAL LAYOUTS, MCCRACKEN IS STAGGERED MORE THAN 8 FT)
		(cons "table_slope" "8.0")
		(cons "cott_buckets" "6.0,6.5,7.0")
		(cons "bucket_step" "0.5") ;; FT, PROJECT SPECIFIC, THE COTT MINIMUM IS THE LOWEST BUCKET MINUS THIS STEP
		(cons "tolerance" "0.04")
		(cons "allowed_flood" "0.5")
		(cons "edge_clearance" "2.5") ;; FT, LOWEST MODULE EDGE AT MAX TILT ABOVE THE FLOOD SURFACE + FREEBOARD (YELLOW BANKS 2.5)
		(cons "flood_freeboard" "0.0") ;; FT ADDED TO THE FLOOD DEPTH
		(cons "cott_reveal_offset" "0.75") ;; FT, COTT - PILE REVEAL ON ARRAY (BEARING) PILES, NEXTRACKER 0.75
		(cons "cott_reveal_offset_motor" "0.75") ;; FT, COTT - PILE REVEAL ON THE MOTOR (GEAR RACK) PILE
		(cons "anchor_pile" "Motor") ;; PILE THAT STAYS FIXED WHEN A TRACKER TILTS: Motor, North OR South (TRACKERS WITHOUT A MOTOR USE North)
		(cons "decimals" "3")
		(cons "eor_confirmed" "0")
	)
)
;; RETURNS DEFAULT PROJECT SETTINGS AS AN ASSOCIATION LIST OF ( "KEY" . "VALUE" ) STRING PAIRS
;; KEYS MATCH THE SETTINGS DIALOG TILE KEYS, SO THE ORDER OF THIS LIST DEFINES WHICH TILES ARE LOADED AND SAVED (SEE PPP:SettingsDialog)
;; [ dir ] == PROJECT ROOT FOLDER, THE PROJECT NUMBER AND NAME COME FROM ITS NAME, NIL LEAVES THEM BLANK

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



;;; PPP SHARED FUNCTIONS - PROJECT CONTEXT ;;;
;; EVERY PPP STAGE (PPPASSIGN NOW, PILE PLACEMENT / TERRAIN / BOM LATER) TAKES ONE ARGUMENT, THE PROJECT CONTEXT
;; THE CONTEXT IS AN ASSOCIATION LIST, EACH STAGE READS WHAT IT NEEDS AND RETURNS THE CONTEXT WITH ITS RESULTS ADDED:
;;	("ppp" . PATH)			PPP-BOM ROOT
;;	("cfg" . SETTINGS)		PROJECT SETTINGS, ( "KEY" . "VALUE" ) PAIRS
;;	("presets" . LIST)		TRACKER PRESETS (JSON OBJECTS)
;;	("geo" . GEOLOCATION)	RESULT OF geo:ReadGeoData / geo:LoadGeoFile, NIL IF THE PROJECT IS NOT GEOLOCATED
;;	("assign" . STATS)		ADDED BY PPPASSIGN, ( ("assigned" . N) ("exterior" . N) ("blocks" . N) )
;;	("trackers" . LIST)		ADDED BY PPPASSIGN, ( ENAME BLOCK-NAME X SOUTH NORTH WIDTH PRESET ROW SIDES ) PER ASSIGNED BLOCK, WEST TO EAST
;; PPPSETUP BUILDS THE CONTEXT AND PASSES IT DOWN THE CHAIN IN ONE RUN, NOTHING IS KEPT IN MEMORY BETWEEN COMMANDS
;; A STAGE RUN AS ITS OWN COMMAND GETS NIL AND BUILDS THE CONTEXT WITH PPP:ProjectContext, DRAWING DATA (EX. TRACKERS) IS READ FROM THE DRAWING
;; ADDING A STAGE ADDS A KEY, NO EXISTING FUNCTION SIGNATURE OR CALL CHANGES

(defun PPP:ProjectContext ( / dir cfg prs geo )
	(setq dir (vl-filename-directory (vl-string-right-trim "\\" (getvar "DWGPREFIX"))))
	(cond
		((= 0 (getvar "DWGTITLED"))
			(prompt "\nThis command must be run in a saved PPP setup drawing, run PPPSetup first.")
			nil
		)
		((not
				(or
					(findfile (strcat dir "\\" (PPP:SettingsFile)))
					(and
						(setq dir (wsh:ReturnFolder "Select PPP-BOM Folder Location" 512 dir))
						(findfile (strcat (setq dir (vl-string-right-trim "\\" dir)) "\\" (PPP:SettingsFile)))
					)
				)
			) ;; ONE LEVEL ABOVE THE DRAWING, OR A FOLDER SELECTED BY THE USER, !REVISIT! THE PROJECT FILE LOCATION MAY CHANGE WITH THE TEAM PROCESS
			(prompt (strcat "\nError: " (PPP:SettingsFile) " was not found in the PPP-BOM folder, run PPPSetup first."))
			nil
		)
		((null (setq cfg (PPP:LoadSettings (strcat dir "\\" (PPP:SettingsFile)))))
			(prompt (strcat "\nError: Project settings could not be read from " dir "\\" (PPP:SettingsFile)))
			nil
		)
		((null (setq prs (PPP:LoadPresets (strcat dir "\\" (PPP:PresetsFile)))))
			(prompt (strcat "\nError: Tracker presets could not be read from " dir "\\" (PPP:PresetsFile)))
			nil
		)
		( t
			(if (std:FunctionLoaded 'geo:LoadGeoFile)
				(setq geo (geo:LoadGeoFile (strcat dir "\\" (PPP:GeoFile))))
			) ;; OPTIONAL, NIL IF THE FILE IS MISSING OR LIB-GEO IS NOT LOADED, ONLY STAGES THAT NEED LAT / LONG CHECK IT
			(list
				(cons "ppp" dir)
				(cons "cfg" cfg)
				(cons "presets" (json:Items prs)) ;; THE FILE RETURNS THE TAGGED ARRAY, STAGES USE THE PLAIN LIST
				(cons "geo" geo)
			)
		)
	)
)
;; BUILDS THE PROJECT CONTEXT FOR A STAGE RUN AS ITS OWN COMMAND, READING THE PROJECT FILES FROM THE PPP-BOM ROOT
;; PRINTS THE REASON AND RETURNS NIL IF THE DRAWING IS NOT SAVED OR A PROJECT FILE IS MISSING OR UNREADABLE


;;; PPP SHARED FUNCTIONS - SETTINGS AND DEBUG ;;;

(defun PPP:SettingNumber ( cfg key def / num )
	(if (and (setq num (std:ToNumber (cdr (assoc key cfg)))) (>= num 0))
		num
		def
	)
)
;; RETURNS A NUMERIC PROJECT SETTING OF ZERO OR GREATER, SAVED AS A NUMBER (EX. "cott_min") OR AS TEXT, [ def ] IF THE SETTING IS MISSING OR NOT VALID
;; [ cfg ] == PROJECT SETTINGS, ( "KEY" . "VALUE" ) PAIRS
;; [ key ] == SETTING NAME (EX. "stagger_allowance")
;; [ def ] == DEFAULT VALUE

(defun PPP:SettingText ( cfg key def / val )
	(if (and (= 'STR (type (setq val (cdr (assoc key cfg))))) (/= "" (vl-string-trim " " val)))
		val
		def
	)
)
;; RETURNS A TEXT PROJECT SETTING, [ def ] IF THE SETTING IS MISSING OR BLANK
;; [ cfg ] == PROJECT SETTINGS, ( "KEY" . "VALUE" ) PAIRS
;; [ key ] == SETTING NAME (EX. "area_label_layer")
;; [ def ] == DEFAULT TEXT
;; !REVISIT! THE AREA LABEL AND PILE LABEL SETTINGS ARE NOT IN THE SETTINGS DIALOG YET, THEY CAN BE ADDED TO PPP-SETTINGS.cfg BY HAND

(defun PPP:DebugStep ( cmd msg / )
	(if (and (boundp '*PPP:Trace*) *PPP:Trace*)
		(progn
			(prompt (strcat "\n[" cmd " " (itoa (getvar "MILLISECS")) "] " msg))
			(princ) ;; PRINT NOW, SO THE LAST STEP IS VISIBLE IF AUTOCAD STOPS
		)
	)
)
;; DEBUG STEP MESSAGES, ONLY PRINTED AFTER PPPDEBUGON (OR (setq *PPP:Trace* T) AT THE COMMAND LINE), PPPDEBUGOFF TURNS THEM OFF
;; [ cmd ] == COMMAND NAME SHOWN WITH THE MESSAGE (EX. "PPPASSIGN")
;; [ msg ] == STEP MESSAGE



;;; PPP SHARED FUNCTIONS - CIVIL GRADING OUTPUT ;;;

(defun PPP:CivilHeaders ( / )
	(list
		"X (ft)" "Y" "Z Terrain Enter (ft)" "Z Frame attach (ft)" "COTT (ft)" "Pile Height (ft)" "Table Slope (deg)" ;; 0 - 6
		"Z terrain - original" "Y Adjusted" "BlockHandle" ;; 7 - 9
	)
)
;; RETURNS THE CIVIL SLOPE TOOL COLUMNS PPP READS, IN THE ORDER PPPPILES USES THEM (SEE THE "Structural" SHEET OF THE AREA FILES)
;; REQUIRED: X, Y (OR "Y Adjusted" WHEN "Y" IS MISSING), Z TERRAIN ENTER, Z FRAME ATTACH, COTT, PILE HEIGHT
;; "Table Slope (deg)" IS BLANK ON THE LAST PILE OF EVERY TRACKER, IT IS FILLED FROM THE OTHER PILES WITH THE SAME "BlockHandle" (ONE SLOPE PER TRACKER)
;; MATCHING USES "Y": CHECKED ON MCCRACKEN, THE DELIVERED BOM SITS WITHIN 0.09 FT OF CIVIL'S "Y" BUT UP TO 0.46 FT FROM "Y Adjusted"
;; CIVIL'S "Z Frame attach" IS THE PILE TOP (Z TERRAIN ENTER + PILE HEIGHT), THE BOM'S IS THE TORQUE TUBE CENTER (Z TERRAIN ENTER + COTT)
;; !REVISIT! COLUMN NAMES COME FROM CIVIL'S INTERNAL TOOL (MCCRACKEN 10-2026), CONFIRM WITH CIVIL THAT THEY STAY STABLE

(defun PPP:FindCivilOutput ( ppp / eng cvl hit )
	(setq eng (vl-filename-directory (vl-filename-directory (vl-filename-directory (vl-string-right-trim "\\" ppp)))))
	;; PPP-BOM -> ENGINEERING -> 05 STRUCTURAL -> 02 ENGINEERING
	(cond
		((setq hit (std:FindSubDirectory eng "01 CIVIL\\CAD\\Data Shortcuts\\PV Grading References\\Slope Tool Outputs" 0))
			hit
		)
		((and (vl-file-directory-p (setq cvl (strcat eng "\\01 CIVIL"))) (setq hit (std:FindSubDirectory cvl "Slope Tool Outputs" 5)))
			hit
		) ;; SAME FOLDER NAME ANYWHERE UNDER 01 CIVIL
	)
)
;; FINDS THE CIVIL SLOPE TOOL OUTPUT FOLDER FROM THE PPP-BOM ROOT, NIL IF IT IS NOT FOUND (THE USER CAN SELECT IT IN PPPPILES)
;; KNOWN LOCATION (MCCRACKEN): ...\Project Files\02 ENGINEERING\01 CIVIL\CAD\Data Shortcuts\PV Grading References\Slope Tool Outputs
;; !REVISIT! ONE PROJECT SO FAR, CONFIRM THE FOLDER NAME AND LOCATION WITH CIVIL. FILE NAMES ARE NOT STANDARD, SO FILES ARE CHECKED BY THEIR COLUMNS
;; [ ppp ] == PPP-BOM ROOT

(defun PPP:CivilFiles ( dir pat / out )
	(if (and (= 'STR (type dir)) (vl-file-directory-p (setq dir (vl-string-right-trim "\\" dir))))
		(foreach nam (vl-directory-files dir (cond ((= 'STR (type pat)) pat) ("*.xlsx")) 1)
			(if (not (wcmatch nam "`~$*")) ;; SKIP EXCEL LOCK FILES ("~$Area 1.xlsx"), THE BACKQUOTE MAKES "~" LITERAL, A LEADING "~" IN wcmatch MEANS "NOT"
				(setq out (cons (strcat dir "\\" nam) out))
			)
		)
	)
	(vl-sort out '<)
)
;; RETURNS THE FULL PATHS OF THE CIVIL OUTPUT WORKBOOKS IN A FOLDER (ONE PER AREA, EX. "Area 5 - STR.xlsx"), SORTED BY NAME
;; [ dir ] == FOLDER, [ pat ] == FILE PATTERN, NIL FOR "*.xlsx"



;;; PPP SHARED FUNCTIONS - SOURCE DRAWINGS (CIVIL TOPO XREF, FILE CHANGES) ;;;
;; THE CIVIL TOPO (V-TOPO*.dwg, EXISTING GRADE CONTOURS) IS AN OVERLAY XREF NAMED "PPP-CIVIL-TOPO" AT 0,0,0, SCALE 1, ROTATION 0 (PPPSETUP)
;; ITS CONTOURS ARE READ FROM THE XREF DEFINITION (LAYERS "PPP-CIVIL-TOPO|V-TOPO-MAJR" ...), THE POINTS NEED NO TRANSFORM
;; CHECKED 11-2026 ON MCCRACKEN (PPPXREFTEST): GEOLOCATION MATCH (KY83-SF), 6,325 CONTOURS READ IN 6.8 S WITH THE SAME LIBRARY FUNCTIONS
;; THE XREF KEEPS ITS FULL PATH: EVERY USER HAS THE SAME C:\_ACC\ACCDocs\ANS Team\ ROOT, AND THE DRAWING IS NOT SAVED YET WHEN IT IS ATTACHED
;; THE X-ARRAY IS STILL COPIED IN (PPPASSIGN SWAPS ITS BLOCKS AND WRITES XDATA ON THEM, WHICH AN XREF CANNOT TAKE)
;; BOTH SOURCE FILES ARE STAMPED IN THE SETTINGS AT SETUP ("xarray_file" / "xarray_date", "topo_file" / "topo_date"), PPP:SourceCheck COMPARES THEM

(defun PPP:TopoName ( / )
	"PPP-CIVIL-TOPO"
)
;; RETURNS THE BLOCK NAME OF THE CIVIL TOPO XREF, ITS LAYERS ARE PREFIXED WITH IT ("PPP-CIVIL-TOPO|V-TOPO-MAJR")

(defun PPP:TopoXref ( doc / blk )
	(if (and (setq blk (std:ReturnCollectionItem (vla-get-blocks doc) (PPP:TopoName))) (= :vlax-true (vla-get-isxref blk)))
		blk
	)
)
;; RETURNS THE CIVIL TOPO XREF DEFINITION (BLOCK OBJECT), LOADED OR NOT, NIL IF THE DRAWING HAS NONE (PROJECTS SET UP BEFORE 11-2026 COPIED THE CONTOURS)

(defun PPP:TopoPlace ( doc / ins lst bad )
	(vlax-for obj (vla-get-modelspace doc)
		(if (and (= "AcDbBlockReference" (vla-get-objectname obj)) (= (strcase (PPP:TopoName)) (strcase (vla-get-name obj))))
			(setq lst (cons obj lst))
		)
	) ;; COLLECTED FIRST, CHANGING OBJECTS WHILE vlax-for WALKS THE SAME COLLECTION CAN CRASH AUTOCAD
	(setq bad 0 ins 0)
	(foreach obj lst
		(if (not
				(and
					(equal (vlax-get obj 'InsertionPoint) (list 0.0 0.0 0.0) 1e-6)
					(equal 1.0 (vla-get-xscalefactor obj) 1e-9)
					(equal 1.0 (vla-get-yscalefactor obj) 1e-9)
					(equal 1.0 (vla-get-zscalefactor obj) 1e-9)
					(equal 0.0 (vla-get-rotation obj) 1e-9)
				)
			)
			(if (vl-catch-all-error-p
					(vl-catch-all-apply
						'(lambda ( )
							(vla-put-xscalefactor obj 1.0)
							(vla-put-yscalefactor obj 1.0)
							(vla-put-zscalefactor obj 1.0)
							(vla-put-rotation obj 0.0)
							(vlax-put obj 'InsertionPoint (list 0.0 0.0 0.0))
						)
					)
				)
				(setq bad (1+ bad)) ;; LIKELY A LOCKED LAYER
				(setq ins (1+ ins))
			)
		)
	)
	(strcat
		(if (< 0 ins) (strcat "\n  The " (PPP:TopoName) " xref was moved, scaled or rotated, it was put back at 0,0,0 / scale 1 / rotation 0.") "")
		(if (< 0 bad) (strcat "\n  Warning: the " (PPP:TopoName) " xref is not at 0,0,0 / scale 1 / rotation 0 and could not be fixed (locked layer?), it will not line up with the arrays.") "")
	)
)
;; PUTS EVERY INSERT OF THE CIVIL TOPO XREF AT 0,0,0, SCALE 1, ROTATION 0, WHERE ITS GRID COORDINATES LINE UP WITH THE X-ARRAY
;; AUTOCAD SCALES AN ATTACHED XREF BY THE UNITS OF THE TWO DRAWINGS (INCHES -> FEET == x12, FEET -> US SURVEY FEET == 1.000002), WHICH MOVES IT
;; AWAY FROM 0,0,0 IN PROPORTION TO ITS COORDINATES; THE GEOLOCATION CHECK ALREADY CONFIRMED BOTH FILES USE THE SAME GRID, SO SCALE 1 IS RIGHT
;; THE CONTOUR CALCULATION READS THE XREF DEFINITION AND IS NOT AFFECTED, THIS KEEPS THE DRAWING SHOWING WHAT WAS CALCULATED
;; RETURNS COMMAND LINE TEXT, "" WHEN NOTHING NEEDED FIXING

(defun PPP:TopoLoad ( doc rld / Loaded blk )
	(defun Loaded ( / )
		(= 32 (logand 32 (cond ((cdr (assoc 70 (tblsearch "BLOCK" (PPP:TopoName))))) (0))))
	)
	;; T WHEN THE XREF IS RESOLVED (BLOCK FLAG 32), AN UNRESOLVED XREF HAS AN EMPTY DEFINITION
	
	(if (setq blk (PPP:TopoXref doc))
		(progn
			(if (or rld (not (Loaded)))
				(progn
					(PPP:DebugStep "PPP" "reloading the Civil topo xref")
					(vl-catch-all-apply 'vla-reload (list blk))
				)
			)
			(if (Loaded) (PPP:TopoXref doc)) ;; FETCHED AGAIN, A RELOAD CAN REBUILD THE DEFINITION AND LEAVE THE OLD OBJECT POINTING AT FREED MEMORY
		)
	)
)
;; RETURNS THE CIVIL TOPO XREF DEFINITION ONCE IT IS LOADED, RELOADING IT FIRST WHEN [ rld ] IS T OR IT IS NOT LOADED (ONE TRY)
;; RETURNS NIL IF THERE IS NO XREF OR IT STILL DOES NOT LOAD (FILE MOVED OR RENAMED, SEE ITS PATH IN THE XREF PALETTE)

(defun PPP:AttachTopo ( doc pth / blk res )
	(setq res
		(vl-catch-all-apply
			'(lambda ( )
				(if (setq blk (PPP:TopoXref doc))
					(progn
						(if (/= (strcase (vla-get-path blk)) (strcase pth))
							(vla-put-path blk pth)
						)
						(vla-reload blk)
					) ;; A SECOND SETUP IN THE SAME DRAWING POINTS THE EXISTING XREF AT THE NEW FILE
					(vla-attachexternalreference (vla-get-modelspace doc) pth (PPP:TopoName) (vlax-3d-point 0.0 0.0 0.0) 1.0 1.0 1.0 0.0 :vlax-true)
				) ;; :vlax-true == OVERLAY, IT IS NOT CARRIED INTO DRAWINGS THAT REFERENCE THE PPP DRAWING
			)
		)
	)
	(cond
		((vl-catch-all-error-p res) (vl-catch-all-error-message res))
		((wcmatch (PPP:TopoPlace doc) "*Warning:*") "it could not be put at 0,0,0 / scale 1 (locked layer?), it would not line up with the arrays") ;; ALWAYS RUN, SEE PPP:TopoPlace
		((PPP:TopoLoad doc nil))
		((strcat "the xref did not load: " pth))
	)
)
;; REFERENCES THE CIVIL TOPO DRAWING AS THE "PPP-CIVIL-TOPO" OVERLAY AT 0,0,0, SCALE 1, ROTATION 0 (GRID COORDINATES, CHECK ITS GEOLOCATION FIRST)
;; RETURNS THE LOADED XREF DEFINITION, ELSE THE REASON AS A STRING
;; [ doc ] == DOCUMENT, [ pth ] == FULL PATH OF THE V-TOPO DRAWING

(defun PPP:ContourObjects ( doc cfg / lyr blk src pat skp out )
	(PPP:DebugStep "PPP" "reading the contours")
	(setq
		lyr (vl-remove "" (mapcar '(lambda ( nam ) (strcase (vl-string-trim " " nam))) (json:StringSplit (PPP:SettingText cfg "contour_layers" "V-TOPO-MAJR,V-TOPO-MINR") ",")))
		skp 0
	)
	(cond
		((setq blk (PPP:TopoLoad doc nil))
			(setq
				src (strcat "Civil topo xref (" (vl-filename-base (vla-get-path blk)) ".dwg)")
				pat (json:StringJoin (mapcar '(lambda ( nam ) (strcat (strcase (PPP:TopoName)) "|" nam)) lyr) ",")
			)
		)
		((setq blk (PPP:TopoXref doc))
			(setq
				src (strcat "Civil topo xref " (PPP:TopoName) " is not loaded, file not found: " (vla-get-path blk))
				blk nil
			)
		)
		( t
			(setq
				blk (vla-get-modelspace doc)
				src "contours copied into the drawing (set up before the Civil topo xref)"
				pat (json:StringJoin lyr ",")
			)
		)
	)
	(if blk
		(vlax-for obj blk
			(if (wcmatch (strcase (vla-get-layer obj)) pat)
				(if (std:PolylineElevation obj)
					(setq out (cons obj out))
					(setq skp (1+ skp)) ;; ON A CONTOUR LAYER BUT NOT A POLYLINE (EX. TEXT, CIVIL 3D OBJECTS)
				)
			)
		)
	)
	(list out skp src)
)
;; RETURNS ( CONTOURS SKIPPED SOURCE ): THE CONTOUR POLYLINES (VLA-OBJECTS, ELEVATION 0 INCLUDED), OTHER OBJECTS ON THE CONTOUR LAYERS, AND WHERE THEY CAME FROM
;; FROM THE CIVIL TOPO XREF WHEN THE DRAWING HAS ONE (RELOADED ONCE IF IT IS NOT LOADED), ELSE THE COPIED CONTOURS IN MODEL SPACE (OLDER PROJECTS)
;; NO CONTOURS + A SOURCE TEXT THAT SAYS WHY WHEN THE XREF CANNOT BE LOADED, SHARED BY PPPSETUP (VALIDATION) AND PPPPILES (ESTIMATE)
;; [ doc ] == DOCUMENT, [ cfg ] == PROJECT SETTINGS ("contour_layers", EX. "V-TOPO-MAJR,V-TOPO-MINR", WITHOUT THE XREF PREFIX)

(defun PPP:ContourInterval ( objs / lvl prv cnt out )
	(foreach obj objs
		(setq lvl (cons (atof (rtos (std:PolylineElevation obj) 2 2)) lvl))
	) ;; ELEVATIONS TO 0.01 FT
	(foreach elv (vl-sort lvl '<)
		(if (and prv (> (- elv prv) 0.005))
			(setq cnt (std:AssocIncrement cnt (rtos (- elv prv) 2 2)))
		) ;; GAP BETWEEN NEIGHBORING LEVELS, REPEATED LEVELS (MANY POLYLINES AT ONE ELEVATION) ARE SKIPPED
		(setq prv elv)
	)
	(foreach itm cnt
		(if (or (null out) (> (cdr itm) (cdr out)))
			(setq out itm)
		)
	) ;; THE MOST COMMON GAP IS THE CONTOUR INTERVAL (A MISSING LEVEL ONLY ADDS ONE LARGER GAP)
	(if out (atof (car out)))
)
;; RETURNS THE CONTOUR INTERVAL (FT) OF CONTOUR POLYLINES (SEE PPP:ContourObjects): THE MOST COMMON RISE BETWEEN NEIGHBORING ELEVATIONS, NIL IF THERE ARE FEWER THAN TWO LEVELS
;; THE ESTIMATE IS LINEAR BETWEEN CONTOURS, SO A RISE SMALLER THAN THE INTERVAL BETWEEN TWO CROSSINGS (EX. A HUMP BETWEEN PILES) CANNOT BE SEEN
;; [ objs ] == CONTOUR POLYLINES (VLA-OBJECTS)

(defun PPP:SourceStamp ( cfg key pth / )
	(std:AssocSet
		(std:AssocSet cfg (strcat key "_file") pth)
		(strcat key "_date")
		(rtos (cond ((std:FileTimeStamp pth)) (0.0)) 2 0)
	)
)
;; RETURNS THE SETTINGS WITH A SOURCE FILE'S PATH AND LAST MODIFIED TIME SAVED UNDER "<key>_file" AND "<key>_date" (EX. "topo_file", "topo_date")
;; [ cfg ] == PROJECT SETTINGS, [ key ] == "xarray" OR "topo", [ pth ] == FULL PATH OF THE SOURCE FILE

(defun PPP:SourceCheck ( doc cfg rld / DateText pth old now out )
	(defun DateText ( num / str )
		(setq str (rtos num 2 0))
		(if (= 14 (strlen str))
			(strcat (substr str 1 4) "-" (substr str 5 2) "-" (substr str 7 2) " " (substr str 9 2) ":" (substr str 11 2))
			str
		)
	)
	;; YYYYMMDDHHMMSS -> "YYYY-MM-DD HH:MM"
	
	(foreach src (list (list "xarray" "X-ARRAY") (list "topo" "Civil topo"))
		(setq
			pth (cdr (assoc (strcat (car src) "_file") cfg))
			old (std:ToNumber (cdr (assoc (strcat (car src) "_date") cfg)))
			now (std:FileTimeStamp pth)
		)
		(cond
			((/= 'STR (type pth))
				nil ;; SET UP BEFORE THE FILES WERE STAMPED, NOTHING TO COMPARE
			)
			((null now)
				(setq out (cons (strcat "\n  Warning: the " (cadr src) " file was not found: " pth) out))
			)
			((not (and old (> now old)))
				nil ;; UNCHANGED
			)
			((and rld (= "topo" (car src)) (PPP:TopoLoad doc t))
				(setq
					cfg (PPP:SourceStamp cfg "topo" pth)
					out (cons (strcat "\n  Civil topo changed " (DateText now) " (was " (DateText old) "), the xref was reloaded.") out)
				)
			)
			((= "topo" (car src))
				(setq out (cons (strcat "\n  Warning: the Civil topo changed " (DateText now) " (was " (DateText old) "), reload the " (PPP:TopoName) " xref.") out))
			)
			( t
				(setq out
					(cons
						(strcat
							"\n  Warning: the X-ARRAY changed " (DateText now) " (copied in " (DateText old) "), this drawing may not have the current layout."
							"\n    Run PPPSETUP again to bring in the new layout: " pth
						)
						out
					)
				)
			) ;; !REVISIT! X-ARRAY UPDATE SYNC (KEEP ASSIGNMENTS, REPORT ADDED / REMOVED / MOVED TRACKERS) IS NOT BUILT YET
		)
	)
	(PPP:DebugStep "PPP" "source files checked, checking the Civil topo xref placement")
	(list cfg (strcat (apply 'strcat (reverse out)) (PPP:TopoPlace doc)))
)
;; COMPARES THE X-ARRAY AND CIVIL TOPO FILES WITH THE TIMES SAVED AT SETUP (SEE PPP:SourceStamp), RUN AT THE START OF PPPASSIGN AND PPPPILES
;; ALSO PUTS THE TOPO XREF BACK AT 0,0,0 / SCALE 1 IF IT WAS MOVED (SEE PPP:TopoPlace), SO DRAWINGS SET UP WITH THE x12 SCALE ARE FIXED ON THE NEXT RUN
;; A CHANGED TOPO IS RELOADED WHEN [ rld ] IS T AND ITS NEW TIME SAVED IN THE RETURNED SETTINGS, A CHANGED X-ARRAY IS ONLY REPORTED
;; RETURNS ( SETTINGS MESSAGES ), MESSAGES == COMMAND LINE TEXT, "" WHEN NOTHING CHANGED
;; [ doc ] == DOCUMENT, [ cfg ] == PROJECT SETTINGS, [ rld ] == T TO RELOAD A CHANGED TOPO XREF



;;; PPP SHARED FUNCTIONS - ARRAY BLOCKS ;;;

(defun PPP:IsArrayBlock ( obj cfg / nam )
	(and
		(= (vla-get-objectname obj) "AcDbBlockReference")
		(wcmatch (strcase (vla-get-layer obj)) (strcase (cdr (assoc "array_layers" cfg))))
		(not (vl-catch-all-error-p (setq nam (vl-catch-all-apply 'vla-get-effectivename (list obj))))) ;; EFFECTIVE NAME HANDLES DYNAMIC BLOCKS
		(PPP:IsArrayName nam cfg)
	)
)
;; CHECKS IF AN OBJECT IS A PV ARRAY BLOCK USING THE ARRAY LAYER PATTERN AND THE ARRAY BLOCK NAMES FROM THE PROJECT SETTINGS [ cfg ]
;; FILTERS OUT DUPLICATE LAYOUTS (EX. E-LBD TABLES) AND NON-ARRAY BLOCKS (EX. LBDGP-15A)
;; RETURNS T IF THE OBJECT IS AN ARRAY BLOCK, ELSE NIL

(defun PPP:ArrayNames ( cfg / val )
	(if (= 'STR (type (setq val (cdr (assoc "array_block_names" cfg)))))
		(vl-remove "" (mapcar '(lambda ( nam ) (strcase (vl-string-trim " " nam))) (json:StringSplit val "|")))
	)
)
;; RETURNS THE ARRAY BLOCK NAMES PICKED IN THE ARRAY BLOCKS DIALOG (SETTING "array_block_names"), UPPER CASE, NIL IF NONE WERE PICKED
;; THE NAMES ARE SAVED SEPARATED BY "|", WHICH CANNOT BE PART OF A BLOCK NAME

(defun PPP:IsArrayName ( nam cfg / lst )
	(and
		(= 'STR (type nam))
		(not (wcmatch (strcase nam) "PPP-*"))
		(if (setq lst (PPP:ArrayNames cfg))
			(member (strcase nam) lst)
			(wcmatch (strcase nam) (strcase (PPP:SettingText cfg "array_blocks" "")))
		)
	)
)
;; RETURNS T IF A BLOCK NAME IS AN ARRAY BLOCK NAME: ONE OF THE PICKED NAMES, OR WHEN NONE WERE PICKED (PROJECTS SET UP BEFORE
;; THE ARRAY BLOCKS DIALOG) A MATCH FOR THE "array_blocks" PATTERN (EX. "*PVBlock"), PPP BLOCKS ("PPP-*") ARE NEVER ARRAY BLOCKS
;; A BLANK PATTERN MATCHES NOTHING, THE ARRAY BLOCKS DIALOG THEN DECIDES

(defun PPP:XdataValues ( ent app / )
	(cdr (assoc app (cdr (assoc -3 (entget ent (list app))))))
)
;; RETURNS THE ( CODE . VALUE ) ITEMS OF ONE APPLICATION'S XDATA ON AN ENTITY (WITHOUT THE APPLICATION NAME), NIL IF IT HAS NONE
;; EVERY PPP XDATA READ GOES THROUGH HERE: PPPASSIGN, PPPPLACE, PPPPILE, PPPSETUP
;; [ ent ]	== ENAME, [ app ] == REGISTERED APPLICATION NAME (EX. "PPPASSIGN")

(defun PPP:AssignValues ( ent / )
	(mapcar 'cdr (PPP:XdataValues ent "PPPASSIGN"))
)
;; RETURNS THE 'PPPASSIGN' XDATA VALUES OF A BLOCK ( PRESET ROW SIDES ENDS LAYER BLOCK ), NIL IF THE BLOCK IS NOT ASSIGNED
;; WRITTEN BY PPPASSIGN (SEE WriteAssignment), READ BY PPPASSIGN ON RERUNS AND BY PPPPILES
;; BLOCKS ASSIGNED BEFORE END EXPOSURE WAS ADDED HAVE ONLY ( PRESET ROW SIDES ), READ THE VALUES WITH std:SafeNth

(defun PPP:DefinitionData ( doc nam / mn mx box ext ivs mrg )
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
;; MEASURES AN ARRAY BLOCK DEFINITION [ nam ] IN DOCUMENT [ doc ], CALLERS SHOULD CACHE THE RESULT PER BLOCK NAME
;; RETURNS ( LENGTH WIDTH EXTENTS MOTORS ), NIL IF THE BLOCK HAS NO MEASURABLE OBJECTS
;; MOTORS == CENTER OF EACH MOTOR FROM THE NORTH EDGE OF THE BLOCK (THE NORTH OVERHANG LINE), NIL IF NO MOTOR OBJECT WAS FOUND
;; VERIFIED ON ORCA FALLS '1P84@60DEG F TR ID8 PVBlock': MOTOR CENTER 253.086 FT == 56M28, MOTOR HEIGHT 5.922 FT == MOTOR GAP
;; BLOCK COORDINATES (ORCA FALLS): THE INSERTION POINT AND THE NORTH EDGE ARE THE NORTH END OF THE TORQUE TUBE, THE TUBE RUNS DOWN THE CENTER (MIN X + MAX X) / 2
;; VERIFIED ON ORCA FALLS: ALL 36,032 BOM PILES == INSERTION + ( -3.911, -SUM OF HorizontalDistances ), WITHIN 0.055 FT BEFORE SLOPE CORRECTION

(defun PPP:PresetSizeMatch ( prs len wid / nam tub sx sy out )
	(if (and (numberp len) (numberp wid))
		(foreach pre prs
			(if (and
					(= "INT" (PPP:RowPrefix (setq nam (json:Get pre "Name"))))
					(null (PPP:PresetProblem pre))
					(setq tub (PPP:TubeLength pre))
					(numberp (setq sx (json:Get pre "SizeX")))
					(numberp (setq sy (json:Get pre "SizeY")))
					(<= (abs (- len tub)) (* 0.5 sy)) ;; SAME LENGTH WITHIN HALF A MODULE, THE SAME RULE PPPASSIGN USES
					(<= (* 0.5 sx) wid (* 1.5 sx)) ;; ONE TRACKER WIDE, A GROUP OF TRACKERS SIDE BY SIDE IS WIDER
				)
				(setq out (cons (PPP:TrackerName nam) out))
			)
		)
	)
	(reverse out)
)
;; RETURNS THE TRACKER NAMES (INT PRESETS WITHOUT THEIR PREFIX, EX. "90-45M45") WITH THE SIZE OF A BLOCK, NIL IF NONE MATCH
;; [ prs ] == TRACKER PRESETS, [ len ] == BLOCK LENGTH NORTH-SOUTH (FT), [ wid ] == BLOCK WIDTH EAST-WEST (FT), SEE PPP:DefinitionData
;; TWO TRACKERS CAN SHARE A SIZE (EX. 28M56 / 56M28), THE MOTOR POSITION PICKS BETWEEN THEM IN PPPASSIGN

(defun PPP:ScanArrayBlocks ( doc lst cfg prs / lyr blks nam old top dfs nst dfd mtc out )
	(setq
		lyr (strcase (cdr (assoc "array_layers" cfg)))
		blks (vla-get-blocks doc)
	)
	(foreach obj lst
		(if (and
				(= (vla-get-objectname obj) "AcDbBlockReference")
				(wcmatch (strcase (vla-get-layer obj)) lyr)
				(not (vl-catch-all-error-p (setq nam (vl-catch-all-apply 'vla-get-effectivename (list obj)))))
				(not (wcmatch (strcase nam) "PPP-*"))
			)
			(setq
				top (std:AssocIncrement top nam)
				dfs (std:AssocIncrement dfs (vla-get-name obj)) ;; DEFINITION IN USE, A DYNAMIC BLOCK INSERT HAS ITS OWN ANONYMOUS DEFINITION
			)
		)
	)
	(foreach def dfs
		(vlax-for sub (vla-item blks (car def))
			(if (and
					(= (vla-get-objectname sub) "AcDbBlockReference")
					(not (vl-catch-all-error-p (setq nam (vl-catch-all-apply 'vla-get-effectivename (list sub)))))
					(not (wcmatch (strcase nam) "PPP-*"))
				)
				(setq nst
					(if (setq old (assoc nam nst))
						(subst (cons nam (+ (cdr old) (cdr def))) old nst)
						(cons (cons nam (cdr def)) nst)
					)
				) ;; EACH NESTED INSERT COUNTS ONCE PER INSERT OF ITS GROUP
			)
		)
	) ;; ONE LEVEL DOWN, BLOCKS INSIDE GROUP BLOCKS (EX. MCCRACKEN "12STR A" HOLDS FOUR TRACKERS ON LAYER 0)
	(foreach nam (append (mapcar 'car top) (vl-remove-if '(lambda ( itm ) (assoc itm top)) (mapcar 'car nst)))
		(if (vl-catch-all-error-p (setq dfd (vl-catch-all-apply 'PPP:DefinitionData (list doc nam))))
			(setq dfd nil)
		)
		(setq mtc (if dfd (PPP:PresetSizeMatch prs (car dfd) (cadr dfd))))
		(if (or (assoc nam top) mtc (PPP:IsArrayName nam cfg)) ;; A BLOCK ONLY FOUND INSIDE GROUPS IS LISTED IF IT COULD BE A TRACKER
			(setq out
				(cons
					(list
						nam
						(cond ((cdr (assoc nam top))) (0))
						(cond ((cdr (assoc nam nst))) (0))
						(car dfd)
						(cadr dfd)
						mtc
					)
					out
				)
			)
		)
	)
	(vl-sort out '(lambda ( a b ) (> (+ (cadr a) (caddr a)) (+ (cadr b) (caddr b)))))
)
;; LISTS THE BLOCK TYPES ON THE ARRAY LAYERS FOR THE ARRAY BLOCKS DIALOG (SEE PPP:ArrayBlockPicker), MOST USED FIRST
;; [ doc ] == DOCUMENT, [ lst ] == OBJECTS TO SCAN (EX. THE OBJECTS COPIED FROM X-ARRAY), [ cfg ] == PROJECT SETTINGS, [ prs ] == TRACKER PRESETS
;; RETURNS ( NAME COUNT COUNT-IN-GROUPS LENGTH WIDTH TRACKER-MATCHES ) ITEMS, LENGTH / WIDTH ARE NIL IF THE BLOCK IS NOT MEASURABLE

(defun PPP:ArrayGroups ( doc lst cfg / lyr blks nam def cnt dfs out )
	(setq
		lyr (strcase (cdr (assoc "array_layers" cfg)))
		blks (vla-get-blocks doc)
	)
	(foreach obj lst
		(if (and
				(= (vla-get-objectname obj) "AcDbBlockReference")
				(wcmatch (strcase (vla-get-layer obj)) lyr)
				(not (vl-catch-all-error-p (setq nam (vl-catch-all-apply 'vla-get-effectivename (list obj)))))
				(not (wcmatch (strcase nam) "PPP-*"))
				(not (PPP:IsArrayName nam cfg))
			)
			(progn
				(setq def (vla-get-name obj))
				(if (not (assoc def dfs))
					(progn
						(setq cnt 0)
						(vlax-for sub (vla-item blks def)
							(if (and
									(= (vla-get-objectname sub) "AcDbBlockReference")
									(not (vl-catch-all-error-p (setq nam (vl-catch-all-apply 'vla-get-effectivename (list sub)))))
									(PPP:IsArrayName nam cfg)
								)
								(setq cnt (1+ cnt))
							)
						)
						(setq dfs (cons (cons def cnt) dfs))
					)
				) ;; EACH DEFINITION IS CHECKED ONCE
				(if (> (cdr (assoc def dfs)) 0)
					(setq out (cons (cons obj (cdr (assoc def dfs))) out))
				)
			)
		)
	)
	(reverse out)
)
;; FINDS GROUP BLOCKS: INSERTS ON THE ARRAY LAYERS THAT ARE NOT ARRAY BLOCKS BUT HOLD ARRAY BLOCKS ONE LEVEL DOWN
;; [ doc ] == DOCUMENT, [ lst ] == OBJECTS TO CHECK, [ cfg ] == PROJECT SETTINGS (ARRAY LAYERS AND BLOCK NAMES)
;; RETURNS ( GROUP-OBJECT . ARRAY-BLOCKS-INSIDE ) PAIRS, NIL IF THERE ARE NONE

(defun PPP:ExplodeGroups ( doc grp cfg / lys lyr obj nam lay flg res sub snm gcn trk skp fal )
	(regapp "PPPGROUP") ;; RETURNS NIL IF ALREADY REGISTERED, WHICH IS FINE
	(setq
		lys (vla-get-layers doc)
		lyr (strcase (cdr (assoc "array_layers" cfg)))
		gcn 0
		trk 0
		fal 0
	)
	(foreach itm grp
		(setq
			obj (car itm)
			nam (vla-get-effectivename obj)
			lay (vla-get-layer obj)
			flg (if (setq flg (PPP:XdataValues (vlax-vla-object->ename obj) "PPPSETUP")) (list (cons "PPPSETUP" flg))) ;; PPPSETUP FLAGS ON THE GROUP (EX. IGNORED, OUTSIDE THE CONTOURS) CARRY OVER
		)
		(cond
			((not
					(and
						(equal 1.0 (vla-get-xscalefactor obj) 1e-6)
						(equal 1.0 (vla-get-yscalefactor obj) 1e-6)
						(equal 1.0 (vla-get-zscalefactor obj) 1e-6)
					)
				)
				(setq skp (cons nam skp)) ;; MIRRORED OR SCALED, ITS TRACKERS WOULD BE MIRRORED OR SCALED TOO
			)
			((or
					(= :vlax-true (vla-get-lock (vla-item lys lay)))
					(vl-catch-all-error-p (setq res (vl-catch-all-apply 'vla-explode (list obj))))
					(vl-catch-all-error-p (setq res (vl-catch-all-apply 'vlax-safearray->list (list (vlax-variant-value res)))))
				)
				(setq fal (1+ fal)) ;; LOCKED LAYER, THE GROUP STAYS AS IT IS
			)
			( t
				(foreach new res
					(setq snm
						(and
							(= (vla-get-objectname new) "AcDbBlockReference")
							(= 'STR (type (setq snm (vl-catch-all-apply 'vla-get-effectivename (list new)))))
							(PPP:IsArrayName snm cfg)
						)
					) ;; T FOR AN ARRAY BLOCK
					(if (or (= "0" (vla-get-layer new)) (and snm (not (wcmatch (strcase (vla-get-layer new)) lyr))))
						(progn
							(vla-put-layer new lay)
							(if (eq 0 (vl-catch-all-apply 'vla-get-color (list new)))
								(vl-catch-all-apply 'vla-put-color (list new 256))
							) ;; BYBLOCK -> BYLAYER, SO IT STILL SHOWS IN THE GROUP'S COLOR
						)
					) ;; LAYER 0 CONTENT SHOWED ON THE GROUP'S LAYER SO IT STAYS THERE, THE GROUP'S INVERTER LAYER ALSO DEFINES A TRACKER'S AREA
					(if snm
						(progn
							(setq sub (vlax-vla-object->ename new))
							(entmod (append (entget sub) (list (cons -3 (cons (list "PPPGROUP" (cons 1000 nam) (cons 1000 lay)) flg)))))
							(setq trk (1+ trk))
						)
					)
				)
				(vla-delete obj)
				(setq gcn (1+ gcn))
			)
		)
	)
	(prompt
		(strcat
			"\nGroup blocks: " (itoa gcn) " exploded into " (itoa trk) " array blocks."
			(if skp (strcat "\n  Skipped (mirrored or scaled, check by hand): " (itoa (length skp)) " (" (json:StringJoin (reverse skp) ", ") ")") "")
			(if (> fal 0) (strcat "\n  Warning: " (itoa fal) " group blocks could not be exploded, check for locked layers.") "")
		)
	)
	(list gcn trk (reverse skp) fal)
)
;; EXPLODES GROUP BLOCKS ONE LEVEL (SEE PPP:ArrayGroups) SO EACH TRACKER INSIDE IS ITS OWN INSERT THAT PPPASSIGN CAN ASSIGN
;; ARRAY BLOCKS OFF THE ARRAY LAYERS (EX. LAYER 0) AND OTHER LAYER 0 CONTENT MOVE TO THE GROUP'S LAYER (BYBLOCK COLOR -> BYLAYER), SO THE DRAWING LOOKS THE SAME
;; EVERY ARRAY BLOCK FROM A GROUP IS TAGGED WITH 'PPPGROUP' XDATA: ( GROUP BLOCK NAME, GROUP LAYER ), AND KEEPS THE GROUP'S 'PPPSETUP' FLAGS
;; MIRRORED OR SCALED GROUPS ARE LEFT AS THEY ARE AND REPORTED, NESTED GROUPS (A GROUP INSIDE A GROUP) ARE NOT EXPLODED FURTHER
;; [ doc ] == DOCUMENT, [ grp ] == RESULT OF PPP:ArrayGroups, [ cfg ] == PROJECT SETTINGS
;; RETURNS ( EXPLODED ARRAY-BLOCKS SKIPPED-NAMES FAILED ), PRINTS A ONE LINE REPORT



;;; PPP SHARED FUNCTIONS - HEIGHTS AND TRACKER PLACEMENT ;;;
;; ONE STRAIGHT TORQUE TUBE PER TRACKER: FRAME Z = A + b x s, s == DISTANCE ALONG THE TUBE SOUTH OF THE ANCHOR PILE (NEGATIVE NORTH OF IT)
;; b == SIN(TILT), A == FRAME (TORQUE TUBE CENTER) Z AT THE ANCHOR PILE, COTT AT A PILE == FRAME Z - GROUND Z
;; PILE PLAN POSITION == ANCHOR + s x COS(TILT), SO A TILTED TRACKER IS SHORTER IN PLAN AND THE ANCHOR PILE NEVER MOVES
;; PROVED IN PYTHON ON YELLOW BANKS AND CULPEPPER (10-2026): SAME OPTIMUM AS AN LP / MILP SOLVER ON ALL 3,254 TABLES

(defun PPP:SolveTable ( s zt lo hi bmax gol / LowestLine Score Better lns rst ln1 ln2 b bst sco eps tol )
	
	(defun LowestLine ( / pts hul p1 p2 sm b a )
		(setq pts (mapcar '(lambda ( si zi li ) (list si (+ li zi))) s zt lo))
		(foreach p pts
			(while
				(and
					(cdr hul)
					(setq p1 (cadr hul) p2 (car hul))
					(>= (- (* (- (car p2) (car p1)) (- (cadr p) (cadr p1))) (* (- (cadr p2) (cadr p1)) (- (car p) (car p1)))) 0.0)
				)
				(setq hul (cdr hul))
			)
			(setq hul (cons p hul))
		) ;; UPPER HULL OF THE PILE MINIMUMS (s, MIN COTT + GROUND), BUILT WITH THE NEWEST POINT FIRST
		(setq
			hul (reverse hul)
			sm (/ (apply '+ s) (float (length s))) ;; MEAN s: THE TOTAL REVEAL IS LOWEST WHERE THE LINE IS LOWEST AT THE MEAN
			b 0.0
		)
		(while (cdr hul)
			(setq p1 (car hul) p2 (cadr hul) hul (cdr hul))
			(if (and (<= (car p1) sm) (< sm (car p2)) (> (- (car p2) (car p1)) 1e-9))
				(setq
					b (/ (- (cadr p2) (cadr p1)) (- (car p2) (car p1)))
					hul nil
				)
			)
		) ;; THE HULL EDGE OVER THE MEAN GIVES THE TILT
		(setq
			b (max (- bmax) (min bmax b))
			a (apply 'max (mapcar '(lambda ( p ) (- (cadr p) (* b (car p)))) pts)) ;; LOWEST HEIGHT THAT KEEPS EVERY PILE AT ITS MINIMUM
		)
		(if (vl-every '(lambda ( si zi hj ) (<= (- (+ a (* b si)) zi) (+ hj 1e-9))) s zt hi)
			(list a b 0.0 0)
		)
	)
	;; THE FAST PATH: THE LOWEST LINE ABOVE EVERY PILE MINIMUM (TILT LIMITED), RETURNED ONLY IF IT ALSO KEEPS EVERY PILE UNDER ITS MAXIMUM
	;; THEN IT IS THE EXACT OPTIMUM FOR BOTH GOALS (NO GRADING, LOWEST TOTAL REVEAL), O(n) INSTEAD OF O(n^3), MOST TRACKERS STOP HERE
	;; CHECKED IN PYTHON ON YELLOW BANKS AND CULPEPPER: SAME RESULT AS THE FULL SEARCH ON EVERY TRACKER IT HANDLES
	
	(defun Score ( a b / ss zz ll hh cot dif g n r )
		(setq ss s zz zt ll lo hh hi g 0.0 n 0 r 0.0)
		(while (and ss (or (null bst) (if (= gol "fewest_piles") (<= n (cadr bst)) (<= g (+ (car bst) tol)))))
			(setq cot (- (+ a (* b (car ss))) (car zz)))
			(cond
				((> (setq dif (- (car ll) cot)) 0.0)
					(setq g (+ g dif))
					(if (> dif eps) (setq n (1+ n)))
				)
				((> (setq dif (- cot (car hh))) 0.0)
					(setq g (+ g dif))
					(if (> dif eps) (setq n (1+ n)))
				)
			)
			(setq
				r (+ r cot)
				ss (cdr ss) zz (cdr zz) ll (cdr ll) hh (cdr hh)
			)
		)
		(if (null ss)
			(list g n r a b)
		)
	)
	;; RETURNS ( GRADING PILES-OUT REVEAL-SUM A b ) FOR ONE CANDIDATE LINE, NIL WHEN IT IS ALREADY WORSE THAN THE BEST (EARLY EXIT)
	
	(defun Better ( c / )
		(cond
			((null c) nil)
			((null bst) t)
			((= gol "fewest_piles")
				(or
					(< (cadr c) (cadr bst))
					(and (= (cadr c) (cadr bst)) (< (car c) (- (car bst) tol)))
					(and (= (cadr c) (cadr bst)) (<= (abs (- (car c) (car bst))) tol) (< (caddr c) (- (caddr bst) 1e-9)))
				)
			)
			( t
				(or
					(< (car c) (- (car bst) tol))
					(and (<= (abs (- (car c) (car bst))) tol) (< (cadr c) (cadr bst)))
					(and (<= (abs (- (car c) (car bst))) tol) (= (cadr c) (cadr bst)) (< (caddr c) (- (caddr bst) 1e-9)))
				)
			)
		)
	)
	;; T WHEN CANDIDATE [ c ] BEATS THE BEST SO FAR: LEAST GRADING, THEN FEWEST PILES OUT, THEN LOWEST TOTAL REVEAL (OR FEWEST PILES FIRST)
	
	(setq
		eps 5e-4 ;; A PILE IS OUT WHEN IT MISSES ITS WINDOW BY MORE THAN THIS (FT), ROUNDS TO 0.000 IN THE BOM
		tol 1e-7 ;; GRADING TOTALS THIS CLOSE ARE A TIE (FLOAT NOISE), A LARGER TIE LET A SLIGHTLY GRADED LINE BEAT A LOWER, CLEAN ONE
		lns (append (mapcar '(lambda ( si zi li ) (cons si (+ li zi))) s zt lo) (mapcar '(lambda ( si zi hj ) (cons si (+ hj zi))) s zt hi))
	) ;; EVERY PILE WINDOW LIMIT AS A LINE  A + b x s == c  IN THE ( A b ) PLANE
	(setq rst (if (null (setq bst (LowestLine))) lns)) ;; NO FULL SEARCH WHEN THE FAST PATH FITS
	(while rst
		(setq ln1 (car rst) rst (cdr rst))
		(foreach ln2 rst
			(if (> (abs (- (car ln1) (car ln2))) 1e-9)
				(progn
					(setq b (/ (- (cdr ln1) (cdr ln2)) (- (car ln1) (car ln2))))
					(if (and (<= (abs b) (+ bmax 1e-12)) (Better (setq sco (Score (- (cdr ln1) (* b (car ln1))) b))))
						(setq bst sco)
					)
				)
			)
		)
		(foreach lim (list (- bmax) bmax)
			(if (Better (setq sco (Score (- (cdr ln1) (* lim (car ln1))) lim)))
				(setq bst sco)
			)
		) ;; WHERE THIS LINE CROSSES A TILT LIMIT
	) ;; THE OPTIMUM OF EVERY GOAL IS AT A VERTEX: TWO LINES CROSSING, OR A LINE AT A TILT LIMIT
	(cond
		((null bst) nil)
		((= 4 (length bst)) bst) ;; FAST PATH, ALREADY ( A b GRADING PILES-OUT )
		((list (nth 3 bst) (nth 4 bst) (car bst) (cadr bst)))
	)
)
;; SOLVES ONE TRACKER EXACTLY: RETURNS ( A b GRADING PILES-OUT ), NIL WITH NO PILES
;; TRACKERS THAT FIT COME BACK AS LOW AS POSSIBLE (LOWEST TOTAL REVEAL) WITH EVERY PILE IN ITS WINDOW, BOTH GOALS AGREE THERE
;; [ s ] == PILE DISTANCES ALONG THE TUBE FROM THE ANCHOR (FT, + SOUTH), SORTED NORTH TO SOUTH, [ zt ] == GROUND Z, [ lo hi ] == EACH PILE'S COTT WINDOW
;; [ bmax ] == SIN(MAX TILT), [ gol ] == "least_grading" (DEFAULT) OR "fewest_piles"
;; ABOUT 2n x 2n CANDIDATES OF n PILES EACH, THE EARLY EXIT IN Score SKIPS MOST OF THE WORK ONCE A GOOD LINE IS FOUND

(defun PPP:StowDrop ( pre / siz ang )
	(if (and (numberp (setq siz (json:Get pre "SizeX"))) (numberp (setq ang (json:Get pre "TiltAngle"))))
		(* 0.5 siz (sin (* pi (/ ang 180.0))))
	)
)
;; RETURNS HOW FAR THE LOWEST MODULE EDGE SITS BELOW THE COTT AT MAX TILT (FT): HALF THE MODULE E-W LENGTH x SIN(MAX TILT), NIL IF UNKNOWN
;; MEASURED FROM THE COTT (CONFIRMED 10-2026: ROTATION AXIS AT THE COTT, MAX TILT GOVERNS CLEARANCE), MODULE ASSUMED CENTERED ON THE TUBE
;; CHECK: A 7.47 FT MODULE AT 60 DEG GIVES 3.24 FT, + 2.5 FT EDGE CLEARANCE == 5.74 FT, THE 5.75 MINIMUM YELLOW BANKS USES
;; !REVISIT! MODULE FRAME HEIGHT ABOVE THE COTT IS TAKEN AS 0 (IT WOULD SUBTRACT HEIGHT x COS(TILT))

(defun PPP:PlaceValues ( ent / )
	(mapcar 'cdr (PPP:XdataValues ent "PPPPLACE"))
)
;; RETURNS THE PLACEMENT STORED ON A PPP BLOCK ( DZ DY ANGLE ), NIL IF THE BLOCK IS FLAT

(defun PPP:PlaceTracker ( obj dz dy ang / ins ent )
	(setq ins (vlax-get obj 'InsertionPoint))
	(std:RotateAboutX obj ang (cadr ins) (caddr ins))
	(std:TranslateObject obj 0.0 dy dz)
	(regapp "PPPPLACE") ;; RETURNS NIL IF ALREADY REGISTERED, WHICH IS FINE
	(setq ent (vlax-vla-object->ename obj))
	(entmod (append (entget ent) (list (list -3 (list "PPPPLACE" (cons 1040 dz) (cons 1040 dy) (cons 1040 ang))))))
)
;; TILTS A FLAT PPP BLOCK ABOUT ITS INSERTION POINT (EAST-WEST AXIS) BY [ ang ] RADIANS, THEN MOVES IT [ dy ] NORTH AND [ dz ] UP, AND STORES THE
;; THREE VALUES AS 'PPPPLACE' XDATA SO PPP:FlattenTracker CAN UNDO EXACTLY THIS PLACEMENT LATER
;; POSITIVE [ ang ] LIFTS THE NORTH END (SEE std:RotateAboutX), THE BLOCK'S PLAN LENGTH SHRINKS BY COS(ang) LIKE THE PILES

(defun PPP:FlattenTracker ( obj / val ins ent )
	(if (setq val (PPP:PlaceValues (setq ent (vlax-vla-object->ename obj))))
		(progn
			(std:TranslateObject obj 0.0 (- (cadr val)) (- (car val)))
			(setq ins (vlax-get obj 'InsertionPoint))
			(std:RotateAboutX obj (- (caddr val)) (cadr ins) (caddr ins))
			(entmod (append (entget ent) (list (list -3 (list "PPPPLACE"))))) ;; AN APP NAME WITH NO DATA REMOVES ITS XDATA
		)
	)
	val
)
;; PUTS A PLACED PPP BLOCK BACK FLAT (THE REVERSE OF PPP:PlaceTracker FROM ITS STORED VALUES), RETURNS THE PLACEMENT IT REMOVED, NIL IF IT WAS FLAT
;; A BLOCK MOVED IN PLAN SINCE IT WAS PLACED STILL COMES BACK RIGHT (THE TILT IS UNDONE ABOUT ITS CURRENT INSERTION POINT)

(defun PPP:FlattenTrackers ( doc / lst out val bad )
	(setq bad 0)
	(vlax-for obj (vla-get-modelspace doc)
		(if (and
				(= "AcDbBlockReference" (vla-get-objectname obj))
				(PPP:PlaceValues (vlax-vla-object->ename obj))
			)
			(setq lst (cons obj lst))
		)
	) ;; COLLECTED FIRST: CHANGING OBJECTS WHILE vlax-for WALKS THE SAME COLLECTION CAN CRASH AUTOCAD
	(foreach obj lst
		(if (vl-catch-all-error-p (setq val (vl-catch-all-apply 'PPP:FlattenTracker (list obj))))
			(setq bad (1+ bad)) ;; LIKELY A LOCKED LAYER
			(setq out (cons (cons (vlax-vla-object->ename obj) val) out))
		)
	)
	(if (< 0 bad)
		(prompt (strcat "\nWarning: " (itoa bad) " placed tracker blocks could not be flattened (locked layer?), they are skipped."))
	)
	out
)
;; FLATTENS EVERY PLACED PPP BLOCK SO PLAN LOGIC (POSITIONS, ROTATION, EDGES, SWAPS) READS FLAT BLOCKS
;; CALLED AT THE START OF PPPASSIGN AND PPPPILES, RETURNS ( ENAME DZ DY ANGLE ) ITEMS FOR PPP:RestoreTrackers

(defun PPP:RestoreTrackers ( lst skp / obj )
	(foreach itm lst
		(if (and (entget (car itm)) (not (member (car itm) skp)) (null (PPP:PlaceValues (car itm)))) ;; NEVER PLACES A BLOCK TWICE
			(progn
				(setq obj (vlax-ename->vla-object (car itm)))
				(vl-catch-all-apply 'PPP:PlaceTracker (list obj (nth 1 itm) (nth 2 itm) (nth 3 itm)))
			)
		)
	)
)
;; PUTS BACK THE PLACEMENTS PPP:FlattenTrackers REMOVED, EXCEPT THE BLOCKS IN [ skp ] (ENAMES PLACED AGAIN BY THE CALLER), ERASED BLOCKS ARE SKIPPED


;;; PPP SHARED FUNCTIONS - PILE DATA AND BOM ;;;
;; EVERY PILE LABEL CARRIES ITS PILE DATA AS 'PPPPILE' XDATA (WRITTEN BY PPPPILES), THE BOM ROWS ARE ALWAYS BUILT FROM THAT PILE DATA,
;; SO PPPPILES AND PPPBOM WRITE THE SAME FILES, AND PPPBOM CAN WRITE THEM AGAIN WITHOUT SOLVING (EX. THE BOM WAS OPEN IN EXCEL)
;; PILE DATA == LIST OF 22 ITEMS:
;;   0 LABEL  1 TABLE  2 POLE  3 AREA  4 PRESET  5 ROW TYPE  6 PILE CLASS  7 MOTOR (1 / 0)  8 X  9 Y (SLOPE CORRECTED)  10 ARRAY BLOCK HANDLE
;;   11 HEIGHTS SOURCE ("CIVIL" / "ESTIMATE", NIL WITHOUT HEIGHTS)  12 Z ENTER  13 Z FRAME  14 COTT  15 REVEAL  16 TABLE SLOPE (DEG)
;;   17 DISTANCE TO THE CIVIL PILE  18 MIN COTT  19 FLOOD DEPTH  20 CUT(-) / FILL(+)  21 COTT CHECK, 12 - 21 ARE NIL WHEN UNKNOWN

(defun PPP:PileXdata ( pd / )
	(list -3
		(append
			(list "PPPPILE"
				(cons 1000 (nth 0 pd))
				(cons 1071 (nth 1 pd))
				(cons 1071 (nth 2 pd))
				(cons 1000 (nth 3 pd))
				(cons 1000 (nth 4 pd))
				(cons 1000 (nth 5 pd))
				(cons 1000 (nth 6 pd))
				(cons 1071 (nth 7 pd))
				(cons 1040 (nth 8 pd))
				(cons 1040 (nth 9 pd))
				(cons 1005 (nth 10 pd))
			)
			(if (nth 11 pd)
				(cons
					(cons 1000 (nth 11 pd))
					(append
						(mapcar '(lambda ( idx / val ) (if (numberp (setq val (nth idx pd))) (cons 1040 val) (cons 1000 ""))) (list 12 13 14 15 16 17 18 19 20)) ;; "" KEEPS THE POSITIONS WHEN A VALUE IS MISSING
						(list (cons 1000 (cond ((nth 21 pd)) (""))))
					)
				)
			)
		)
	)
)
;; RETURNS THE 'PPPPILE' XDATA ( -3 ( "PPPPILE" ... ) ) FOR A PILE LABEL FROM PILE DATA [ pd ], THE HEIGHTS ARE ONLY WRITTEN WHEN THE PILE HAS THEM

(defun PPP:PileData ( ent / xdl out )
	(if (setq xdl (PPP:XdataValues ent "PPPPILE"))
		(progn
			(repeat 11
				(setq out (cons (cdar xdl) out) xdl (cdr xdl))
			)
			(if xdl
				(progn
					(setq out (cons (cdar xdl) out) xdl (cdr xdl)) ;; HEIGHTS SOURCE
					(repeat 9
						(setq out (cons (if (= 1040 (caar xdl)) (cdar xdl)) out) xdl (cdr xdl))
					) ;; A 1000 "" MARKS A MISSING VALUE
					(setq out (cons (cdar xdl) out))
				)
				(repeat 11
					(setq out (cons nil out))
				) ;; NO HEIGHTS
			)
			(reverse out)
		)
	)
)
;; RETURNS THE PILE DATA STORED ON A PILE LABEL (SEE THE LIST ABOVE), NIL IF [ ent ] IS NOT A PPP PILE LABEL
;; THE REVERSE OF PPP:PileXdata, LABELS FROM EARLIER RUNS (SAME XDATA LAYOUT) READ THE SAME WAY

(defun PPP:PileLabels ( doc / pd out )
	(vlax-for obj (vla-get-modelspace doc)
		(if (and (= "AcDbMText" (vla-get-objectname obj)) (setq pd (PPP:PileData (vlax-vla-object->ename obj))))
			(setq out (cons (cons (vlax-vla-object->ename obj) pd) out))
		)
	)
	out
)
;; RETURNS ( ENAME . PILE-DATA ) FOR EVERY PILE LABEL IN MODEL SPACE (MTEXT WITH 'PPPPILE' XDATA), SHARED BY PPPPILES (CLEARING) AND PPPBOM

(defun PPP:HeightsSource ( cnt / )
	(cond
		((assoc "ESTIMATE" cnt) "ESTIMATE")
		((assoc "CIVIL" cnt) "CIVIL")
	)
)
;; RETURNS THE HEIGHTS SOURCE OF A WHOLE RUN FROM THE PER PILE COUNTS: "ESTIMATE" IF ANY PILE IS ESTIMATED, "CIVIL" IF ANY HAS CIVIL DATA, ELSE NIL
;; ONE ESTIMATED PILE MAKES THE BOM AN ESTIMATE (ITS OWN FILE NAME, SEE PPP:WriteBom), SO A BOM WITH ANY NON-CIVIL HEIGHT IS NEVER ISSUED BY MISTAKE
;; [ cnt ] == ( ( "CIVIL" . N ) ( "ESTIMATE" . N ) ( "NONE" . N ) ) PILE COUNTS (std:AssocIncrement ON PILE DATA ITEM 11, "NONE" WITHOUT HEIGHTS)

(defun PPP:HeightsWarning ( cnt pop / est civ non ttl lst )
	(setq
		est (cdr (assoc "ESTIMATE" cnt))
		civ (cdr (assoc "CIVIL" cnt))
		non (cdr (assoc "NONE" cnt))
		ttl (apply '+ (mapcar 'cdr cnt))
	)
	(if est
		(setq lst
			(list
				(if civ
					(strcat (itoa est) " of " (itoa ttl) " piles have no Civil grading data, their heights are ESTIMATED from the Civil topo (existing grade, no grading).")
					"Pile heights are ESTIMATED from the Civil topo (existing grade, no grading), they are NOT Civil grading data."
				)
				"The BOM is saved as \"PPP BOM (CONTOUR ESTIMATE)\", estimated rows are marked \"ESTIMATE - NOT CIVIL\" and their labels \"(est)\"."
				(strcat "Do not issue it. Add the Civil slope tool output" (if civ " that covers every pile" "") " and run PPPPILES again.")
			)
		)
	)
	(cond
		(pop
			(if lst (json:StringJoin lst "\n\n"))
		)
		( t
			(strcat
				(if lst
					(strcat
						"\n  ******************************************************************************"
						"\n  WARNING: " (car lst)
						(apply 'strcat (mapcar '(lambda ( str ) (strcat "\n  " str)) (cdr lst)))
						"\n  ******************************************************************************"
					)
					""
				)
				(if non (strcat "\n  Warning: " (itoa non) " piles have no heights (no Civil pile or topo contour near them), their height columns are blank.") "")
			)
		)
	)
)
;; RETURNS THE HEIGHTS WARNING FOR A RUN FROM THE PER PILE COUNTS (SEE PPP:HeightsSource), SHARED BY PPPPILES AND PPPBOM SO THE WARNING NEVER DIFFERS
;; [ pop ] == NIL FOR COMMAND LINE TEXT ("" WHEN ALL PILES HAVE CIVIL HEIGHTS), T FOR POPUP TEXT (NIL WHEN NOTHING IS ESTIMATED)

(defun PPP:PileTypeName ( cls / )
	(cdr
		(assoc cls
			(list
				(cons "HMP" "Heavy Motor")
				(cons "HAP" "Heavy Array")
				(cons "SMP" "Standard Motor")
				(cons "SAP" "Standard Array")
				(cons "SAPE" "Standard Array Edge")
				(cons "SAPEND" "Standard Array End")
			)
		)
	)
)
;; RETURNS THE BOM PILE TYPE NAME FOR A PILE CLASS (NEXTRACKER NAMES, AS USED IN THE ORCA FALLS BOM), NIL FOR AN UNKNOWN CLASS
;; !REVISIT! MOVES TO THE MANUFACTURER MAPPING WITH THE PILE SCHEDULE (PPP-SCHEDULE.json)

(defun PPP:TrackerRecommendation ( pds / )
	(cond
		((not (vl-some '(lambda ( pd ) (nth 11 pd)) pds)) "")
		((vl-some '(lambda ( pd ) (member (nth 21 pd) (list "LOW" "LOW_PER_FLOOD" "HIGH"))) pds) "GRADE")
		((vl-some '(lambda ( pd ) (member (nth 21 pd) (list "" nil))) pds) "")
		("OK")
	)
)
;; RETURNS THE TABLE RECOMMENDATION FOR ONE TRACKER: "GRADE" IF ANY PILE IS OUT OF ITS WINDOW, "OK" IF ALL PILES ARE IN, "" WITHOUT HEIGHTS
;; [ pds ] == PILE DATA OF ALL THE TRACKER'S PILES

(defun PPP:BomHeader ( / )
	(list
		"Pile Label" "Table" "Pole" "Area" "Preset type" "Row type" "X (ft)" "Y (ft)" "Z terrain enter (ft)"
		"Z frame attach (ft)" "COTT" "Pile Reveal (ft)" "Flood Depth (ft)" "Table slope (deg)" "Pile type"
		"Min COTT per Flood Depth (ft)" "COTT OK?" "PILE RECOMMENDATION" "TABLE RECOMMENDATION" "Pile Section"
		"Pile Type section" "Length (FT)" "Color" "Weight (lbs) (Aprox)" "CUT(-)/FILL(+) (ft)" "Z terrain Proposed (ft)"
	)
)
;; RETURNS THE 26 BOM COLUMN HEADERS (THE PPP SPREADSHEET), THE LAST TWO MATCH THE CULPEPPER PRELIMINARY BOM

(defun PPP:BomRow ( pd tre / sts tag zt gr )
	(setq
		sts (cond ((nth 21 pd)) (""))
		tag (if (= "ESTIMATE" (nth 11 pd)) " (ESTIMATE - NOT CIVIL)" "")
		zt (nth 12 pd)
		gr (nth 20 pd)
	)
	(append
		(list (nth 0 pd) (nth 1 pd) (nth 2 pd) (nth 3 pd) (nth 4 pd) (nth 5 pd) (nth 8 pd) (nth 9 pd))
		(mapcar '(lambda ( idx / val ) (if (numberp (setq val (nth idx pd))) val "")) (list 12 13 14 15 19 16)) ;; Z ENTER, Z FRAME, COTT, REVEAL, FLOOD, SLOPE
		(list
			(cond ((PPP:PileTypeName (nth 6 pd))) (""))
			(if (numberp (nth 18 pd)) (nth 18 pd) "")
			sts
			(cond ((= "" sts) "") ((= "OK" sts) (strcat "GOOD" tag)) ((strcat "GRADE" tag)))
			(if (= "" tre) "" (strcat tre tag))
			"" "" "" "" "" ;; !REVISIT! PILE SECTION, LENGTH, COLOR, WEIGHT NEED THE PILE SCHEDULE AND SOIL ZONES
			(if (numberp gr) gr "")
			(if (and (numberp zt) (numberp gr)) (+ zt gr) "")
		)
	)
)
;; RETURNS ONE BOM ROW (26 VALUES, "" FOR AN EMPTY CELL) FROM PILE DATA [ pd ] AND ITS TRACKER'S RECOMMENDATION [ tre ] (PPP:TrackerRecommendation)
;; AN ESTIMATE MARKS BOTH RECOMMENDATION COLUMNS "(ESTIMATE - NOT CIVIL)", SO A ROW COPIED OUT OF THE FILE STILL SAYS SO

(defun PPP:WriteBom ( ppp cfg rws upd hds src / Export dir pfx hdr fls lck msg ok )

	(defun Export ( itm / pth res )
		(setq pth (car itm))
		(PPP:DebugStep "PPPBOM" (strcat "writing " (cadr itm) " (Excel)"))
		(setq res
			(msxl:ExportTable
				pth
				(nth 2 itm)
				(nth 3 itm)
				(if (nth 4 itm) (mapcar '(lambda ( i ) (nth i hdr)) (nth 4 itm)) hdr)
				(if (nth 4 itm) (mapcar '(lambda ( r ) (mapcar '(lambda ( i ) (nth i r)) (nth 4 itm))) rws) rws)
				(list (cons "A:A" "@") (cons "D:D" "@") (cons "G:H" "0.000")) ;; TEXT COLUMNS FIRST, SO "1-1" IS NOT READ AS A DATE
			)
		)
		(if (= t res)
			(strcat "\n  " (cadr itm) " saved to: " pth)
			(progn
				(setq ok nil)
				(strcat "\n  Warning: " (cadr itm) " was not saved (" res "): " pth)
			)
		)
	)
	;; WRITES ONE OUTPUT WORKBOOK ( PATH NAME SHEET HEADER-ROW COLUMNS ), RETURNS A SUMMARY LINE (SAVED OR WHY NOT)

	(setq
		ok t
		hdr (PPP:BomHeader)
		dir (cond
				((std:CreateDirectoryPath (strcat ppp "\\2. BOM Processor")))
				(ppp)
			)
		pfx (cond ((cdr (assoc "project_number" cfg))) ("PPP"))
		fls
			(vl-remove 'nil
				(list
					(if (= "1" upd)
						(list (if hds "PILE DRIVE (SELECTED AREAS)" "PILE DRIVE") "PILE DRIVE" 1 (list 0 1 2 3 4 5 6 7 14))
					) ;; DATA ONLY: LABEL, TABLE, POLE, AREA, PRESET, ROW TYPE, X, Y, PILE TYPE
					;; !REVISIT! PILE DRIVE FILE: ADD PILE REVEAL / Z COLUMNS? FORMAT FOR THE GPS PILE DRIVER NOT CONFIRMED
					(list
						(cond ((= "ESTIMATE" src) "PPP BOM (CONTOUR ESTIMATE)") (hds "PPP BOM (SELECTED AREAS)") ("PPP BOM")) ;; A DIFFERENT FILE, SO AN ESTIMATE OR A PARTIAL RUN NEVER REPLACES THE FULL CIVIL BOM
						(strcat (if (= "ESTIMATE" src) "BOM ESTIMATE - " "BOM - ") pfx)
						20 ;; ROWS 1-19 ARE LEFT FOR THE PILE SCHEDULE SUMMARY
						nil
					)
					;; !REVISIT! BOM FORMATTING (COLORS, TEXT STYLE, SUMMARY, HEADER ROW) DIFFERS BY PROJECT (ORCA ROW 20, MCCRACKEN ROW 60 WITH SOIL ZONE / INVERTER COLUMNS), NEEDS A TEMPLATE
				)
			)
		fls (mapcar '(lambda ( itm ) (cons (strcat dir "\\" pfx " - " (car itm) ".xlsx") itm)) fls) ;; ( PATH NAME SHEET HEADER-ROW COLUMNS )
		lck (vl-remove-if-not '(lambda ( itm ) (std:IsFileLocked (car itm))) fls)
	)
	(cond
		((not (std:FunctionLoaded 'msxl:ExportTable))
			(list nil "\n  Warning: the BOM and pile drive file were not saved, LIB-MSXL is not loaded.")
		)
		(lck
			(list nil
				(apply 'strcat
					(mapcar '(lambda ( itm ) (strcat "\n  Warning: " (cadr itm) " was not saved, the file is open (close it in Excel): " (car itm))) lck)
				)
			)
		) ;; NOTHING IS WRITTEN WHILE ANY OUTPUT FILE IS OPEN, SO THE BOM AND THE PILE DRIVE FILE NEVER COME FROM DIFFERENT RUNS
		( t
			(list
				(progn (setq msg (apply 'strcat (mapcar 'Export fls))) ok)
				(strcat msg (if (= "1" upd) "" "\n  Pile drive file: turned off in the options"))
			)
		)
	)
)
;; WRITES THE PILE DRIVE FILE (IF [ upd ] == "1") AND THE BOM TO THE BOM PROCESSOR FOLDER, BOTH REPLACED EVERY TIME
;; RETURNS ( SAVED SUMMARY ): SAVED == T WHEN EVERY FILE WAS WRITTEN, SUMMARY == COMMAND LINE TEXT (SAVED OR WHY NOT, ONE LINE PER FILE)
;; OPEN (LOCKED) FILES ARE CHECKED FIRST, NOTHING IS WRITTEN UNTIL THEY ARE CLOSED
;; [ ppp ] == PPP-BOM ROOT, [ cfg ] == PROJECT SETTINGS, [ rws ] == BOM ROWS (PPP:BomRow), [ hds ] == T FOR A SELECTED AREAS RUN (FILE NAMES)
;; [ src ] == HEIGHTS SOURCE ("CIVIL" / "ESTIMATE" / NIL), AN ESTIMATE GETS ITS OWN FILE NAME


;;; PPP SHARED FUNCTIONS - DIALOGS ;;;

(defun PPP:DialogLoad ( lst pfx / tmp des dch )
	(if (and lst (setq tmp (vl-filename-mktemp pfx nil ".dcl")) (setq des (open tmp "w")))
		(progn
			(foreach line lst
				(write-line line des)
			)
			(close des)
			(if (> (setq dch (load_dialog tmp)) 0)
				(cons tmp dch)
				(progn
					(vl-file-delete tmp)
					nil
				)
			)
		)
	)
)
;; WRITES INLINE DCL LINES [ lst ] TO A TEMPORARY FILE NAMED WITH PREFIX [ pfx ] AND LOADS IT
;; RETURNS ( TEMP-FILE . DIALOG-HANDLE ), NIL IF THE FILE COULD NOT BE WRITTEN OR LOADED

(defun PPP:DialogUnload ( tmp dch / )
	(if (and dch (< 0 dch))
		(unload_dialog dch)
	)
	(if (and (= 'STR (type tmp)) (findfile tmp))
		(vl-file-delete tmp)
	)
	nil
)
;; UNLOADS A DIALOG AND DELETES ITS TEMPORARY DCL FILE, SAFE TO CALL TWICE OR WITH NIL VALUES
;; RETURNS NIL, SO (setq dch (PPP:DialogUnload tmp dch)) ALSO CLEARS THE HANDLE

(defun PPP:ArrayBlockPicker ( rws cfg / DCL:Row DCL:Default DCL:Matches DCL:Pick DCL:Summary DCL:Accept ttl dcl tmp dch dlg sts sel res )
	(defun DCL:Row ( itm / )
		(strcat
			(car itm) "\t"
			(cond
				((= 0 (caddr itm)) (itoa (cadr itm)))
				((= 0 (cadr itm)) (strcat (itoa (caddr itm)) " in groups"))
				( t (strcat (itoa (cadr itm)) " + " (itoa (caddr itm)) " in groups"))
			) "\t"
			(if (nth 3 itm) (strcat (rtos (nth 3 itm) 2 1) " x " (rtos (nth 4 itm) 2 1)) "not measurable") "\t"
			(if (nth 5 itm) (json:StringJoin (nth 5 itm) " / ") "-")
		)
	)
	;; LIST BOX ROW: NAME, COUNT, SIZE (LENGTH x WIDTH), MATCHING TRACKERS
	
	(defun DCL:Matches ( fnc / pos out )
		(setq pos -1)
		(foreach itm rws
			(setq pos (1+ pos))
			(if (apply fnc (list itm))
				(setq out (cons pos out))
			)
		)
		(reverse out)
	)
	;; RETURNS THE ROW INDEXES PASSING [ fnc ]
	
	(defun DCL:Default ( / sav pat )
		(setq
			sav (PPP:ArrayNames cfg)
			pat (strcase (PPP:SettingText cfg "array_blocks" ""))
		)
		(cond
			((and sav (setq sav (DCL:Matches '(lambda ( itm ) (member (strcase (car itm)) sav)))))
				sav
			) ;; THE ROW INDEXES, AUTOLISP 'and' ONLY RETURNS T
			((DCL:Matches '(lambda ( itm ) (or (nth 5 itm) (wcmatch (strcase (car itm)) pat)))))
		)
	)
	;; STARTING SELECTION: THE NAMES PICKED LAST TIME IF ANY ARE HERE, ELSE BLOCKS MATCHING A PRESET SIZE OR THE OLD NAME PATTERN
	
	(defun DCL:Pick ( lst / )
		(setq sel lst)
		(set_tile "blocks" (json:StringJoin (mapcar 'itoa sel) " "))
		(DCL:Summary)
	)
	;; SETS THE SELECTED ROWS
	
	(defun DCL:Summary ( / blk grp )
		(setq blk 0 grp 0)
		(foreach pos sel
			(setq
				blk (+ blk (cadr (nth pos rws)))
				grp (+ grp (caddr (nth pos rws)))
			)
		)
		(set_tile "summary"
			(strcat
				(itoa (length sel)) " of " (itoa (length rws)) " block types selected, " (itoa (+ blk grp)) " array blocks"
				(if (> grp 0) (strcat " (" (itoa grp) " in group blocks, these are exploded)") "")
			)
		)
	)
	;; DISPLAYS THE SELECTED BLOCK TYPES AND BLOCK COUNTS
	
	(defun DCL:Accept ( / )
		(if sel
			(progn
				(setq res (mapcar '(lambda ( pos ) (car (nth pos rws))) sel))
				(done_dialog 1)
			)
			(set_tile "error" "Error: Select at least one tracker block.")
		)
	)
	;; RETURNS THE SELECTED BLOCK NAMES IN 'res' AND CLOSES THE DIALOG
	
	(setq ttl "PPP_Blocks")
	(setq dcl
		(list
			"// Temporary DCL file;"
			(strcat ttl " : dialog {")
			"	label = \"Pile Plot Plan - Array Blocks\";"
			"	: text { label = \"Select the tracker blocks. Blocks matching a tracker preset size are selected.\"; }"
			"	: list_box { label = \"Block name / Count / Size (ft) / Preset match\"; key = \"blocks\"; multiple_select = true; width = 110; height = 14; tabs = \"46 70 88\"; }"
			"	: row {"
			"		: button { label = \"Select matches\"; key = \"matches\"; width = 18; fixed_width = true; }"
			"		: button { label = \"Clear\"; key = \"clear\"; width = 12; fixed_width = true; }"
			"	}"
			"	: text { key = \"summary\"; width = 110; }"
			"	spacer_1;"
			"	errtile;"
			"	ok_cancel;"
			"}"
		)
	)
	(if (setq dlg (PPP:DialogLoad dcl "PPPBLOCKS"))
		(progn
			(setq
				tmp (car dlg)
				dch (cdr dlg)
			)
			(if (new_dialog ttl dch)
				(progn
					(PPP:FillList "blocks" (mapcar 'DCL:Row rws) nil)
					(DCL:Pick (DCL:Default))
					
					(action_tile "blocks" "(setq sel (read (strcat \"(\" $value \")\"))) (DCL:Summary)")
					(action_tile "matches" "(DCL:Pick (DCL:Matches '(lambda ( itm ) (nth 5 itm))))")
					(action_tile "clear" "(DCL:Pick nil)")
					(action_tile "accept" "(DCL:Accept)")
					(action_tile "cancel" "(done_dialog 0)")
					
					(setq sts (start_dialog))
				)
				(prompt "\nError loading dialog box.")
			)
			(setq dch (PPP:DialogUnload tmp dch))
		)
		(prompt "\nError loading dialog box.")
	)
	(if (= 1 sts) res)
)
;; DISPLAYS THE ARRAY BLOCKS DIALOG: EVERY BLOCK TYPE ON THE ARRAY LAYERS WITH ITS COUNT, SIZE, AND MATCHING TRACKER PRESETS
;; THE USER TICKS THE TRACKER BLOCKS, THE CALLER SAVES THE EXACT NAMES AS "array_block_names" (SEE PPP:IsArrayName)
;; SO NOBODY NEEDS TO OPEN X-ARRAY TO LOOK UP BLOCK NAMES
;; [ rws ] == RESULT OF PPP:ScanArrayBlocks, [ cfg ] == PROJECT SETTINGS (LAST PICKED NAMES AND THE OLD NAME PATTERN)
;; RETURNS THE SELECTED BLOCK NAMES, NIL IF CANCELLED
;; 'tmp' / 'dch' ARE NAMED LIKE THE CALLERS' SO THEIR *error* (DYNAMIC SCOPE) UNLOADS THIS DIALOG IF ESC OR AN ERROR HITS

(defun PPP:SettingsDialog
	(
		cfg prs dir lck
		/
		;Functions
		CottRange PresetText PacksToRows RowsToPacks PacksString PairMismatch ImportPresetFile
		;Variables
		def shw sts dlg
		;DCL Functions
		DCL:RunDialogChecks DCL:TileValue DCL:TileNumber
		DCL:UpdateCottRange DCL:UpdateSlope DCL:UpdatePresetInfo
		DCL:PresetsDialog DCL:PresetRefresh DCL:PresetFill DCL:PresetModes DCL:PresetRead DCL:PresetCommit
		DCL:PresetSelect DCL:PresetNew DCL:PresetDuplicate DCL:PresetPair DCL:PresetDelete DCL:PresetImport
		DCL:PresetSummary DCL:PresetAccept DCL:PresetDraft DCL:PileGeometry DCL:CenterMotor DCL:SpacingEnd
		DCL:LayoutModules DCL:LayoutNextRow DCL:LayoutRefresh DCL:LayoutModes DCL:LayoutChanged DCL:LayoutSelect
		DCL:LayoutBaySize DCL:LayoutNoJoint DCL:LayoutAdd DCL:LayoutDelete DCL:LayoutMove DCL:LayoutFlip
		DCL:PackSizes DCL:JointSpacing DCL:SpacingRows DCL:SpacingRefresh DCL:SpacingModes DCL:SpacingChanged DCL:SpacingMode
		DCL:SpacingSelect DCL:SpacingValue DCL:SpacingAdd DCL:SpacingDelete
		;DCL Variables
		ttl dcl tmp des dch res keys pops wrk idx mfr lay lix spc six bsz cps jte
	)
	
	(defun CottRange ( lst stp / srt )
		(if (and lst (numberp stp) (> stp 1e-8))
			(progn
				(setq srt (vl-sort lst '<))
				(list (- (car srt) stp) (last srt) stp)
			)
		)
	)
	;; CALCULATES THE COTT RANGE FROM FOUNDATION SCHEDULE BUCKET HEIGHTS AND THE BUCKET STEP (EDITABLE PER PROJECT, DEFAULT 0.5 FT)
	;; MINIMUM = LOWEST BUCKET MINUS THE BUCKET STEP, MAXIMUM = HIGHEST BUCKET (EX. 6.0, 6.5, 7.0 WITH STEP 0.5 -> 5.5 TO 7.0)
	;; RETURNS ( MIN MAX STEP ), NIL IF NO BUCKETS ARE PASSED OR THE STEP IS NOT GREATER THAN ZERO
	
	(defun PresetText ( val / )
		(if (= 'STR (type val))
			val
			""
		)
	)
	;; RETURNS THE VALUE IF IT IS A STRING, ELSE ""
	
	(defun PacksToRows ( pck / prv pos out )
		(setq pos -1)
		(foreach itm pck
			(setq pos (1+ pos))
			(cond
				((= "Modules" (car itm))
					(if (= "Modules" prv)
						(setq out (cons (list "B" (cadr (car out)) t) (cdr out)))
					) ;; BACK TO BACK MODULE PACKS, THE PREVIOUS BAY HAS NO JOINT GAP AFTER IT
					(setq out (cons (list "B" (cdr itm) nil) out))
				)
				((= "JointGap" (car itm))
					(repeat
						(if (and (= "Modules" prv) (= "Modules" (car (nth (1+ pos) pck))))
							(1- (cdr itm)) ;; ONE JOINT GAP BETWEEN TWO BAYS IS IMPLIED, ONLY EXTRA ONES GET A ROW
							(cdr itm)
						)
						(setq out (cons (list "J" 1 nil) out))
					)
				)
				( t
					(repeat (cdr itm)
						(setq out (cons (list "M" 1 nil) out))
					)
				)
			)
			(setq prv (car itm))
		)
		(reverse out)
	)
	;; CONVERTS PACKS (SEE PPP:ParsePacks) INTO MODULE LAYOUT ROWS FOR THE PRESETS DIALOG, NORTH TO SOUTH
	;; EACH ROW IS ( KIND COUNT NO-JOINT ): ("B" 6 nil) == BAY OF 6 MODULES, ("M" 1 nil) == MOTOR, ("J" 1 nil) == EXTRA JOINT GAP
	;; A JOINT GAP BETWEEN TWO BAYS IS IMPLIED, NO-JOINT == T MARKS A BAY WITH NO JOINT GAP AFTER IT (PVCASE BACK TO BACK MODULE PACKS)
	
	(defun RowsToPacks ( rows / AddPack lst prv )
		(defun AddPack ( typ cnt / )
			(if (and (/= "Modules" typ) (= typ (car (car lst))))
				(setq lst (cons (cons typ (+ cnt (cdr (car lst)))) (cdr lst)))
				(setq lst (cons (cons typ cnt) lst))
			)
		)
		;; ADDS A PACK TO THE FRONT OF 'lst', BACK TO BACK JOINT GAPS OR MOTORS ARE COMBINED INTO ONE COUNT
		
		(foreach row rows
			(cond
				((= "B" (car row))
					(if (and (= "B" (car prv)) (not (caddr prv)))
						(AddPack "JointGap" 1)
					) ;; IMPLIED JOINT GAP BETWEEN TWO BAYS (EXTRA JOINT ROWS IN BETWEEN ADD TO IT)
					(AddPack "Modules" (cadr row))
					(setq prv row)
				)
				((= "M" (car row))
					(AddPack "Motor" 1)
					(setq prv row)
				)
				( t
					(AddPack "JointGap" 1) ;; EXTRA JOINT ROWS DO NOT CHANGE WHICH BAY OR MOTOR CAME BEFORE
				)
			)
		)
		(reverse lst)
	)
	;; CONVERTS MODULE LAYOUT ROWS (SEE PacksToRows) BACK INTO PACKS, THE STORED FORMAT IS UNCHANGED
	
	(defun PacksString ( pck / )
		(json:StringJoin (mapcar '(lambda ( itm ) (strcat (car itm) "," (itoa (cdr itm)))) pck) "; ")
	)
	;; CONVERTS PACKS INTO A PVCASE PACKS STRING (EX. "Modules,6; Motor,1; Modules,6")
	
	(defun PairMismatch ( lst / par )
		(vl-some
			'(lambda ( pre )
				(if (and
						(setq par (PPP:FindPreset lst (PPP:PairName (json:Get pre "Name"))))
						(not (PPP:PresetGeometryEqual pre par))
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
								(PPP:PresetNormalize (json:Object
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
								)) ;; NORMALIZED SO THE SOUTH END DISTANCE IS THE DIFFERENCE, PILE POSITIONS DO NOT CHANGE
								lst
							)
						)
					)
				)
				(list (reverse lst) uns)
			)
			((json:Has dat "Presets")
				(list (mapcar 'PPP:PresetNormalize (json:Items (json:Get dat "Presets"))) 0)
			)
			( t
				(strcat src " does not contain PVcase templates or PPP presets.")
			)
		)
	)
	;; READS PRESETS FROM A PVCASE .PVsettings FILE OR ANOTHER PPP PRESET FILE (EX. A SHARED REPOSITORY FILE)
	;; PVCASE HAS NO TRACKER MANUFACTURER FIELD, [ mfr ] IS USED FOR IMPORTED TEMPLATES
	;; RETURNS ( PRESETS UNSUPPORTED-COUNT ), OR A MESSAGE STRING IF THE FILE COULD NOT BE READ
	
	(setq ttl "PPP_Setup")
	(setq
		def (PPP:DefaultSettings dir)
		keys (mapcar 'car def) ;; THE DEFAULT KEYS ARE THE TILE KEYS, OTHER SETTINGS (EX. "civil_output") ARE KEPT BUT NOT SHOWN
		shw (std:AssocMerge cfg def) ;; SAVED VALUES OVER THE DEFAULTS
	)
	(setq pops
		(list
			(list "milestone" "30%" "60%" "90%" "IFC")
			(list "units" "Feet" "US Survey Feet")
			(list "manufacturer" "Nextracker" "Array Technologies (ATI)" "Other")
			(list "anchor_pile" "Motor" "North" "South")
		)
	) ;; POPUP LIST TILE KEYS AND THEIR DISPLAYED VALUES
	(setq dcl
		(list
			"// Temporary DCL file;"
			(strcat ttl " : dialog {")
			(strcat "	label = \"" (if lck "Pile Plot Plan Settings" "Pile Plot Plan Setup Dialog Control") "\";")
			"	: row {"
			"		: boxed_column {"
			"			label = \"Project\";"
			"			: edit_box { label = \"Project number:\"; key = \"project_number\"; edit_width = 28; }"
			"			: edit_box { label = \"Project name:\"; key = \"project_name\"; edit_width = 28; }"
			"			: popup_list { label = \"Milestone:\"; key = \"milestone\"; edit_width = 26; }"
			"			: edit_box { label = \"Drawing name:\"; key = \"file_name\"; edit_width = 28; }"
			"			: popup_list { label = \"Drawing units:\"; key = \"units\"; edit_width = 26; }"
			"			: edit_box { label = \"Contour layers:\"; key = \"contour_layers\"; edit_width = 28; }"
			"			: edit_box { label = \"Array layers:\"; key = \"array_layers\"; edit_width = 28; }"
			"			: edit_box { label = \"Array block pattern:\"; key = \"array_blocks\"; edit_width = 28; }"
			"		}"
			"		: boxed_column {"
			"			label = \"Tracker\";"
			"			: popup_list { label = \"Manufacturer:\"; key = \"manufacturer\"; edit_width = 26; }"
			"			: popup_list { label = \"Anchor pile (fixed when tilted):\"; key = \"anchor_pile\"; edit_width = 12; }"
			"			: edit_box { label = \"Exterior rows per side:\"; key = \"exterior_rows\"; edit_width = 10; }"
			"			: edit_box { label = \"Stagger allowance (ft):\"; key = \"stagger_allowance\"; edit_width = 10; }"
			"			: text { label = \"End offset allowed between neighboring arrays.\"; }"
			"			: text { label = \"0 = any offset is an edge (staggered layouts only).\"; }"
			"			: edit_box { label = \"Max table slope (%):\"; key = \"table_slope\"; edit_width = 10; }"
			"			: text { key = \"slope_deg\"; width = 36; }"
			"			: row {"
			"				: text { key = \"presets_info\"; width = 30; }"
			"				: button { label = \"Presets...\"; key = \"presets\"; fixed_width = true; }"
			"			}"
			"		}"
			"	}"
			"	: boxed_column {"
			"		label = \"COTT Range\";"
			"		: edit_box { label = \"Foundation schedule bucket heights (ft):\"; key = \"cott_buckets\"; edit_width = 24; fixed_width = true; }"
			"		: text { key = \"cott_range\"; width = 80; }"
			"		: row {"
			"			: boxed_column {"
			"				label = \"Window\";"
			"				: edit_box { label = \"Bucket step (ft):\"; key = \"bucket_step\"; edit_width = 7; }"
			"				: edit_box { label = \"Tolerance (ft):\"; key = \"tolerance\"; edit_width = 7; }"
			"				: edit_box { label = \"Decimals:\"; key = \"decimals\"; edit_width = 7; }"
			"			}"
			"			: boxed_column {"
			"				label = \"Flood\";"
			"				: edit_box { label = \"Allowed flood (ft):\"; key = \"allowed_flood\"; edit_width = 7; }"
			"				: edit_box { label = \"Freeboard (ft):\"; key = \"flood_freeboard\"; edit_width = 7; }"
			"				: edit_box { label = \"Edge clearance (ft):\"; key = \"edge_clearance\"; edit_width = 7; }"
			"			}"
			"			: boxed_column {"
			"				label = \"COTT to Pile Top\";"
			"				: edit_box { label = \"Array piles (ft):\"; key = \"cott_reveal_offset\"; edit_width = 7; }"
			"				: edit_box { label = \"Motor pile (ft):\"; key = \"cott_reveal_offset_motor\"; edit_width = 7; }"
			"				spacer;"
			"			}"
			"		}"
			"		: text { label = \"Flooded piles: COTT >= flood + freeboard + clearance + module drop.\"; }"
			"		: toggle { label = \"COTT range confirmed with engineer of record\"; key = \"eor_confirmed\"; }"
			"	}"
			"	: text { key = \"lock_note\"; width = 80; }"
			"	spacer_1;"
			"	errtile;"
			"	ok_cancel;"
			"}"
			"PPP_Presets : dialog {"
			"	label = \"Tracker Presets\";"
			"	: row {"
			"		: boxed_column {"
			"			label = \"Presets\";"
			"			: list_box { key = \"preset_list\"; width = 24; height = 22; }"
			"			: column {"
			"				fixed_height = true;"
			"				: row { fixed_height = true; : button { label = \"New\"; key = \"preset_new\"; } : button { label = \"Duplicate\"; key = \"preset_copy\"; } }"
			"				: row { fixed_height = true; : button { label = \"Copy to Pair\"; key = \"preset_pair\"; } : button { label = \"Delete\"; key = \"preset_delete\"; } }"
			"				: button { label = \"Import...\"; key = \"preset_import\"; fixed_height = true; }"
			"			}"
			"		}"
			"		: column {"
			"			: boxed_column {"
			"				label = \"Preset\";"
			"				: row { : edit_box { label = \"Name (INT / EXT):\"; key = \"Name\"; edit_width = 16; } : edit_box { label = \"Manufacturer:\"; key = \"Manufacturer\"; edit_width = 16; } }"
			"				: edit_box { label = \"Description:\"; key = \"Description\"; edit_width = 60; edit_limit = 256; }"
			"			}"
			"			: boxed_column {"
			"				label = \"Tracker (ft)\";"
			"				: row {"
			"					: column {"
			"						: edit_box { label = \"Modules per tracker:\"; key = \"Columns\"; edit_width = 8; }"
			"						: edit_box { label = \"Module length, E-W:\"; key = \"SizeX\"; edit_width = 8; }"
			"						: edit_box { label = \"Module width, N-S:\"; key = \"SizeY\"; edit_width = 8; }"
			"					}"
			"					: column {"
			"						: edit_box { label = \"Module gap:\"; key = \"ModuleGap\"; edit_width = 8; }"
			"						: edit_box { label = \"Joint gap:\"; key = \"JointGapSize\"; edit_width = 8; }"
			"						: edit_box { label = \"Motor gap:\"; key = \"MotorGapSize\"; edit_width = 8; }"
			"					}"
			"					: column {"
			"						: edit_box { label = \"Max tilt (deg):\"; key = \"TiltAngle\"; edit_width = 8; }"
			"						: edit_box { label = \"Overhang north:\"; key = \"overhang_north\"; edit_width = 8; }"
			"						: edit_box { label = \"Overhang south:\"; key = \"overhang_south\"; edit_width = 8; }"
			"					}"
			"				}"
			"				: text { label = \"Overhang = torque tube end to first / last module, south is calculated.\"; }"
			"			}"
			"			: boxed_row {"
			"				label = \"Layout (north to south)\";"
			"				: boxed_column {"
			"					label = \"Modules\";"
			"					: list_box { label = \"Bays, joint gaps, and motor:\"; key = \"layout_list\"; width = 32; height = 12; tabs = \"9\"; }"
			"					: text { key = \"layout_totals\"; width = 32; }"
			"					: row { : popup_list { key = \"layout_kind\"; width = 14; } : popup_list { label = \"Modules:\"; key = \"bay_size\"; edit_width = 5; } }"
			"					: row { : button { label = \"Add\"; key = \"layout_add\"; } : button { label = \"Delete\"; key = \"layout_delete\"; } }"
			"					: row { : button { label = \"Up\"; key = \"layout_up\"; } : button { label = \"Down\"; key = \"layout_down\"; } : button { label = \"Flip\"; key = \"layout_flip\"; } }"
			"					: toggle { label = \"No joint gap after this bay\"; key = \"no_joint\"; }"
			"				}"
			"				: boxed_column {"
			"					label = \"Piles\";"
			"					: toggle { label = \"Piles at every joint and the motor\"; key = \"pile_mode\"; }"
			"					: list_box { label = \"Pile spacing (ft):\"; key = \"spacing_list\"; width = 32; height = 10; tabs = \"16\"; }"
			"					: edit_box { label = \"Selected (ft or ft-in):\"; key = \"spacing_value\"; edit_width = 12; }"
			"					: row { : button { label = \"Add\"; key = \"spacing_add\"; } : popup_list { label = \"Copies:\"; key = \"spacing_copies\"; edit_width = 4; } : button { label = \"Delete\"; key = \"spacing_delete\"; } }"
			"					: text { label = \"Spacing runs from the north end of the torque tube.\"; }"
			"					: button { label = \"Center modules on motor pile\"; key = \"center_motor\"; }"
			"				}"
			"			}"
			"			: text { key = \"preset_summary\"; width = 100; }"
			"			: text { key = \"preset_lengths\"; width = 100; }"
			"			: text { key = \"preset_warning\"; width = 100; }"
			"		}"
			"	}"
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
				(setq num (std:ToNumber (get_tile key)))
				(>= num mn)
				(or (null mx) (<= num mx))
				(or (not int) (equal num (fix num) 1e-8))
			)
			num
		)
	)
	;; RETURNS THE NUMERIC VALUE OF AN EDIT BOX IF IT FALLS WITHIN [ mn ] TO [ mx ] (NIL MX = NO LIMIT), ELSE NIL
	;; [ int ] == T TO REQUIRE A WHOLE NUMBER
	
	(defun DCL:UpdateCottRange ( / lst rng gap )
		(setq
			lst (std:ParseNumberList (get_tile "cott_buckets"))
			rng (CottRange lst (std:ToNumber (get_tile "bucket_step")))
		)
		(set_tile "cott_range"
			(if rng
				(strcat
					"COTT range: " (rtos (car rng) 2 3) " to " (rtos (cadr rng) 2 3) " ft"
					(if (and
							(cdr (setq lst (vl-sort lst '<)))
							(not (equal (setq gap (apply 'min (mapcar '- (cdr lst) lst))) (caddr rng) 1e-8))
						)
						(strcat " (buckets " (rtos gap 2 3) " apart, step " (rtos (caddr rng) 2 3) ")")
						""
					)
				)
				"Enter bucket heights (comma separated) and a step above zero."
			)
		)
	)
	;; DISPLAYS THE CALCULATED COTT RANGE BELOW THE BUCKET HEIGHTS, NOTES WHEN THE STEP DIFFERS FROM THE BUCKET SPACING
	
	(defun DCL:UpdateSlope ( slo / num )
		(set_tile "slope_deg"
			(if (and (setq num (std:ToNumber slo)) (> num 0))
				(strcat "Max table slope = " (rtos (std:SlopeToDegrees num) 2 3) " degrees")
				"Enter table slope as a percent."
			)
		)
	)
	;; DISPLAYS THE MAX TABLE SLOPE CONVERTED TO DEGREES, [ slo ] == SLOPE TILE TEXT (PERCENT)
	
	(defun DCL:RunDialogChecks ( / lst rng )
		(cond
			((= "" (vl-string-trim " " (get_tile "project_number")))
				(set_tile "error" "Error: Project number cannot be blank.")
			)
			((not (std:ValidFileName (get_tile "file_name")))
				(set_tile "error" "Error: Drawing name is blank or contains invalid characters.")
			)
			((= "" (vl-string-trim " " (get_tile "contour_layers")))
				(set_tile "error" "Error: Contour layers cannot be blank.")
			)
			((= "" (vl-string-trim " " (get_tile "array_layers")))
				(set_tile "error" "Error: Array layers cannot be blank.")
			)
			((null (setq lst (std:ParseNumberList (get_tile "cott_buckets"))))
				(set_tile "error" "Error: Enter one or more foundation schedule bucket heights, separated by commas.")
			)
			((not (vl-every '(lambda ( x ) (> x 0)) lst))
				(set_tile "error" "Error: COTT bucket heights must be greater than zero.")
			)
			((null (setq rng (CottRange lst (DCL:TileNumber "bucket_step" 1e-6 nil nil))))
				(set_tile "error" "Error: Bucket step must be a number greater than zero.")
			)
			((not (vl-every '(lambda ( key ) (DCL:TileNumber key 0.0 nil nil)) (list "allowed_flood" "tolerance" "edge_clearance" "flood_freeboard" "cott_reveal_offset" "cott_reveal_offset_motor")))
				(set_tile "error" "Error: Flood, tolerance, clearance and COTT to pile top values must be numbers of zero or greater.")
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
							(cons "table_slope_deg" (std:SlopeToDegrees (DCL:TileNumber "table_slope" 1e-6 nil nil)))
						)
					)
				) ;; TILE VALUES MUST BE READ BEFORE DONE_DIALOG, CALCULATED VALUES ARE STORED FOR LATER STAGES
				(done_dialog 1)
			)
		)
	)
	;; RUNS INNER FUNCTION DIALOG BOX CHECKS FOR ERRORS, SETS 'res' TO THE SETTINGS LIST IF ALL CHECKS PASS
	
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
				(setq
					bsz (std:CountList 120) ;; BAY SIZES 1 TO 120 MODULES
					cps (std:CountList 20) ;; SPACING COPIES 1 TO 20
				)
				(PPP:FillList "layout_kind" (list "Bay" "Motor" "Extra joint gap") 0)
				(PPP:FillList "bay_size" bsz 6) ;; 7 MODULES, THE MOST COMMON BAY
				(PPP:FillList "spacing_copies" cps 0)
				;; INITIALIZE POPUP LISTS BEFORE A PRESET IS LOADED
				
				(DCL:PresetRefresh nil) ;; SELECTS THE FIRST PRESET IN SORTED ORDER
				
				(action_tile "preset_list" "(DCL:PresetSelect $value)")
				(action_tile "preset_new" "(DCL:PresetNew)")
				(action_tile "preset_copy" "(DCL:PresetDuplicate)")
				(action_tile "preset_pair" "(DCL:PresetPair)")
				(action_tile "preset_delete" "(DCL:PresetDelete)")
				(action_tile "preset_import" "(DCL:PresetImport)")
				(foreach key (list "Name" "SizeX" "SizeY" "ModuleGap" "JointGapSize" "MotorGapSize" "TiltAngle" "overhang_north")
					(action_tile key "(DCL:SpacingChanged)")
				) ;; LIVE SUMMARY (AND CALCULATED SPACING) WHEN A GEOMETRY VALUE CHANGES
				
				(action_tile "layout_list" "(DCL:LayoutSelect $value)")
				(action_tile "bay_size" "(DCL:LayoutBaySize $value)")
				(action_tile "no_joint" "(DCL:LayoutNoJoint $value)")
				(action_tile "layout_add" "(DCL:LayoutAdd)")
				(action_tile "layout_up" "(DCL:LayoutMove -1)")
				(action_tile "layout_down" "(DCL:LayoutMove 1)")
				(action_tile "layout_delete" "(DCL:LayoutDelete)")
				(action_tile "layout_flip" "(DCL:LayoutFlip)")
				;; MODULE LAYOUT LIST EDITOR
				
				(action_tile "pile_mode" "(DCL:SpacingMode $value)")
				(action_tile "spacing_list" "(DCL:SpacingSelect $value)")
				(action_tile "spacing_value" "(DCL:SpacingValue $value)")
				(action_tile "spacing_add" "(DCL:SpacingAdd)")
				(action_tile "spacing_delete" "(DCL:SpacingDelete)")
				(action_tile "center_motor" "(DCL:CenterMotor)")
				;; PILE SPACING LIST EDITOR
				(action_tile "accept" "(DCL:PresetAccept)")
				(action_tile "cancel" "(done_dialog 0)") ;; BACK TO THE SETTINGS DIALOG, THE DCL FILE IS UNLOADED WHEN THE SETTINGS DIALOG CLOSES
				
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
		(PPP:FillList "preset_list" (mapcar '(lambda ( pre ) (PresetText (json:Get pre "Name"))) wrk) nil)
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
	
	(defun DCL:PresetFill ( pre / hd ovn clc )
		(foreach key (list "Name" "Manufacturer" "Description")
			(set_tile key (PresetText (json:Get pre key)))
		)
		(foreach key (list "SizeX" "SizeY" "ModuleGap" "JointGapSize" "MotorGapSize" "TiltAngle")
			(set_tile key (std:NumberString (json:Get pre key)))
		)
		(setq
			lay (PacksToRows (PPP:ParsePacks (json:Get pre "Packs")))
			lix (if lay 0)
			hd (json:Items (json:Get pre "HorizontalDistances"))
			spc (if (PPP:PileSpacing pre) hd) ;; FULL LIST: NORTH END TO PILE 1, SPACING BETWEEN PILES, LAST PILE TO SOUTH END
			six (if spc 0)
			jte (if spc (list (car hd) (last hd)) (list 0.0 0.0)) ;; TORQUE TUBE END DISTANCES USED BY PILES AT EVERY JOINT
			ovn (json:Get pre "PilingOverhangLeftSize")
		)
		(set_tile "overhang_north" (if (numberp ovn) (std:NumberString ovn) "0"))
		(set_tile "pile_mode" "1")
		(set_tile "pile_mode"
			(if (and
					spc
					(numberp ovn)
					(cdr (setq clc (DCL:JointSpacing)))
					(equal (- (car hd) ovn) (car clc) (PPP:PresetTolerance)) ;; PILE 1 AT THE FIRST JOINT
					(= (length (cdr clc)) (length (PPP:PileSpacing pre)))
					(vl-every '(lambda ( a b ) (equal a b (PPP:PresetTolerance))) (mapcar '- (cdr clc) clc) (PPP:PileSpacing pre))
				)
				"1" ;; PRESETS WITH A PILE AT EVERY JOINT AND THE MOTOR USE THE CALCULATED SPACING
				"0" ;; EVERYTHING ELSE, INCLUDING NEW PRESETS, USES THE MANUFACTURER PILE SPACING AS ENTERED
			)
		) ;; SET TO "1" FIRST SO DCL:JointSpacing READS THE NUMBER TILES JUST FILLED
		(set_tile "error" "")
		(DCL:LayoutRefresh)
		(DCL:SpacingRefresh)
		(DCL:PresetSummary)
	)
	;; LOADS A PRESET INTO THE EDIT TILES AND THE LAYOUT / SPACING LISTS, NIL CLEARS THEM
	;; THE PILE MODE IS NOT STORED, A PRESET WHOSE PILES MATCH PILES AT EVERY JOINT OPENS IN THE CALCULATED MODE
	
	(defun DCL:PresetModes ( / mde )
		(setq mde (if idx 0 1))
		(foreach key
			(list
				"Name" "Manufacturer" "Description" "SizeX" "SizeY" "ModuleGap" "JointGapSize" "MotorGapSize" "TiltAngle"
				"overhang_north" "center_motor" "preset_copy" "preset_pair" "preset_delete"
				"layout_list" "layout_kind" "bay_size" "no_joint" "layout_add" "layout_up" "layout_down" "layout_delete" "layout_flip"
				"pile_mode" "spacing_list" "spacing_value" "spacing_add" "spacing_copies" "spacing_delete"
			)
			(mode_tile key mde)
		)
		(mode_tile "Columns" 1) ;; ALWAYS CALCULATED FROM THE MODULE LAYOUT
		(mode_tile "overhang_south" 1) ;; ALWAYS CALCULATED, THE DIFFERENCE (SEE PPP:TubeOverhangs)
		(if idx
			(progn
				(DCL:LayoutModes)
				(DCL:SpacingModes)
			)
		) ;; FINER ENABLING FOR THE LIST EDITORS (EX. ONLY ONE MOTOR)
	)
	;; DISABLES THE EDIT TILES AND PRESET BUTTONS WHILE NO PRESET IS SELECTED
	
	(defun DCL:PresetDraft ( / lbl nam num dup gmt )
		(setq lbl
			(list
				(cons "SizeX" "Module length")
				(cons "SizeY" "Module width")
				(cons "ModuleGap" "Module gap")
				(cons "JointGapSize" "Joint gap")
				(cons "MotorGapSize" "Motor gap")
				(cons "TiltAngle" "Max tilt")
			)
		) ;; NUMERIC TILE KEYS AND THEIR LABELS FOR ERROR MESSAGES
		(setq nam (vl-string-trim " " (get_tile "Name")))
		(if (PPP:RowPrefix nam)
			(setq nam (strcat (PPP:RowPrefix nam) (substr nam 4)))
		) ;; STANDARD UPPERCASE PREFIX, THE BOM READS THE ROW TYPE FROM IT, THE REST OF THE NAME (HYPHEN OR NOT) IS KEPT AS TYPED
		(setq num
			(mapcar
				'(lambda ( itm )
					(cons (car itm)
						(if (= "TiltAngle" (car itm))
							(std:ToNumber (get_tile (car itm)))
							(std:ParseLength (get_tile (car itm))) ;; LENGTHS ACCEPT FEET-INCHES
						)
					)
				)
				lbl
			)
		)
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
			((null (vl-some '(lambda ( row ) (= "B" (car row))) lay))
				"Module layout is empty, pick Bay and click Add to build the tracker north to south."
			)
			((and (= "0" (get_tile "pile_mode")) (null (std:ParseLength (get_tile "overhang_north"))))
				"Overhang north (torque tube end to the first module) must be a number."
			)
			((null (setq gmt (DCL:PileGeometry)))
				(if (= "1" (get_tile "pile_mode"))
					"Piles at every joint needs two or more joint gaps or a motor."
					"Pile spacing needs a north end, one or more spacings, and a south end."
				)
			)
			( t
				(PPP:PresetNormalize
					(json:Object
						(mapcar
							'(lambda ( key )
								(cons key
									(cond
										((= "Name" key) nam)
										((member key (list "Manufacturer" "Description")) (vl-string-trim " " (get_tile key)))
										((= "Columns" key) (DCL:LayoutModules nil)) ;; WHOLE NUMBER FROM THE LAYOUT
										((= "Packs" key) (PacksString (RowsToPacks lay)))
										((= "PilingOverhangLeftSize" key) (cadr gmt))
										((= "PilingOverhangRightSize" key) (caddr gmt)) ;; CALCULATED, THE DIFFERENCE
										((= "HorizontalDistances" key) (json:Array (car gmt))) ;; AS ENTERED, NEVER ADJUSTED
										( t (cdr (assoc key num)))
									)
								)
							)
							(PPP:PresetKeys)
						)
					)
				)
			)
		)
	)
	;; BUILDS A PRESET FROM THE EDIT TILES AND THE LAYOUT / SPACING LISTS WITHOUT CHECKING THE GEOMETRY (SEE DCL:PresetRead)
	;; RETURNS THE PRESET, OR A MESSAGE STRING IF A VALUE IS MISSING OR NOT A NUMBER
	
	(defun DCL:PresetRead ( / pre )
		(cond
			((= 'STR (type (setq pre (DCL:PresetDraft))))
				pre
			)
			((PPP:PresetProblem pre))
			(pre)
		)
	)
	;; BUILDS A PRESET FROM THE EDIT TILES AND THE LAYOUT / SPACING LISTS
	;; RETURNS THE PRESET IF IT IS VALID, ELSE A MESSAGE STRING DESCRIBING THE FIRST PROBLEM
	
	(defun DCL:PileGeometry ( / val mdl hd ovn pos )
		(if (and
				lay
				(setq val (DCL:PackSizes))
				(setq mdl (last (last (apply 'PPP:PackSpans (cons (RowsToPacks lay) val))))) ;; MODULES LENGTH, SAME AS PPP:TrackerLayout
				(cddr (setq hd (DCL:SpacingRows)))
				(vl-every 'numberp hd)
				(setq ovn
					(if (= "1" (get_tile "pile_mode"))
						(if (setq pos (DCL:JointSpacing)) (PPP:CleanLength (- (car hd) (car pos)))) ;; PILE 1 SITS AT THE FIRST JOINT
						(std:ParseLength (get_tile "overhang_north"))
					)
				)
			)
			(list hd ovn (PPP:CleanLength (- (apply '+ hd) mdl ovn)) mdl)
		)
	)
	;; RETURNS ( PILE-SPACING NORTH-OVERHANG SOUTH-OVERHANG MODULES-LENGTH ) FROM THE CURRENT TILES AND LISTS, NIL IF A VALUE IS MISSING
	;; PILE SPACING IS THE FULL LIST, NORTH END OF THE TORQUE TUBE TO SOUTH END, ITS TOTAL IS THE TORQUE TUBE LENGTH
	;; THE SOUTH OVERHANG IS THE DIFFERENCE: TORQUE TUBE LENGTH - MODULES LENGTH - NORTH OVERHANG (SEE PPP:TubeOverhangs)
	
	
	;; MODULE LAYOUT LIST ;;
	
	(defun DCL:LayoutModules ( sde / mot cnt )
		(setq cnt 0)
		(foreach row lay
			(cond
				((= "M" (car row))
					(setq mot t)
				)
				((and (= "B" (car row)) (or (null sde) (and (= sde "N") (not mot)) (and (= sde "S") mot)))
					(setq cnt (+ cnt (cadr row)))
				)
			)
		)
		cnt
	)
	;; RETURNS THE MODULE COUNT OF THE LAYOUT, [ sde ] == "N" NORTH OF THE MOTOR, "S" SOUTH OF THE MOTOR, NIL ALL MODULES
	
	(defun DCL:LayoutNextRow ( pos / out )
		(while (and (null out) (setq pos (1+ pos)) (nth pos lay))
			(if (/= "J" (car (nth pos lay)))
				(setq out (nth pos lay))
			)
		)
		out
	)
	;; RETURNS THE NEXT BAY OR MOTOR ROW AFTER ROW [ pos ], SKIPPING EXTRA JOINT ROWS, NIL AT THE END
	
	(defun DCL:LayoutRefresh ( / bay pos )
		(setq
			bay 0
			pos -1
		)
		(PPP:FillList "layout_list"
			(mapcar
				'(lambda ( row )
					(setq pos (1+ pos))
					(cond
						((= "B" (car row))
							(strcat
								"Bay " (itoa (setq bay (1+ bay))) "\t" (itoa (cadr row)) " module" (if (= 1 (cadr row)) "" "s")
								(if (and (caddr row) (= "B" (car (DCL:LayoutNextRow pos)))) ", no joint after" "")
							)
						)
						((= "M" (car row)) "Motor")
						( t "Extra joint gap")
					)
				)
				lay
			)
			lix
		)
		(set_tile "Columns" (itoa (DCL:LayoutModules nil)))
		(set_tile "layout_totals"
			(cond
				((null lay)
					"Empty, pick Bay and click Add to start at the north end."
				)
				((assoc "M" lay)
					(strcat
						"North of motor " (itoa (DCL:LayoutModules "N"))
						", south " (itoa (DCL:LayoutModules "S"))
						", total " (itoa (DCL:LayoutModules nil))
					)
				)
				( t
					(strcat "No motor, total " (itoa (DCL:LayoutModules nil)) " modules")
				)
			)
		)
		(if idx
			(DCL:LayoutModes)
		)
	)
	;; RELOADS THE MODULE LAYOUT LIST, THE CALCULATED MODULE COUNT, AND THE NORTH / SOUTH TOTALS
	
	(defun DCL:LayoutModes ( / row nxt )
		(setq
			row (if lix (nth lix lay))
			nxt (if lix (DCL:LayoutNextRow lix))
		)
		(mode_tile "bay_size" (if (= "B" (car row)) 0 1))
		(if (= "B" (car row))
			(set_tile "bay_size" (itoa (1- (min (cadr row) (length bsz)))))
		)
		(mode_tile "no_joint" (if (and (= "B" (car row)) (= "B" (car nxt))) 0 1)) ;; ONLY BETWEEN TWO BAYS
		(set_tile "no_joint" (if (and (= "B" (car row)) (= "B" (car nxt)) (caddr row)) "1" "0"))
		(foreach key (list "layout_delete" "layout_up" "layout_down")
			(mode_tile key (if row 0 1))
		)
		(if row
			(progn
				(if (= 0 lix) (mode_tile "layout_up" 1))
				(if (= lix (1- (length lay))) (mode_tile "layout_down" 1))
			)
		)
		(mode_tile "layout_flip" (if lay 0 1))
	)
	;; ENABLES THE LAYOUT CONTROLS THAT APPLY TO THE SELECTED ROW
	
	(defun DCL:LayoutChanged ( / )
		(DCL:LayoutRefresh)
		(DCL:SpacingRefresh)
		(DCL:PresetSummary)
	)
	;; UPDATES EVERYTHING THAT DEPENDS ON THE MODULE LAYOUT
	
	(defun DCL:LayoutSelect ( val / )
		(setq lix (atoi val))
		(DCL:LayoutModes)
	)
	;; LAYOUT LIST SELECTION CHANGED
	
	(defun DCL:LayoutBaySize ( val / row )
		(if (and lix (= "B" (car (setq row (nth lix lay)))))
			(progn
				(setq lay (std:ReplaceNth lay lix (list "B" (1+ (atoi val)) (caddr row))))
				(DCL:LayoutChanged)
			)
		)
	)
	;; BAY SIZE POPUP CHANGED, UPDATES THE SELECTED BAY
	
	(defun DCL:LayoutNoJoint ( val / row )
		(if (and lix (= "B" (car (setq row (nth lix lay)))))
			(progn
				(setq lay (std:ReplaceNth lay lix (list "B" (cadr row) (= "1" val))))
				(DCL:LayoutChanged)
			)
		)
	)
	;; NO JOINT AFTER TOGGLE CHANGED (PVCASE BACK TO BACK MODULE PACKS)
	
	(defun DCL:LayoutAdd ( / knd pos )
		(setq knd (nth (atoi (get_tile "layout_kind")) (list "B" "M" "J")))
		(if (and (= "M" knd) (assoc "M" lay))
			(set_tile "error" "Error: This tracker already has a motor, only one motor per tracker is supported.")
			(progn
				(setq
					pos (if lix (1+ lix) (length lay))
					lay (std:InsertNth lay pos (list knd (if (= "B" knd) (1+ (atoi (get_tile "bay_size"))) 1) nil))
					lix pos
				)
				(set_tile "error" "")
				(DCL:LayoutChanged)
			)
		)
	)
	;; ADDS THE KIND PICKED IN THE POPUP (BAY, MOTOR, OR EXTRA JOINT GAP) AFTER THE SELECTED ROW, NEW BAYS USE THE MODULES POPUP
	
	(defun DCL:LayoutDelete ( / )
		(if lix
			(progn
				(setq
					lay (std:RemoveNth lay lix)
					lix (if lay (min lix (1- (length lay))))
				)
				(DCL:LayoutChanged)
			)
		)
	)
	;; DELETES THE SELECTED ROW
	
	(defun DCL:LayoutMove ( drc / row )
		(if (and lix (< -1 (+ lix drc) (length lay)))
			(progn
				(setq
					row (nth lix lay)
					lay (std:ReplaceNth (std:ReplaceNth lay lix (nth (+ lix drc) lay)) (+ lix drc) row)
					lix (+ lix drc)
				)
				(DCL:LayoutChanged)
			)
		)
	)
	;; MOVES THE SELECTED ROW UP ( -1 ) OR DOWN ( 1 )
	
	(defun DCL:LayoutFlip ( / gmt nam pos beg end nrt srt )
		(setq
			gmt (DCL:PileGeometry) ;; READ BEFORE FLIPPING, THE SOUTH OVERHANG BECOMES THE NORTH OVERHANG
			lay (PacksToRows (reverse (RowsToPacks lay))) ;; REVERSING THE PACKS KEEPS EVERY JOINT GAP BETWEEN THE SAME BAYS
			lix (if lay 0)
			spc (reverse spc)
			six (if spc 0)
			jte (reverse jte)
			nam (vl-string-trim " " (get_tile "Name"))
		)
		(if (and gmt (= "0" (get_tile "pile_mode")))
			(set_tile "overhang_north" (std:NumberString (caddr gmt)))
		) ;; PILES AT EVERY JOINT RECALCULATE IT
		(if (and
				(PPP:RowPrefix nam)
				(setq pos (vl-string-search "M" (strcase nam) 3)) ;; THE "M" BETWEEN THE NORTH AND SOUTH MODULE COUNTS, AFTER THE PREFIX
				(setq beg pos)
				(progn
					(while (and (> beg 3) (wcmatch (substr nam beg 1) "#"))
						(setq beg (1- beg))
					)
					(setq nrt (substr nam (1+ beg) (- pos beg))) ;; DIGITS JUST BEFORE THE "M"
					(setq srt (substr nam (+ pos 2)))
					(if (not (wcmatch srt "#*"))
						(setq srt "")
					) ;; NOTHING TO SWAP IF NO NUMBER FOLLOWS THE "M"
					(setq end 0)
					(while (wcmatch (substr srt (1+ end) 1) "#")
						(setq end (1+ end))
					)
					(and (< 0 (strlen nrt)) (< 0 end))
				)
			)
			(set_tile "Name" (strcat (substr nam 1 beg) (substr srt 1 end) "M" nrt (substr srt (1+ end))))
		) ;; THE COUNTS EITHER SIDE OF THE "M" SWAP: INT-28M0 BECOMES INT-0M28, INT30-0M30 BECOMES INT30-30M0
		(DCL:LayoutChanged)
	)
	;; REVERSES THE TRACKER NORTH TO SOUTH: MODULE LAYOUT, PILE SPACING, OVERHANGS, AND AN XMY NAME
	
	
	;; PILE SPACING LIST ;;
	
	(defun DCL:PackSizes ( / val )
		(setq val (mapcar 'std:ParseLength (mapcar 'get_tile (list "SizeY" "ModuleGap" "JointGapSize" "MotorGapSize"))))
		(if (vl-every 'numberp val)
			val
		)
	)
	;; RETURNS ( MODULE-WIDTH MODULE-GAP JOINT-GAP MOTOR-GAP ) IN FT FROM THE EDIT TILES (THE PPP:PackSpans SIZES), NIL IF ANY IS NOT A NUMBER
	
	(defun DCL:JointSpacing ( / val )
		(if (and lay (setq val (DCL:PackSizes)))
			(mapcar
				'(lambda ( spn ) (* 0.5 (+ (caddr spn) (cadddr spn)))) ;; PILE AT THE CENTER OF THE JOINT GAP OR MOTOR GAP
				(vl-remove-if '(lambda ( spn ) (= "Modules" (car spn))) (apply 'PPP:PackSpans (cons (RowsToPacks lay) val)))
			)
		)
	)
	;; CALCULATES PILE POSITIONS FOR A TRACKER WITH A PILE AT EVERY JOINT GAP AND AT THE MOTOR (EX. NEXTRACKER)
	;; RETURNS PILE OFFSETS (FT) FROM THE NORTH EDGE OF THE MODULES, PILE 1 FIRST
	;; NIL IF A VALUE IS MISSING OR THE LAYOUT HAS NO JOINT GAP OR MOTOR
	
	(defun DCL:SpacingRows ( / pos )
		(if (= "1" (get_tile "pile_mode"))
			(if (cdr (setq pos (DCL:JointSpacing)))
				(append (list (car jte)) (mapcar '- (cdr pos) pos) (list (cadr jte)))
			)
			spc
		)
	)
	;; RETURNS THE FULL PILE SPACING SHOWN IN THE LIST: NORTH END TO PILE 1, SPACING BETWEEN PILES, LAST PILE TO SOUTH END
	;; PILES AT EVERY JOINT CALCULATE THE SPACING BETWEEN PILES, THE TORQUE TUBE END DISTANCES ARE STILL ENTERED
	
	(defun DCL:SpacingEnd ( pos / )
		(and pos (or (= 0 pos) (= pos (1- (length (DCL:SpacingRows))))))
	)
	;; T IF LIST ROW [ pos ] IS A TORQUE TUBE END DISTANCE (FIRST OR LAST ROW)
	
	(defun DCL:SpacingRefresh ( / lst pos cnt gmt )
		(setq
			lst (DCL:SpacingRows)
			cnt (length lst)
			pos -1
		)
		(if (and six (>= six cnt))
			(setq six (if lst (1- cnt)))
		)
		(PPP:FillList "spacing_list"
			(cond
				(lst
					(mapcar
						'(lambda ( val )
							(strcat
								(cond
									((= 0 (setq pos (1+ pos))) "North end - 1")
									((= pos (1- cnt)) (strcat (itoa pos) " - South end"))
									( t (strcat (itoa pos) " - " (itoa (1+ pos))))
								)
								"\t" (std:NumberString val)
							)
						)
						lst
					)
				)
				((= "1" (get_tile "pile_mode"))
					(list "Needs 2+ joint gaps or a motor")
				)
				( t
					(list "Click Add to enter spacing")
				)
			)
			(if lst six)
		)
		(set_tile "spacing_value" (if (and six lst) (std:NumberString (nth six lst)) ""))
		(setq gmt (DCL:PileGeometry))
		(if (= "1" (get_tile "pile_mode"))
			(set_tile "overhang_north" (if gmt (std:NumberString (cadr gmt)) ""))
		) ;; PILES AT EVERY JOINT FIX THE MODULES TO THE PILES, SO THE NORTH OVERHANG IS CALCULATED
		(set_tile "overhang_south" (if gmt (std:NumberString (caddr gmt)) ""))
		(if idx
			(DCL:SpacingModes)
		)
	)
	;; RELOADS THE PILE SPACING LIST AND THE OVERHANGS, THE LIST RUNS FROM THE NORTH END OF THE TORQUE TUBE TO THE SOUTH END
	
	(defun DCL:SpacingModes ( / mde )
		(setq mde (if (= "1" (get_tile "pile_mode")) 1 0))
		(mode_tile "spacing_value" (if (and six (or (= 0 mde) (DCL:SpacingEnd six))) 0 1))
		(mode_tile "spacing_delete" (if (and (= 0 mde) six) 0 1))
		(foreach key (list "spacing_add" "spacing_copies" "overhang_north")
			(mode_tile key mde)
		)
		(mode_tile "center_motor" (if (and (= 0 mde) spc (assoc "M" lay)) 0 1))
		(mode_tile "overhang_south" 1)
	)
	;; CUSTOM SPACING: EVERY ROW AND THE NORTH OVERHANG ARE EDITABLE
	;; PILES AT EVERY JOINT: ONLY THE TORQUE TUBE END DISTANCES ARE EDITABLE, THE SOUTH OVERHANG IS ALWAYS CALCULATED
	
	(defun DCL:SpacingChanged ( / )
		(DCL:SpacingRefresh)
		(DCL:PresetSummary)
	)
	;; UPDATES EVERYTHING THAT DEPENDS ON THE PILE SPACING
	
	(defun DCL:SpacingMode ( val / )
		(cond
			((and (= "0" val) (null spc))
				(setq spc (progn (set_tile "pile_mode" "1") (DCL:SpacingRows)))
				(set_tile "pile_mode" "0")
			) ;; SWITCHING TO CUSTOM STARTS FROM THE CALCULATED SPACING, THE NORTH OVERHANG TILE ALREADY SHOWS THE CALCULATED VALUE
			((and (= "1" val) spc (cddr spc))
				(setq jte (list (car spc) (last spc)))
			) ;; SWITCHING TO PILES AT EVERY JOINT KEEPS THE TORQUE TUBE END DISTANCES
		)
		(setq six (if (DCL:SpacingRows) 0))
		(DCL:SpacingChanged)
	)
	;; PILE MODE TOGGLE CHANGED
	
	(defun DCL:CenterMotor ( / pre best ovn nov )
		(cond
			((= 'STR (type (setq pre (DCL:PresetDraft))))
				(set_tile "error" (strcat "Error: " pre))
			)
			((null (setq best (PPP:MotorPileNearest pre)))
				(set_tile "error" "Error: Add a motor to the module layout first.")
			)
			((<= (abs (cdr best)) 1e-9)
				(set_tile "error" (strcat "The motor gap is already centered on pile " (itoa (car best)) "."))
			)
			((minusp (setq nov (PPP:CleanLength (+ (setq ovn (json:Get pre "PilingOverhangLeftSize")) (cdr best)))))
				(set_tile "error" (strcat "Error: Centering puts the modules " (rtos (- nov) 2 3) " ft past the north end of the tube."))
			)
			( t
				(set_tile "overhang_north" (std:NumberString nov))
				(DCL:SpacingChanged)
				(set_tile "error"
					(strcat
						"Moved the modules " (rtos (abs (cdr best)) 2 4) " ft " (if (minusp (cdr best)) "north" "south")
						" to center the motor gap on pile " (itoa (car best)) ", spacing unchanged."
					)
				)
			)
		)
	)
	;; MOVES THE MODULES ON THE TORQUE TUBE (BY CHANGING THE NORTH OVERHANG) SO THE MOTOR GAP IS CENTERED ON THE NEAREST PILE
	;; THE PILE SPACING DOES NOT CHANGE, THE SOUTH OVERHANG ABSORBS THE DIFFERENCE, CUSTOM MODE ONLY
	
	(defun DCL:SpacingSelect ( val / )
		(setq six (atoi val))
		(set_tile "spacing_value" (std:NumberString (nth six (DCL:SpacingRows))))
		(DCL:SpacingModes)
	)
	;; SPACING LIST SELECTION CHANGED
	
	(defun DCL:SpacingValue ( val / num end )
		(setq end (DCL:SpacingEnd six))
		(cond
			((or (null six) (and (= "1" (get_tile "pile_mode")) (not end)))
				nil
			)
			((and (setq num (std:ParseLength val)) (if end (>= num 0) (> num 0)))
				(if (= "1" (get_tile "pile_mode"))
					(setq jte (if (= 0 six) (list num (cadr jte)) (list (car jte) num)))
					(setq spc (std:ReplaceNth spc six num))
				)
				(set_tile "error" "")
				(DCL:SpacingChanged)
			)
			(end
				(set_tile "error" "Error: Torque tube end distance must be zero or greater, in decimal feet or feet-inches.")
			)
			( t
				(set_tile "error" "Error: Spacing between piles must be greater than zero, in decimal feet or feet-inches.")
			)
		)
	)
	;; SELECTED ROW EDITED, ACCEPTS DECIMAL FEET OR FEET-INCHES (EX. 29'-7 1/4")
	;; THE FIRST AND LAST ROWS ARE THE TORQUE TUBE END DISTANCES, THE ONLY ROWS EDITABLE WITH PILES AT EVERY JOINT
	
	(defun DCL:SpacingAdd ( / val cnt )
		(setq
			val (if six (nth six spc) 0.0)
			cnt (1+ (atoi (get_tile "spacing_copies")))
		)
		(repeat cnt
			(setq
				six (if six (1+ six) (length spc))
				spc (std:InsertNth spc six val)
			)
		)
		(DCL:SpacingChanged)
	)
	;; ADDS COPIES OF THE SELECTED SPACING AFTER IT (REPLACES THE OLD n@s SHORTHAND), THE LAST COPY IS SELECTED
	
	(defun DCL:SpacingDelete ( / )
		(if six
			(progn
				(setq
					spc (std:RemoveNth spc six)
					six (if spc (min six (1- (length spc))))
				)
				(DCL:SpacingChanged)
			)
		)
	)
	;; DELETES THE SELECTED SPACING
	
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
				(setq wrk (std:ReplaceNth wrk idx res))
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
				(setq wrk (if pos (std:ReplaceNth wrk pos par) (cons par wrk)))
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
					wrk (std:RemoveNth wrk idx)
					idx nil ;; THE DELETED PRESET IS NOT SAVED BY THE REFRESH
				)
				(DCL:PresetRefresh (if wrk (json:Get (nth (min pos (1- (length wrk))) wrk) "Name")))
			)
		)
	)
	;; DELETES THE SELECTED PRESET AFTER CONFIRMATION, THE NEXT PRESET IN THE LIST IS SELECTED
	
	(defun DCL:PresetImport ( / cur pth res add skp bad uns prb fpb nam )
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
						bad 0
						uns (cadr res) ;; NOT 1P SINGLE ROW (MULTI-ROW, LANDSCAPE), COUNTED BY ImportPresetFile
					)
					(foreach pre (car res)
						(cond
							((setq prb (PPP:PresetProblem pre))
								(if (= 0 bad)
									(setq fpb (strcat (PresetText (json:Get pre "Name")) ": " prb))
								) ;; THE FIRST PROBLEM IS SHOWN, PRESETS FROM ONE FILE USUALLY SHARE IT
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
							(if (> uns 0) (strcat ", " (itoa uns) " unsupported (not 1P single row)") "")
							(if (> bad 0) (strcat ", " (itoa bad) " not valid, ex. " fpb) ".")
						)
					)
				)
			)
		)
	)
	;; IMPORTS PRESETS FROM A PVCASE .PVsettings FILE OR A PPP PRESET FILE, SELECTS THE FIRST IMPORTED PRESET
	
	(defun DCL:PresetSummary ( / res trk mot par ovh wrn )
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
						trk (PPP:TrackerLayout res) ;; NOT 'lay', THAT IS THE MODULE LAYOUT LIST
						mot (PPP:MotorPileIndex res)
						par (PPP:PairName (json:Get res "Name"))
						ovh (PPP:TubeOverhangs res)
					)
					(set_tile "error" "") ;; THE VALUES ARE VALID AGAIN, CLEAR AN ERROR LEFT FROM BEFORE THEY WERE FIXED
					(strcat
						(itoa (fix (json:Get res "Columns"))) " modules, "
						(itoa (length (PPP:PilePositions res))) " piles, "
						(if mot (strcat "motor pile " (itoa mot)) "no motor") ", "
						(if (PPP:FindPreset wrk par) (strcat "pair " par) (strcat "no " (PPP:RowPrefix par) " pair yet"))
					)
				)
			)
		)
		(set_tile "preset_lengths"
			(if (and idx (/= 'STR (type res)) (setq ovh (PPP:TubeOverhangs res)))
				(strcat
					"Torque tube " (rtos (PPP:TubeLength res) 2 3) " ft, modules " (rtos (car (PPP:TrackerLayout res)) 2 3) " ft, "
					"overhang N " (rtos (car ovh) 2 3) " / S " (rtos (cadr ovh) 2 3) " ft"
				)
				""
			)
		) ;; SECOND LINE SO THE SUMMARY FITS THE DIALOG WIDTH
		(set_tile "preset_warning"
			(if (and idx (/= 'STR (type res)) (setq wrn (PPP:PresetWarnings res)))
				(strcat "Note: " (car wrn) (if (cdr wrn) (strcat " (+" (itoa (length (cdr wrn))) " more)") ""))
				""
			)
		)
	)
	;; DISPLAYS THE CALCULATED TRACKER LENGTH, PILE COUNT, OVERHANGS, MOTOR PILE, AND INT / EXT PAIR, OR THE FIRST PROBLEM FOUND
	;; UNUSUAL BUT ALLOWED VALUES (SEE PPP:PresetWarnings) ARE SHOWN ON THE LINE BELOW
	
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
			((and
					(setq msg
						(vl-remove nil
							(mapcar
								'(lambda ( pre / wrn )
									(if (setq wrn (PPP:PresetWarnings pre))
										(strcat (PresetText (json:Get pre "Name")) ": " (car wrn))
									)
								)
								wrk
							)
						)
					)
					(/= 6
						(wsh:ConfirmationDialog
							(strcat
								(itoa (length msg)) " preset" (if (= 1 (length msg)) " has" "s have") " unusual values:\n\n"
								(json:StringJoin (if (> (length msg) 10) (append (reverse (member (nth 9 msg) (reverse msg))) (list "...")) msg) "\n") ;; FIRST 10, PRESET NAMES ARE UNIQUE
								"\n\nSave the presets anyway?"
							)
							"Tracker Presets" 0 52
						)
					)
				)
				(set_tile "error" "Not saved. Select a preset to see its note below the summary.")
			)
			( t
				(setq prs wrk)
				(done_dialog 1)
			)
		)
	)
	;; SAVES THE CURRENT PRESET, CHECKS EVERY PRESET AND INT / EXT PAIR, AND UPDATES 'prs' IF ALL CHECKS PASS
	;; PRESETS WITH UNUSUAL VALUES (SEE PPP:PresetWarnings) ARE LISTED AND SAVED ONLY IF THE USER CONFIRMS
	
	
	(setq res nil)
	(if
		(and
			(setq dlg (PPP:DialogLoad dcl "PPPSETUP"))
			(setq tmp (car dlg) dch (cdr dlg))
			(new_dialog ttl dch)
		)
		(progn
			(foreach pop pops
				(PPP:FillList (car pop) (cdr pop) nil)
			)
			;; INITIALIZE POPUP LISTS
			
			(foreach key keys
				(set_tile key
					(if (assoc key pops)
						(itoa (cond ((vl-position (cdr (assoc key shw)) (cdr (assoc key pops)))) (0)))
						(cdr (assoc key shw))
					)
				)
			)
			;; LOAD SAVED OR DEFAULT VALUES INTO EVERY TILE
			
			(foreach key lck
				(mode_tile key 1)
			)
			(set_tile "lock_note" (if lck "Greyed out fields are set by PPPSETUP and cannot be changed here." ""))
			;; FIELDS SET BY PPPSETUP (EX. DRAWING NAME, UNITS) ARE SHOWN BUT LOCKED WHEN EDITING AN EXISTING PROJECT (PPPSETTINGS)
			
			(DCL:UpdateCottRange)
			(DCL:UpdateSlope (get_tile "table_slope"))
			(DCL:UpdatePresetInfo)
			
			(action_tile "accept" "(DCL:RunDialogChecks)")
			(action_tile "cancel" "(done_dialog 0)") ;; PPP:DialogUnload BELOW UNLOADS THE DIALOG AND DELETES THE TEMPORARY DCL FILE
			
			(action_tile "cott_buckets" "(DCL:UpdateCottRange)")
			(action_tile "bucket_step" "(DCL:UpdateCottRange)")
			(action_tile "table_slope" "(DCL:UpdateSlope $value)")
			(action_tile "presets" "(DCL:PresetsDialog)") ;; NESTED DIALOG, OPENS ON TOP OF THE SETTINGS DIALOG
			
			(setq sts (start_dialog))
		)
		(prompt "\nError loading dialog box.")
	)
	(setq dch (PPP:DialogUnload tmp dch))
	(if (= 1 sts)
		(list
			(append res (vl-remove-if '(lambda ( pair ) (assoc (car pair) res)) cfg)) ;; SETTINGS NOT IN THE DIALOG (EX. "civil_output", ADDED BY HAND) ARE KEPT
			prs
		)
	)
)
;; DISPLAYS THE PROJECT SETTINGS DIALOG AND THE NESTED TRACKER PRESETS DIALOG, USED BY PPPSETUP (NEW PROJECT) AND PPPSETTINGS (EXISTING PROJECT)
;; RETURNS ( SETTINGS PRESETS ) IF ACCEPTED, NIL IF CANCELLED OR THE DIALOG FAILED TO LOAD
;; THE RETURNED SETTINGS ARE THE TILE VALUES, THE CALCULATED COTT RANGE / SLOPE, AND EVERY OTHER KEY OF [ cfg ] UNCHANGED
;; THE PRESETS ARE ONLY CHANGED WHEN THE PRESETS DIALOG IS ACCEPTED, NOTHING IS SAVED TO FILE HERE, THE CALLER SAVES BOTH
;; [ cfg ] == PROJECT SETTINGS, ( "KEY" . VALUE ) PAIRS (NIL FOR A NEW PROJECT), SAVED STRING VALUES WIN OVER THE DEFAULTS
;; [ prs ] == TRACKER PRESETS, PLAIN LIST OF JSON OBJECTS
;; [ dir ] == PROJECT ROOT FOLDER FOR THE PROJECT NUMBER / NAME DEFAULTS (SEE PPP:DefaultSettings), NIL WHEN THE SETTINGS ALREADY HAVE THEM
;; [ lck ] == TILE KEYS SHOWN BUT LOCKED (EX. "file_name"), NIL FOR PPPSETUP, ALSO SWITCHES THE DIALOG TITLE AND SHOWS A NOTE
;; 'tmp' / 'dch' ARE NAMED LIKE THE CALLERS' SO THEIR *error* (DYNAMIC SCOPE) UNLOADS THIS DIALOG IF ESC OR AN ERROR HITS


(defun PPP:FillList ( key lst sel / )
	(start_list key)
	(foreach itm lst
		(add_list itm)
	)
	(end_list)
	(if sel
		(set_tile key (itoa sel))
	)
)
;; FILLS A LIST BOX OR POPUP LIST [ key ] WITH STRINGS, [ sel ] == ZERO-BASED ITEM TO SELECT, OR NIL


; (Load:DescriptionLog "STR-PPP_Utilities" "Shared functions for the PPP / BOM commands")
