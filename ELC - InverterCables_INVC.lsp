;; INVERTER COMMANDS FOR ELECTRICAL TEAM
;; THESE CODES ARE SPECIFIC TO THE ELECTRICAL DISCIPLINE ONLY AND ARE FOR SETTING UP INVERTER LOCATIONS AS WELL AS STORING META-DATA FOR EXCEL

;;; STEPS:
;;; INSERT INVERTERS
;;;		- MUTE ATTRIBUTE POP-UP FOR EACH INSERTION
;;;		- SELECT LOCATION FOR EACH INVERTER, HIT ENTER TO CANCEL / STOP
;;;		- IF AT LEAST TWO INVERTERS WERE PLACED, CONTINUE
;;; SELECT APPROXIMATE LOCATION OF SUBSTATION
;;;		- USER TO CLICK LOCATION OF SUBSTATION FOR CABLE DESTINATION
;;; ASSOCIATE CABLES
;;;		- DIALOG BOX FOR USER SPECIFIED PARAMETERS (NUMBER OF CABLE TYPES, CABLE NAME / TAG, CABLE LAYER COLOR) *NEED TO FIGURE OUT HOW TO CREATE OR ASSOCIATE LAYER*
;;;		- CLICK INSERT POINT OF FIRST INVERTER AND THEN INSERTION POINT OF NEXT INVERTER
;;;		- OPTIONALLY CLICK ADDITIONAL LOCATIONS FOR BENDS IN CABLE
;;;		- CABLE IS DRAWN BETWEEN INVERTERS AND CLICKED POINT LOCATIONS
;;;		- META-DATA TO BE FILLED BY USER FOR EACH CABLE (DIALOG BOX ??)
;;; FINISHED
;;; THERE WILL BE A SEPERATE COMMAND FOR PARSING BLOCK AND LINE DATA INTO AN EXCEL SPREAD SHEET

;;; NOTES:
;;; CABLES TO BE 3'-6" MINIMUM AWAY FROM INVERTER "BOUNDARY"
;;; IF SELECTION OF POINT IS MADE ON INVERTER, THE CABLES LEFT SIDE MUST BE ANGLED 45 DEG. AND FORM A "V" SHAPE FROM THE INVERTER INSERTION POINT
;;; LINE MUST ALWAYS BE OFFSET FROM INVERTER "BOUNDARY" LOCATION
;;; USE XDATA FOR CABLE LINES:
;;; LDATA (LISP Data) — Easiest for LISP
;;;	If you only need to access this data via AutoLISP, vlax-ldata functions are the most user-friendly. They store data in "extension dictionaries" attached to the object. 
;;;	Syntax:
;;;	- Store: (vlax-ldata-put entity_object "KeyName" "YourValue").
;;;	- Retrieve: (vlax-ldata-get entity_object "KeyName").
;;;	Pro: It handles complex data types (like lists or integers) without you needing to worry about DXF group codes.
;;; KEEP COUNT OF NUMBER OF INVERTERS, THERE CAN BE TWO DIFFERENT TYPES OF INVERTERS (THIS COORESPONDS TO THE ATTRIBUTE VALUES FOR TYP_1 AND TYP_2
;;;	INVERTER COUNTS START FROM INVERTER FURTHEST FROM THE SUBSTATION
;;; THERE ARE 5 - 6 DIFFERENT INVERTER TYPES
;;; CROSSINGS ARE TRIGGERED BASED ON CABLE LINES CROSSING BOUNDARIES FOR ROADWAYS, STREAMS OR OTHER OBSTRUCTIONS

;;; REQUIREMENTS:
;;; THERE MUST BE A CENTRAL LOCATION FOR THE INVERTER BLOCKS TO BE PULLED FROM
;;; INVERTER BLOCK MUST BE THE SAME AND ADJUSTABLE THROUGH A DIALOG BOX (I WILL ADJUST EXISTING INVERTER BLOCK)
;;; CABLE LAYERS MUST EXIST OR BE CREATED USING THE DIALOG BOX AND CABLE NAMES ???

;;; ADDITIONAL REQUIRED FUNCTIONS:
;;; *CODE TO EDIT META-DATA IN CABLES*
;;;		- SINCE THE DATA IS HIDDEN, THERE NEEDS TO BE A FUNCTION THAT CAN DISPLAY CABLE INFORMATION TO THE USER AND ALLOW THEM TO MAKE CHANGES AS REQUIRED IF NEEDED
;;;	*CABLE FUNCTION SHOULD BE STAND-ALONE AND CALLABLE AT ANY POINT*
;;;		- CABLE LINES MAY NEED TO BE REMOVED AND EDITED, THERE SHOULD BE AN EASY WAY TO CREATE NEW CABLE LINES WHERE NEEDED
;;;		- POTENTIALLY COULD USE REACTOR METHODS TO CHECK IF THE INVERTER BLOCK TAGS CHANGED AND AUTOMATICALLY UPDATE THE LINE **RISKY, PROBABLY WON'T GO THIS ROUTE**
;;; *DCL BOX*
;;;		- DIALOG BOXES TO ALLOW USER TO FILL IN PERTINENT DATA RELATED TO THE INVERTER INFORMATION (INVERTER MANUFACTURER, SIZE [MWs] ETC.)


;;; !IMPORTANT!: THE "LIB - STD SUB-FUNCTION.LSP" LIBRARY FILE AND THE "LIB - GRAPHICAL READ SUB-FUNCTION.LSP" FILE ARE REQUIRED FOR THIS FUNCTION TO WORK PROPERLY

;;; INVERTER INSERTION FUNCTIONS ;;;

(defun c:INVR ( )
	(c:InverterInsert)
)

(defun c:InverterInsert
	( 
		/
		;Functions
		*error*
		ReturnInverterBlockNames CheckForBlockName
		UpdateAttributeValue IncrementInverterTagString UpdateInverterBlockRotation
		InsertInverterHelperFunction
		INVR:CreateDialogForm INVR:DialogUpdates
		;Variables
		dir dwg fil doc acd gst
		run sts ilp grd cod dat bnm blk
		bfr rtf bpt dst mpt
		ttl dcl tmp des dch
		nms inv tag siz rot osm ort osf
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
		(redraw)
		(if gst (vla-delete gst))
		(std:ObjectRelease (list doc))
		(strcat "\nError: " msg)
	)
	
	(defun ReturnInverterBlockNames ( doc ptn / bnm lst )
		(vlax-for blk (vla-get-blocks doc)
			(if (and
					(wcmatch (strcase (setq bnm (vla-get-name blk))) (strcase ptn))
					(= (vla-get-isxref blk) :vlax-false)
					(= (vla-get-islayout blk) :vlax-false)
				)
				(setq lst (cons (substr bnm (strlen ptn)) lst))
			)
		)
		lst
	)
	;; RETURNS LIST ALL BLOCK NAMES THAT START WITH A SPECIFIC STRING PREFIX IN A DOCUMENT OBJECT
	
	(defun CheckForBlockName ( bnm / msp res )
		(setq bnm (strcat "*" bnm))
		(setq msp (vl-catch-all-apply 'vla-get-modelspace (list (vla-get-activedocument (vlax-get-acad-object)))))
		(if (not (vl-catch-all-error-p msp))
			(vlax-for blk msp
				(if (and
						(= (vla-get-objectname blk) "AcDbBlockReference")
						(wcmatch (vla-get-effectivename blk) bnm)
					)
					(setq res t)
				)
			)
		)
		res
	)
	;; CHECKS IF THE BLOCK WITH THE NAME 'BNM' EXISTS IN THE DRAWING DATABASE
	;; NOTE: THIS DOES NOT REDEFINE THE EXISTING BLOCK
	
	(defun UpdateAttributeValue ( blk att val )
		(if (= (vla-get-hasattributes blk) :vlax-true)
			(foreach a (std:Variant->List (vla-getattributes blk))
				(if (= (vla-get-tagstring a) att)
					(vla-put-textstring a val)
				)
			)
		)
	)
	;; UPDATES BLOCK ATTRIBUTE VALUES BASED ON ATTRIBUTE NAME IF FOUND
	
	(defun UpdateInverterBlockRotation ( blk prp rot )
		(if (= (vla-get-isdynamicblock blk) :vlax-true)
			(foreach p (vlax-invoke blk 'GetDynamicBlockProperties)
				(if (= (vla-get-propertyname p) prp)
					(vla-put-value p (vlax-make-variant rot vlax-vbDouble))
				)
			)
		)
	)
	;; ONLY APPLICABLE TO THE ROTATION PROPERTY WITHIN THE BLOCK
	;; UPDATES INVERTER ROTATION BASED ON 'ROT' VALUE
	
	(defun IncrementInverterTagString ( tag / pfx sfx )
		(if (= (type (read (substr tag (strlen tag) 1))) 'INT)
			(progn
				(setq pfx (vl-string-right-trim "0123456789" tag))
				(setq sfx (substr tag (1+ (strlen pfx))))
				(strcat pfx (itoa (1+ (atoi sfx))))
			)
			(strcat tag "1")
		)
	)
	;; INCREMENTS THE INVERTERS TAG STRING BY ONE UPON EACH INSERTION
	;; !BOOKMARK! MAY NEED TO ADD LOGIC FOR SITUATIONS WHERE TAGS END IN "01", CURRENT FUNCTION NEGATES "0" PADDING
	
	(defun InsertInverterHelperFunction ( / )
		(setq blk (vla-copy gst)) ;; PASTE BLOCK AT POINTER LOCATION
		(setq bpt (std:Variant->List (vla-get-insertionpoint blk)))
		(mapcar '(lambda ( pr ) (UpdateAttributeValue blk (car pr) (cdr pr)))
			(list
				(cons "TAG" tag)
				(cons "SIZE" siz)
				(cons "TYPE" bnm)
			)
		)
		(setq tag (IncrementInverterTagString tag)) ;; UPDATE ATTRIBUTE VALUES
		(setq bfr "") ;; CLEAR STRING BUFFER
	)
	;; GLOBAL FUNCTION USED TO CONDENSE LOGIC FOR INSERTING INVERTER BLOCKS
	
	(defun INVR:CreateDialogForm ( )
		(if (and
				(= (type dcl) 'USUBR)
				(setq tmp (vl-filename-mktemp "INVR" nil ".dcl"))
				(setq des (open tmp "w"))
				(foreach line (dcl) (write-line line des))
				(not (close des))
				(> (setq dch (load_dialog tmp)) 0)
				(new_dialog ttl dch)
			)
			dch
		)
	)
	;; GLOBAL FUNCTION USED TO UPDATE DIALOG BOX WHENEVER USER MAKES AN UPDATE TO THE SETTINGS
	
	(defun INVR:DialogUpdates ( )
		(setq
			inv (get_tile "inv")
			tag (get_tile "tag")
			siz (get_tile "siz")
			rot (rtos (atof (get_tile "rot")) 2 1)
			osm (get_tile "osm")
			ort (get_tile "ort")
			
			rtf nil
		)
		(if (not (vl-every '(lambda ( v ) (> (strlen v) 0)) (list inv tag siz rot)))
			(progn (set_tile "error" "Error: All fields must be filled.") nil)
			t
		)
	)
	
	(vl-load-com)
	
	(setq ttl "Inverter_Insertion")
	(defun dcl ( )
		(list
			"// Temporary DCL file;"
			(strcat ttl " : dialog {")
			"	label = \"Inverter Insertion Dialog Control\";"
			"	: row {"
			"		: column {"
			"			label = \"Attribute Values\";"
			"			: popup_list {"
			"				label = \"Inverter Type:\";"
			"				key = \"inv\";"
			"				width = 40;"
	(strcat "				value = \"" (if (null inv) "" inv) "\";")
			"			}"
			"			: edit_box {"
			"				label = \"Tag Name:\";"
			"				key = \"tag\";"
			"				edit_width = 20;"
	(strcat	"				value = \"" (if (null tag) "" tag) "\";")
			"			}"
			"			: edit_box {"
			"				label = \"Inverter Size:\";"
			"				key = \"siz\";"
			"				edit_width = 20;"
	(strcat	"				value = \"" (if (null siz) "" siz) "\";")
			"			}"
			"		}"
			"		spacer_1;"
			"		: column {"
			"			label = \"Inverter Geometric Details\";"
			"			: edit_box {"
			"				label = \"Inverter Rotation Value:\";"
			"				key = \"rot\";"
			"				width = 10;"
	(strcat	"				value = \"" (if (null rot) "0.0" rot) "\";")
			"			}"
			"			: row {"
			"				: toggle {"
			"					label = \"OSNAP Mode\";"
			"					key = \"osm\";"
	(strcat	"					value = \"" (if (null osm) "0" osm) "\";")
			"				}"
			"				: toggle {"
			"					label = \"Ortho Mode\";"
			"					key = \"ort\";"
	(strcat	"					value = \"" (if (null ort) "0" ort) "\";")
			"				}"
			"			}"
			"		}"
			"	}"
			"	spacer_1;"
			"	errtile;"
			"	ok_cancel;"
			"}"
		)
	)
	;; DCL UI FOR INVERTER BLOCK INSERTION LOGIC AND USER SETTINGS
	
	;; TEMPORARY BLOCK DIRECTORY -> UPDATE THIS PATH TO MATCH THE LIVE LOCATION FOR THE BLOCK LIBRARY FOR INVERTERS
	(setq dir (strcat (getenv "UserProfile") "\\OneDrive - FastGrid\\File Transfer\\AutoLISP\\Projects\\Attribute Data (Gregg)\\References\\"))
	;; TEMPORARY INVERTER BLOCK FILE NAME -> UPDATE THIS NAME TO MATCH THE LIVE DRAWING FILE THAT USERS WILL ALL HAVE ACCESS TO
	(setq dwg "INVERTER SKID COMPILATION - MJS Update Version.dwg")
	;; WILL NEED TO ADD LOGIC TO TEST PATH AND FILE LOCATION IS AVAILABLE TO THE USER (WORK WITH ACC ??)
	
	(setq fil (findfile (strcat dir dwg)))
	
	(vlax-for d (vla-get-documents (vlax-get-acad-object))
		(if (= (strcase (vla-get-fullname d)) (strcase fil))
			(setq doc d)
		)
	)
	
	(if (null doc)
		(progn
			(setq doc (std:CreateObjectDBX))
			(vla-open doc fil)
		)
	)
	
	(setq acd (vla-get-activedocument (vlax-get-acad-object)))
	(if (= 1 (getvar 'cvport))
		(prompt "\nFunction only applicable to model space.")
		(if (and fil doc)
			(if (and
					(setq dch (INVR:CreateDialogForm))
					(setq run t osf (LM:grsnap:snapfunction))
				) ;; INITIALIZE DIALOG BOX / SETTINGS
				(while run
					(start_list "inv")
					(setq nms (mapcar 'add_list (ReturnInverterBlockNames doc "Inverter - *")))
					(end_list)
					
					(action_tile "accept" "(if (INVR:DialogUpdates) (done_dialog 1))")
					(action_tile "cancel" "(setq run nil) (done_dialog 0)")
					
					(setq sts (start_dialog))
					(unload_dialog dch)
					
					(if (= sts 1)
						(progn
							(setq ilp t bfr "") ;; INITIALIZE INSERT LOOP LOGIC AND KEYBOARD STRING BUFFER
							(princ "\nInsertion loop in progress: Click to insert inverter block | [K] = Dialog Options | [ESC] / [ENTER] = Terminate Command")
							
							(setq bnm (car (mapcar '(lambda ( x ) (nth x nms)) (read (strcat "(" inv ")")))))
							(if (null (CheckForBlockName (strcat "Inverter - " bnm)))
								(if (not (vl-catch-all-error-p (setq blk (vl-catch-all-apply 'vla-Item (list (vla-get-Blocks doc) (strcat "Inverter - " bnm))))))
									(vla-CopyObjects 
										doc 
										(vlax-make-variant 
											(vlax-safearray-fill 
												(vlax-make-safearray vlax-vbObject '(0 . 0))
												(list blk)
											)
										)
										(vla-get-blocks (vla-get-ActiveDocument (vlax-get-acad-object)))
									)
								)
							)
							;; CHECK IF BLOCK NAME IS IN CURRENT DRAWING - IF NOT, PULL THE BLOCK FROM THE DOC FILE
							
							(setq gst (vl-catch-all-apply 'vla-insertblock (list (vla-get-ModelSpace acd) (vlax-3d-point '(0 0 0)) (strcat "Inverter - " bnm) 1 1 1 0)))
							(if (not (vl-catch-all-error-p gst))
								(while ilp
									(setq grd (grread t 15 0))
									(setq cod (car grd) dat (cadr grd))
									;(princ "\nCode: ") (princ cod) (princ " | Data: ") (princ dat) ;; TESTING ONLY
									(cond
										((= cod 5) ;; MOUSE MOVEMENT
											(redraw) ;; CLEAR PREVIOUS FRAMES
											(if (or (null rtf) (/= rtf rot))
												(progn
													(UpdateInverterBlockRotation gst "Inverter Rotation" (rtos (std:DegreesToRadians (atof rot)) 2 1)) ;; SETS PRECISION TO 1 DECIMAL
													(setq rtf rot)
												) ;; ROTATION FLAG ALLOWS THE FUNCTION TO RENDER THE BLOCK ONCE AND ENHANCES PERFORMANCE OF THE PROGRAM
											)
											(vla-update gst) ;; UPDATE GHOST BLOCK TO MATCH NEW POSITION AND ROTATION
											;; ORTHO MODE CHECK / LOGIC
											(if (and bpt (= ort "1")) (setq dat (gr:OrthoMode bpt dat)))
											;; OSNAP MODE CHECK / LOGIC
											(redraw (vlax-vla-object->ename gst) 2) ;; HIDE BLOCK
											(if (= osm "1") (setq dat (osf dat (getvar 'osmode)))) ;; CALCULATE SNAP ;; (setq dat (osf dat BIT_CODE))
											(redraw (vlax-vla-object->ename gst) 1) ;; SHOW BLOCK
											;; CHECK GRAPHICAL READ SUBFUNCTION LISP FILE FOR ADDITIONAL BIT CODES WITH LM:grsnap:snapfunction
											;; HIDE / SHOW LOGIC ALLOWS FOR SMOOTH SNAPPING LOGIC WITHOUT SACRIFICING PERFORMANCE

											(vla-put-insertionpoint gst (vlax-3d-point dat))
										)
										((= cod 3) ;; LEFT CLICK
											(InsertInverterHelperFunction)
										)
										((= cod 2) ;; KEYBOARD INPUT
											(cond
												((member dat (mapcar 'ascii (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))) ;; DISTANCE TYPED IN KEYBOARD LOGIC
													(setq bfr (strcat bfr (chr dat)))
													(princ (strcat "\rInverter Distance: " bfr " | Press [ENTER] to accept."))
												) ;; LOGIC FOR ALLOWING USER TO TYPE DISTANCE STRING FROM KEYBOARD SAVING VALUE TO A BUFFER
												((= dat 8) ;; BACKSPACE LOGIC
													(if (> (strlen bfr) 0)
														(progn
															(setq bfr (substr bfr 1 (1- (strlen bfr))))
															(princ (strcat "\rInverter Distance: " bfr " | Press [ENTER] to accept."))
														) ;; CLEAR THE LAST CHARACTER THEN RE-PRINT BUFFER
														(princ "\rEnter a value for distance.")
													)
												)
												((= dat 13) ;; ENTER PRESS
													(if (and (not (null bpt)) (/= bfr ""))
														(progn
															(setq mpt (cadr (grread t)))
															(setq dst (distof bfr))
															(setq bpt (polar bpt (angle bpt (if (= ort "1") (gr:OrthoMode bpt mpt) mpt)) dst))
															(vla-put-insertionpoint gst (vlax-3d-point bpt))
															(InsertInverterHelperFunction)
															(princ (strcat "\rInverter block inserted at distance: " bfr))
														) ;; BUFFER CHECK - CALCULATE DISTANCE FROM LAST POINT TO TYPED POINT AND INSERT BLOCK
														(progn
															(setq ilp nil run nil)
															(vla-delete gst)
														) ;; IF BUFFER IS EMPTY - [ENTER] KEY (CANCEL FUNCTION)
													)
												)
												((or (= dat (ascii "K")) (= dat (ascii "k")))
													(setq ilp nil)
													(vla-delete gst)
													(if (and (eq (type tmp) 'STR) (setq tmp (findfile tmp)))
														(vl-file-delete tmp)
													)
													(setq dch (INVR:CreateDialogForm))
												)
												((= dat 27) ;; [ESC] KEY (CANCEL FUNCTION)
													(setq ilp nil run nil)
													(vla-delete gst)
												)
											)
										)
										((member cod '(13 25)) ;; ENTER / RIGHT CLICK (CANCEL FUNCTION)
											(setq ilp nil run nil)
											(vla-delete gst)
										)
									)
								)
								(alert "Error: Source file does not contain the selected block name, or block name is null.")
							)
						)
					)
					
					
					;; ADD LOGIC FOR OFFSETTING THE ATTRIBUTE FOR THE TAG NAME ON THE INVERTERS (FUTURE PATCH)
					;; TAG NAME OFFSET: DISTANCE FOR TAG NAME TO BE OFFSET FROM THE INVERTER BLOCK (??? NOT SURE IF THIS WOULD BE HELPFUL)
					;; ADD FUNCTION FOR MATCHING ATTRIBUTE POSITION OF SELECTED BLOCK (??)
					
					;; POTENTIAL BUG FIXES:
					;;	- OSNAP MODE CAUSES BUGGYNESS, THERE WILL NEED TO BE A FUTURE PATCH FOR THIS FEATURE BEFORE RELEASE
					
					;; ADD LOGIC FOR REDEFINING BLOCK ??
					;;	- MAY BE NEEDED IF INVERTER BLOCKS CHANGE AFTER A DRAWING IS COMPLETED
					;;	- FUTURE POTENTIAL UPDATE
					
					;; ADD LOGIC FOR "POINT MAPPING"
					;;	- ALLOWS USER TO SET POINT MAPPING SO THE USER CAN TYPE A DISTANCE OFFSET BUT NOT INSERT A BLOCK
					;;	- THIS WOULD BE USEFUL IN SITUATIONS WHERE THE INVERTER IS LOCATED OFFSET FROM THE PREVIOUS INVERTER (150 X DIRECTION, 100 Y DIRECTION, 75 X DIRECTION)
					;;	- THE ABOVE INPUTS WOULD PLACE THE NEXT INVERTER AT A POLARIZED ANGLE AWAY FROM THE LAST INSERTED INVERTER WITHOUT HAVING TO CANCEL THE FUNCTION
					;;	- FUNCTIONALITY WOULD WORK PRIMARILY WITH ORTHO MODE TURNED ON, SO OPTION SHOULD BE GREYED OUT IF ORTHO IS "OFF"
					
					;; ?? ADD LOGIC FOR [ TAB ] ROTATION: INCREMENT THE INVERTER ROTATION PARAMTER BY 5 DEGREES WHEN [ TAB ] IS PRESSED
					;;	- WILL NEED TO UPDATE UI TO DISPLAY "CURRENT ROTATION: XXX.X"
					;;	- REMOVE ROTATION FIELD FROM DCL UI ??
					;;	- FUTURE POTENTIAL PATCH, NEED INPUT FROM USERS ON WHICH OPTION IS EASIER / MORE CONVINIENT
					;;	- ADJUST INSERTION LOCATION BASED ON "MV COMPARTMENT" LOCATION WITHIN BLOCK OBJECT ??
					
					;; ALL DIALOG BOX ITEMS SHOULD BE STORED GLOBALLY OR IN A TEMPORARY FILE FOR CACHING
					;; USE PREVIOUSLY FILLED VALUES TO UPDATE DIALOG BOX AUTOMATICALLY, ADD LOGIC TO CACHE USER DATA
					;;	- NEED TO FIGURE OUT A WAY TO UPDATE THE DIALOG BOX TO A NEW DIALOG BOX WITH USER PARAMETERS
					
					
				)
				(prompt "\nUnable to load dialog box.")
			)
			(prompt "\nSource file not found.")
		)
	)
	
	(redraw) ;; CLEAR FRAMES
	
	(if (findfile tmp)
		(vl-file-delete tmp)
	)
	(std:ObjectRelease (list doc))
	
	(princ)
)



;;; CABLES FUNCTIONS ;;;

(defun c:INVC ( )
	(c:InverterCables)
)

(defun c:InverterCables
	(
		/
		;Functions
		*error*
		ReturnCableLayerList CreateLineObject
		ReturnBlockObject ReturnDynamicPropertyValue
		CalculateAnchorPoint CalculateCornerPoint GetFlippedPoint CalculateJagLine
		INVC:CreateDialogForm INVC:DialogUpdates
		;Variables
		run dyn sts ilp grd cod dat
		spt bpt jpt bfr dst inc lnm
		bnm bl1 bl2 ins
		ac1 ac2 cr1 cr2 lst ptl gst
		ttl dcl tmp des dch
		prs nms cbl acm ipx spl plr tag
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
		(if dyn (setvar 'DYNMODE dyn))
		(if gst (vla-delete gst))
		(redraw)
		(strcat "\nError: " msg)
	)
	
	(defun ReturnCableLayerList ( pfx / lys lnm ret )
		(setq lys (vla-get-layers (vla-get-activedocument (vlax-get-acad-object))))
		(vlax-for lay lys
			(if (wcmatch (setq lnm (vla-get-name lay)) (strcat pfx "*"))
				(setq ret (cons (cons lnm lay) ret))
			)
		)
		(reverse ret)
	)
	;; RETURNS ALL LAYERS AS A LIST WITHIN THE CURRENT DRAWING FILE
	
	(defun CreateLineObject ( pts lay thk / msp lst var obj )
		(setq msp (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object))))
		(setq lst (list (car (car pts)) (cadr (car pts)) (car (cadr pts)) (cadr (cadr pts))))
		(setq var (vlax-make-variant (vlax-safearray-fill (vlax-make-safearray vlax-vbDouble (cons 0 3)) lst)))
		(setq obj (vla-addlightweightpolyline msp var))
		(vla-put-layer obj lay)
		(vla-put-constantwidth obj thk)
		obj
	)
	;; CREATE LINE FOR CABLE LOGIC
	
	(defun ReturnBlockObject ( pt / sel ent obj bnm )
		(if (and pt (setq sel (nentselp pt)))
			(progn
				(setq ent (if (> (length sel) 2) (last (last sel)) (car sel)))
				(setq obj (vlax-ename->vla-object ent))
				(if (and 
						(= (vla-get-objectname obj) "AcDbBlockReference")
						(vlax-property-available-p obj 'EffectiveName)
						(setq bnm (vla-get-effectivename obj))
						(wcmatch (strcase bnm) "INVERTER - *,SUBSTATION - *") ;; CHECK IF BLOCK NAME STARTS WITH "INVERTER" OR "SUBSTATION"
					)
					obj
				)
			)
		)
	)
	;; RETURNS BLOCK OBJECT FROM ENTITY SELECTION POINT IF SELECTION MATCHES REQUIRED BLOCK PERAMETERS, ELSE RETURNS NIL
	
	(defun ReturnDynamicPropertyValue ( blk prp )
		(setq prp (strcase prp))
		(vl-some
			'(lambda ( x )
				(if (= prp (strcase (vla-get-propertyname x)))
					(vlax-get x 'value)
				)
			)
			(vlax-invoke blk 'getdynamicblockproperties)
		)
	)
	;; RETURN DYNAMIC PROPERTY VALUE IF IT EXISTS, ELSE NIL
	
	(defun CalculateAnchorPoint ( blk prm / x y ins )
		(if (= (type blk) 'VLA-OBJECT)
			(progn
				(setq ins (std:Variant->List (vla-get-insertionpoint blk)))
				(setq x (ReturnDynamicPropertyValue blk (strcat prm " X")))
				(setq y (ReturnDynamicPropertyValue blk (strcat prm " Y")))
				(if (and x y)
					(list (+ (car ins) x) (+ (cadr ins) y) (caddr ins))
					ins
				)
			)
		)
	)
	;; RETURNS THE POINT PARAMETER "PRM" WITHIN A PASSED BLOCK, ELSE RETURNS THE BLOCKS INSERTION POINT IF "PRM" DOES NOT EXIST
	;; POINT PARAMETERS CONTAIN AN "X" AND "Y" VALUE WHICH IS WHY THE PRM VALUE IS USED WITH "X" AND "Y" TO CALCULATE THE POINT
	;; "PRM" IS CASE SENSITIVE | IF "BLK" IS NOT A BLOCK, FUNCTION RETURNS NIL

	(defun CalculateCornerPoint ( pt mpt / x y dst ang )
		(setq dst 42.0) ;; DEFAULT LENGTH FROM POINT VALUE
		(cond
			((and (>= (car mpt) (car pt)) (>= (cadr mpt) (cadr pt))) ; Quad 1: X+, Y+
				(setq ang (/ pi 4.0))
			) ; 45 degrees
			((and (< (car mpt) (car pt)) (>= (cadr mpt) (cadr pt))) ; Quad 2: X-, Y+
				(setq ang (* 3.0 (/ pi 4.0)))
			) ; 135 degrees
			((and (< (car mpt) (car pt)) (< (cadr mpt) (cadr pt))) ; Quad 3: X-, Y-
				(setq ang (* 5.0 (/ pi 4.0)))
			) ; 225 degrees
			( t ; Quad 4: X+, Y-
				(setq ang (* 7.0 (/ pi 4.0)))
			) ; 315 degrees
		)
		(polar pt ang dst)
	)
	;; RETURN SECONDARY POINT FROM PASSED POINT LOCATION DEPENDING ON MOUSE DIRECTION
	
	(defun GetFlippedPoint ( pt1 pt2 mpt / dx dy )
		(setq dx (abs (- (car pt2) (car pt1))))
		(list 
			(if (< (car mpt) (car pt1)) (- (car pt1) dx) (+ (car pt1) dx))
			(cadr pt2)
			0.0
		)
	)
	;; FLIPS X AXIS POINT BASED ON MOUSE LOCATION
	;;; KEPT HERE TEMPORARILY INCASE ONLY THE X AXIS NEEDS TO FLIP
	;;; REPLACE gr:GetFlippedPoint WITH GetFlippedPoint , THIS IS A CUSTOM FUNCTION SPECIFIC TO THIS FILE
	
	(defun CalculateJagLine ( pt1 pt2 / dx dy )
		(if (or
				(equal (car pt1) (car pt2) 1e-8) 
				(equal (cadr pt1) (cadr pt2) 1e-8)
			)
			nil ;; JAG LINE NOT REQUIRED
			(progn
				(setq
					dx (- (car pt2) (car pt1)) ;; Horizontal delta
					dy (- (cadr pt2) (cadr pt1)) ;; Vertical delta
				)
				(if (> (abs dx) (abs dy))
					(std:FlattenList2D
						(list
							(polar pt1 (if (> dx 0) 0.0 pi) (/ (abs dx) 2.0))
							(polar pt2 (if (> dx 0) pi 0.0) (/ (abs dx) 2.0))
						)
					) ;; CASE: Horizontal Split (X)
					(std:FlattenList2D
						(list
							(polar pt1 (if (> dy 0) (* 0.5 pi) (* 1.5 pi)) (/ (abs dy) 2.0))
							(polar pt2 (if (> dy 0) (* 1.5 pi) (* 0.5 pi)) (/ (abs dy) 2.0))
						)
					) ;; CASE: Vertical Split (Y)
				)
			)
		)
	)
	; (std:FlattenList2D (list pt1 pt2)) ;; FLATTEN REFERENCE
	;; CALCULATES INTERMEDIATE POINTS EITHER IN THE X AXIS OR Y AXIS IF THE PASSED POINTS X & Y VALUES VARY
	;; RETURNS A FLATTENED LIST OF POINT VALUES BETWEEN PASSED POINTS PT1 AND PT2
	
	(defun PointTest ( pt )
		(entmake (list (cons 0 "POINT") (cons 10 pt)))
		(setq bfr "") ;; CLEAR STRING BUFFER
	)
	;; TESTING FUNCTION ONLY ;;; DELETE BEFORE PRODUCTION READY ;;;
	
	(defun INVC:CreateDialogForm ( )
		(if (and
				(= (type dcl) 'USUBR)
				(setq tmp (vl-filename-mktemp "INVC" nil ".dcl"))
				(setq des (open tmp "w"))
				(foreach line (dcl) (write-line line des))
				(not (close des))
				(> (setq dch (load_dialog tmp)) 0)
				(new_dialog ttl dch)
			)
			dch
		)
	)
	;; GLOBAL FUNCTION USED TO UPDATE DIALOG BOX WHENEVER USER MAKES AN UPDATE TO THE SETTINGS
	
	(defun INVC:DialogUpdates ( )
		(setq
			cbl (get_tile "cbl")
			ipx (get_tile "ipx")
			acm (get_tile "acm") ;; AUTO-CABLE MODE
			spl (get_tile "spl")
			plr (get_tile "plr")
			inc (rtos (atof (get_tile "inc")) 2 1)
			
			;rtf nil
		)
		(if (not (vl-every '(lambda ( v ) (> (strlen v) 0)) (list cbl acm ipx)))
			(progn (set_tile "error" "Error: All fields must be filled.") nil)
			t
		)
	)
	
	(vl-load-com)
	
	(princ "InverterCables: In progress...")
	;; MESSAGE TESTING ;;
	
	(setq ttl "Inverter_Cables")
	(defun dcl ( )
		(list
			"// Temporary DCL file;"
			(strcat ttl " : dialog {")
			"	label = \"Inverter Cables Dialog Control\";"
			"	: row {"
			"		: column {"
			"			label = \"Cable Information\";"
			"			: popup_list {"
			"				label = \"Cable Type:\";"
			"				key = \"cbl\";"
			"				width = 20;"
	(strcat "				value = \"" (if (null cbl) "" cbl) "\";")
			"			}"
			"			: edit_box {"
			"				label = \"Inverter Tag Prefix:\";"
			"				key = \"ipx\";"
			"				edit_width = 20;"
	(strcat	"				value = \"" (if (null ipx) "" ipx) "\";")
			"			}"
			"		}"
			"		spacer_1;"
			"		: column {"
			"			label = \"Cable Geometric Details\";"
			"			: text {"
			"				key = \"inf\";"
	(strcat "				label = \"" (if (or (null acm) (= acm "0")) "OFF = Click to add vertices - User Defined Cables." "ON = Select blocks - Automatic Cables.") "\";")
			"			}"
			"			: row {"
			"				: toggle {"
			"					label = \"Auto-Cable Mode:\";"
			"					key = \"acm\";"
	(strcat	"					value = \"" (if (null acm) "0" acm) "\";")
			"				}"
			"				: toggle {"
			"					label = \"Add Splice\";"
			"					key = \"spl\";"
	(strcat "					value = \"" (if (null spl) "0" spl) "\";")
			"				}"
			"				: column {"
			"					: toggle {"
			"						label = \"Polar Mode:\";"
			"						key = \"plr\";"
	(strcat	"						value = \"" (if (null plr) "0" plr) "\";")
			"					}"
			"					: edit_box {"
			"						label = \"Polar Tracking Angle (Degrees):\";"
			"						key = \"inc\";"
	(strcat "						value = \"" (if (or (null inc) (not (<= 0.0 (atof inc) 90.0))) "60.0" inc) "\";")
	(strcat "						is_enabled = " (if (or (null plr) (= plr "0")) "false" "true") ";")
			"					}"
			"				}"
			"			}"
			"		}"
			"	}"
			"	spacer_1;"
			"	errtile;"
			"	ok_cancel;"
			"}"
		)
	)
	;; ADD RADIO BUTTONS FOR CONNECTION TYPE TO INVERTER BLOCK (TOP / BOTTOM)
	
	(if (= 1 (getvar 'cvport))
		(prompt "\nFunction only applicable to model space.")
		(if (and
				(setq dch (INVC:CreateDialogForm))
				(setq run t)
				(setq dyn (getvar 'DYNMODE))
			) ;; INITIALIZE DIALOG BOX / SETTINGS
			(while run
				(start_list "cbl")
				(setq nms (mapcar 'add_list (mapcar 'car (setq prs (ReturnCableLayerList "E-")))))
				(end_list)
				
				(action_tile "acm" "(set_tile \"inf\" (if (= $value \"1\") \"ON = Select blocks - Automatic Cables.\" \"OFF = Click to add vertices - User Defined Cables.\"))")
				(action_tile "plr" "(mode_tile \"inc\" (if (= $value \"1\") 0 1))")
				
				(action_tile "accept" "(if (INVC:DialogUpdates) (done_dialog 1))")
				(action_tile "cancel" "(setq run nil) (done_dialog 0)")
				
				(setq sts (start_dialog))
				(unload_dialog dch)
				
				(if (= sts 1)
					(progn
						(setq ilp t bfr "") ;; INITIALIZE INSERT LOOP LOGIC, KEYBOARD STRING BUFFER AND PICK-POINT FLAG FOR INITIAL BLOCK SELECTION
						(princ "\nCable loop in progress: Select inverter block to draw cables | [K] = Dialog Options | [ESC] / [ENTER] = Terminate Command")
						
						(setq lnm (car (mapcar '(lambda ( x ) (nth x nms)) (read (strcat "(" cbl ")")))))
						;; LOGIC CHECKS HERE
						
						(while ilp
							(setq grd (grread t 15 2)) ;; DISPLAYS PICK-BOX
							(setq cod (car grd) dat (cadr grd))
							; (princ "\nCode: ") (princ cod) (princ " | Data: ") (princ dat) ;; TESTING ONLY
							(cond
								((= cod 5) ;; MOUSE MOVEMENT | TRACKING LOGIC ONLY
									(redraw) ;; CLEAR PREVIOUS FRAMES
									(if bpt (setq dat (gr:PolarMode bpt dat (if (= plr "1") (atof inc) 90.0))))
									(if (and cr1 gst (listp dat) (null ptl) (null bl2))
										(progn
											(setq cr1 (gr:GetFlippedPoint ac1 cr1 dat))
											(setq lst
												(list
													(car ac1) (cadr ac1) ;; Point 1: Anchor
													(car cr1) (cadr cr1) ;; Point 2: 45° Corner
													;(car dat) (cadr dat) ;; Point 3: Snapped Cursor (Ortho/Polar)
												)
											)
											(vlax-put gst 'Coordinates lst)
											(setq bpt cr1)
										)
									)
								)
								((= cod 3) ;; LEFT CLICK
									(setq spt dat)
									
									(if bpt
										(setq bpt (gr:PolarMode bpt dat (if (= plr "1") (atof inc) 90.0)))
										(if cr1
											(setq bpt (gr:PolarMode spt cr1 (if (= plr "1") (atof inc) 90.0)))
											(setq bpt dat)
										)
									) ;; CONSTRAINS TO 90 DEGREES OR POLAR SET DEGREES
									
									(cond
										;; CASE A: NO START BLOCK YET - Try to set bl1
										((null bl1)
											(if (null (setq bl1 (ReturnBlockObject bpt)))
												(princ "\nPlease select a starting block for cable logic (Inverter).")
												(progn
													(if (setq ac1 (CalculateAnchorPoint bl1 "MV Compartment"))
														(setq gst (CreateLineObject (list ac1 (setq cr1 (CalculateCornerPoint ac1 dat))) lnm 10))
													)
													(princ "\nGhost Line Value: ") (princ gst)
												)
											)
											;; ADD LOGIC TO DETERMINE WHICH BLOCKS COUNT (INVERTER FOR FIRST BLOCK SELECTION, INVERTER OR SUBSTATION FOR SECOND BLOCK SELECTION
										)

										;; CASE B: START BLOCK EXISTS - Handle acm and bl2
										( bl1
											(if (= acm "1")
												;;--- ACM MODE: Only allow block-to-block ---
												(if (setq bl2 (ReturnBlockObject spt))
													(progn
														(princ "\nEnd block targeted! Drawing direct connection...")
														;; DETERMINE TOP OR BOTTOM CONNECTION TO BLOCK BASED ON USER CONNECTION
														
														;; [Insert Logic: Draw line from bl1 to bl2]
														(if (setq ac2 (CalculateAnchorPoint bl2 "MV Compartment"))
															(progn
																;; 1. Calculate the final corner leading into the second block
																(setq cr2 (CalculateCornerPoint ac2 dat))
																
																(if (setq jpt (CalculateJagLine cr1 cr2))
																	(setq ptl (append ptl jpt))
																)
																
																;; 2. Update the existing 'gst' with BOTH the corner and the final anchor
																(vlax-put gst 'Coordinates 
																	(append 
																		(apply 'append (std:FlattenList2D (list ac1 cr1)))
																		(apply 'append ptl)
																		(apply 'append (std:FlattenList2D (list cr2 ac2)))
																	)
																)
																
																;; 3. Finalize visual and clear pointer
																(vla-update gst)
																(setq gst nil)
																
																(setq bpt ac2)
																(setq ac1 ac2)
																(setq gst (CreateLineObject (list ac1 (setq cr1 (CalculateCornerPoint ac1 dat))) lnm 10))
																
																(setq bl1 bl2 bl2 nil jpt nil ptl nil) ;; RESET FOR NEXT RUN

																(princ "\nConnection to MV Compartment complete.")
															)
														)
													)
													(princ "\nACM Mode: Please select a valid end block for cable logic (Inverter / Substation).")
												)

												;;--- MANUAL MODE: Collect points UNTIL a block is hit ---
												(if (setq bl2 (ReturnBlockObject spt))
													(progn
														(princ "\nEnd block hit! Finalizing path...")
														;; DETERMINE TOP OR BOTTOM CONNECTION TO BLOCK BASED ON USER CONNECTION
														
														;(setq ptl (append ptl (std:FlattenList2D (list bpt)))) ;; MIGHT NEED TO CHANGE THIS TO CALC BLOCK ENTRY POINT ;;; BOOKMARK ;;;
														
														(princ (strcat "\nTotal vertices: " (itoa (length ptl))))
														;(setq bl1 nil bl2 nil ptl nil) ;; Reset everything
														
														(if (setq ac2 (CalculateAnchorPoint bl2 "MV Compartment"))
															(progn
																;; 1. Calculate the final corner leading into the second block
																(setq cr2 (CalculateCornerPoint ac2 dat))
																
																(if (setq jpt (CalculateJagLine (if ptl (last ptl) cr1) cr2))
																	(setq ptl (append ptl jpt))
																)
																
																;; 2. Update the existing 'gst' with BOTH the corner and the final anchor
																(vlax-put gst 'Coordinates 
																	(append
																		(apply 'append (std:FlattenList2D (list ac1 cr1)))
																		(apply 'append ptl)
																		(apply 'append (std:FlattenList2D (list cr2 ac2)))
																	)
																)
																
																;; 3. Finalize visual and clear pointer
																(vla-update gst)
																(setq gst nil)
																
																(setq bpt ac2)
																(setq ac1 ac2)
																(setq gst (CreateLineObject (list ac1 (setq cr1 (CalculateCornerPoint ac1 dat))) lnm 10))
																
																(setq bl1 bl2 bl2 nil jpt nil ptl nil) ;; RESET FOR NEXT RUN

																(princ "\nConnection to MV Compartment complete.")
															)
														)
													)
													(progn
														(if gst
															(vlax-put gst 'Coordinates 
																(append
																	(std:Variant->List (vla-get-coordinates gst))
																	(apply 'append (std:FlattenList2D (list bpt)))
																)
															)
														)
														(setq ptl (append ptl (std:FlattenList2D (list bpt))))
														(princ "\nPoint added to path. Pick next point or select end block (Inverter / Substation).")
													)
												)
											)
										)
									)
									
									(PointTest bpt)
									;;; BOOKMARK ;;;
									;; TESTING POINT LOCATION LOGIC
									
								)
								((= cod 2) ;; KEYBOARD INPUT
									(if (not (= acm "1"))
										(cond
											((member dat (mapcar 'ascii (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))) ;; DISTANCE TYPED IN KEYBOARD LOGIC
												(setq bfr (strcat bfr (chr dat)))
												(princ (strcat "\rCable Length: " bfr " | Press [ENTER] to accept."))
											) ;; LOGIC FOR ALLOWING USER TO TYPE DISTANCE STRING FROM KEYBOARD SAVING VALUE TO A BUFFER
											((= dat 8) ;; BACKSPACE LOGIC
												(if (> (strlen bfr) 0)
													(progn
														(setq bfr (substr bfr 1 (1- (strlen bfr))))
														(princ (strcat "\rCable Length: " bfr " | Press [ENTER] to accept."))
													) ;; CLEAR THE LAST CHARACTER THEN RE-PRINT BUFFER
													(princ "\rEnter a value for distance.")
												)
											)
										)
									)
									(cond
										((= dat 13) ;; ENTER PRESS
											(if (and (not (null bpt)) (/= bfr ""))
												(progn
													(setq mpt (cadr (grread t)))
													(setq dst (distof bfr))
													(setq bpt (polar bpt (angle bpt (gr:PolarMode bpt mpt (if (= plr "1") (atof inc) 90.0))) dst))
													(princ (strcat "\rCable Length: " bfr))
													
													;; UPDATE LINE "GST"
													(if gst
														(progn
															;(if (null ptl) (vlax-put gst 'Coordinates (list (car ac1) (cadr ac1) (car cr1) (cadr cr1))))
															(setq ptl (append ptl (std:FlattenList2D (list bpt))))
															(vlax-put gst 'Coordinates
																(append
																	(std:Variant->List (vla-get-coordinates gst))
																	(apply 'append (std:FlattenList2D (list bpt)))
																)
															)
														) ;; FLATTEN LIST TO X AND Y VALUES ONLY
													)
													(PointTest bpt)
												) ;; BUFFER CHECK - CALCULATE DISTANCE FROM LAST POINT TO TYPED POINT AND DRAW CABLE LINE FOR TOTAL DISTANCE
												(progn
													(if gst (vla-delete gst))
													(setq ilp nil run nil)
												) ;; IF BUFFER IS EMPTY - [ENTER] KEY (CANCEL FUNCTION)
											)
										)
										((or (= dat (ascii "K")) (= dat (ascii "k")))
											(setq ilp nil)
											(if (and (eq (type tmp) 'STR) (setq tmp (findfile tmp)))
												(vl-file-delete tmp)
											)
											(setq dch (INVC:CreateDialogForm))
										)
										((= dat 27) ;; [ESC] KEY (CANCEL FUNCTION)
											(setq ilp nil run nil)
										)
									)
								)
								((member cod '(13 25)) ;; ENTER / RIGHT CLICK (CANCEL FUNCTION)
									(setq ilp nil run nil)
								)
							)
						)
					)
				)
			)
		)
	)
	
	
	;; NOTES:
	;; DIALOG POP-UP FOR CABLE DATA (CABLE TYPE) WHICH SHOULD INCLUDE INFORMATION FOR THE FOLLOWING:
	;;	- CONNECTION LOCATION DEFAULT (TOP, BOTTOM, LEFT, RIGHT) COMBINATIONS OR BASED ON DIRECTION
	;; 	- CABLE TYPE (TAG NAME) AND LAYER
	;;	- ALLOW USER DEFINED LINEWORK OR AUTOMATIC LINEWORK (AUTOMATIC IS BASED ON CLICKING THE INVERTER BLOCKS AND AUTOMATICALLY DRAWING THE BEST FITTING LINE)
	;;	- VALUE FOR POLAR TRACKING (THIS WILL NEED TO CODED IN SINCE GRREAD DOES NOT ALLOW TRACKING AUTOMATICALLY)
	;; THIS SELECTION WILL BE USED TO DETERMINE LAYER OF THE CABLE
	;; ALL CABLES TO BE DRAWN TO THE MV COMPARTMENT LOCATION WITHIN THE INVERTER BLOCK
	;; COMPARTMENT LOCATION IS GOING TO NEED TO BE CALCULATED BY ADDING (OR SUBTRACTING) INSERTION POINT TO COMPARTMENT LOCATION WITHIN BLOCK
	;; COUNT INVERTERS FROM LEFT OF SUBSTATION FOR NO. OF TYPES FIELDS, WILL NEED TO "TRACE" FROM END OF LINE SEGMENT TO SUBSTATION TO CALCULATE BOTH TYPE FIELDS
	;; "TO" AND "FROM" FIELDS TO BE DETERMINED BY THE CONNECTED BLOCK TYPES TAGS (THIS EXCLUDES SPLICE LOCATIONS I BELIEVE, WILL NEED TO CONFIRM WITH GREGG)
	;; CROSSING NO. WILL NEED TO BE DETERMINED BY CROSSED LINE TYPES OF A CERTAIN VALUE, ROADWAYS, RIVERS ETC. AND WILL NEED TO COUNT CROSSINGS IN PAIRS FROM BEGINNING TO END
	;; FOR "CROSSING NO." WILL NEED TO DETERMINE IF THE NUMBER STARTS FROM THE BEGINNING OF THE OVERALL LINE SEGMENT OR IF THIS NEEDS TO BE A RUNNING TOTAL FOR ALL CABLES
	;; AND INDEXED AT CERTAIN POINTS / CABLE DATA
	;; CABLE CROSSING LENGTH CAN BE DETERMINED BY CALCULATING THE TOTAL LENGTH FROM CROSSING ROADWAY / RIVER LAYER AT FIRST AND SECOND POINTS
	;; WILL ALSO NEED A RUNNING TOTAL FOR THIS CALCULATION AS WELL
	;; WILL NEED TO ADD A "CABLE DATA CHECK" LOGIC THAT WILL ALLOW THE USER TO SEE THE DATA WITHIN SELECTED CABLES OR ALL THE CABLES AT ONCE
	;; THERE SHOULD ALSO BE AN OPTION TO ALLOW THE USER TO CYCLE THROUGH CABLES WHERE DATA IS EITHER EMPTY OR DOES NOT MATCH BLOCK INFORMATION
	;; ADDITIONALLY, THERE SHOULD BE A SEPERATE LOGIC TO "SYNC" THE CABLE DATA WHICH SHOULD DO THE FOLLOWING ACTIONS:
	;; 	- ALLOWS THE CABLE LINES TO EXTEND TO THE NEAREST INVERTER IF THEY ARE NOT CONNECTED
	;; 	- UPDATES CABLE TAG INFORMATION BASED ON INVERTER / SUBSTATION BLOCK INFORMATION (IF CHANGED)
	;;	- RECALCS THE FOLLOWING: NO. OF INVERTER TYPES (1 & 2), CROSSING NO.'S / LENGTHS, CHECKS CABLE TYPE (LAYER), UPDATES "TO" AND "FROM" FIELDS BASED ON INVERTER BLOCKS
	;; THE SYNC FUNCTION SHOULD BE RUN AUTOMATICALLY BEFORE SENDING DATA TO THE ECHO PLATFORM (MV CALCS DATA)
	
	
	(redraw) ;; CLEAR FRAMES
	
	(if (findfile tmp)
		(vl-file-delete tmp)
	)
	
	(princ)
)



;;; SYNC FUNCTIONS ;;;
;; BOOKMARK: ADD INVERTER RE-NUMBERING FUNCTION

(defun c:INVCS ( )
	(c:SyncInverterCables)
)

(defun c:SyncInverterCables
	(
		/
		;Functions
		
		;Variables
		
	)
	(princ "SyncInverterCables: Not available yet.")
	
	
	(princ)
)