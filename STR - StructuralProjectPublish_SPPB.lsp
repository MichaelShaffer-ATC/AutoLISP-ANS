;; THIS FUNCTION ALLOWS FOR THE 'QUICK' PUBLISH OF AN ANS PROJECT
;; THE CODE EXECUTES A BACKGROUND PLOTTING PROCESS SIMILAR TO WHAT IS CALLED WHEN THE 'PUBLISH' FUNCTION IS USED WITHIN CAD
;; A TEMPORARY .DSD FILE IS CREATED TO ALLOW AUTOCAD TO FINALIZE HOW THE PLOT SHOULD BE SET UP

(defun c:SPPB ( )
	(c:STRUCTURALPROJECTPUBLISH)
)


(defun c:SPQPB ( )
	(c:STRUCTURALQUICKPUBLISH)
)


(defun c:STRUCTURALQUICKPUBLISH ( / dir OrderedLayoutListNames )
	;; FIND CLIENT FOLDER LOCATION AND PASS DIRECTORY TO WRAPPER
	
	(defun OrderedLayoutListNames ( / map )
		(vlax-for lyt (vla-get-layouts (vla-get-activedocument (vlax-get-acad-object)))
			(setq map (cons (cons (vla-get-taborder lyt) (vla-get-name lyt)) map))
		)
		(cdr (mapcar 'cdr (vl-sort map '(lambda ( a b ) (< (car a) (car b))))))
	)
	;; RETURNS A LIST OF LAYOUT NAMES IN ORDER OF THEIR RESPECTIVE TAB NUMBER
	;; IGNORES 'MODEL SPACE'
	
	(setq dir
		(GetSubfolderPathInTarget 
			(getvar "DWGPREFIX")
			"CAD"
			"Individual PDF's"
			(strcat "C:\\Users\\" (getvar "LOGINNAME") "\\ANS Geo\\ANS Geo Projects - Documents\\3 - PROJECTS\\")
		)
	)
	(PUBLISHWRAPPER
		dir
		(list (cons (getvar "DWGNAME") (OrderedLayoutListNames)))
		nil
		t
	)
	
	(princ)
)
;; IMMEDIATELY SENDS PARAMETERS TO THE WRAPPER FUNCTION BYPASSING THE DIALOG BOX
;; AUTOMATICALLY CREATES A SINGLE PDF FOR THE PROJECT WITHIN THE PROJECTS CURRENT DIRECTORY


(defun c:STRUCTURALPROJECTPUBLISH
	( 
		/ 
		;Functions
		*error*
		OrderedLayoutListNames
		WSH:ReturnFolder WSH:ConfirmationDialog
		;DCL Functions
		DCL:RunDialogChecks DCL:Cancel DBG:LogTesting DCL:FinishDialog DCL:ResetErrorTile
		DCL:BrowseFolder DCL:AddToPlotList DCL:ClearPlotList
		DCL:CurrentDirectoryToggle DCL:IndividualPlotToggle DCL:UseDrawingFileNameToggle DCL:OpenFileLocationToggle
		DCL:LayoutList
		;DCL (Global Variables)
		out_dir cur_dir fnm pfx mlt ofl lyts fls
		;Variables
		dir ttl dcl des dch res err
	)
	;; SHOW DIALOG BOX OPTIONS HERE AND SEND RETURN VALUES TO WRAPPER FUNCTION
	;; WRAPPER FUNCTION HOSTS ALL FUNCTIONS REQUIRED FOR PUBLISH, THIS FUNCTION IS ONLY USED FOR USER SELECTION / DIALOG CONTROLS
	
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
		(strcat "\nError: " msg)
	)
	;; ERROR FUNCTION IS SPECIFIC TO THE DIALOG CONTROL
	
	
	(defun OrderedLayoutListNames ( / map )
		(vlax-for lyt (vla-get-layouts (vla-get-activedocument (vlax-get-acad-object)))
			(setq map (cons (cons (vla-get-taborder lyt) (vla-get-name lyt)) map))
		)
		(cdr (mapcar 'cdr (vl-sort map '(lambda ( a b ) (< (car a) (car b))))))
	)
	;; RETURNS A LIST OF LAYOUT NAMES IN ORDER OF THEIR RESPECTIVE TAB NUMBER
	;; IGNORES 'MODEL SPACE'
	
	
	(defun WSH:ReturnFolder ( ttl flg / shll fldr pth )
		(vl-catch-all-apply
			'(lambda ( )
				(if (and
						(setq shll (vla-getinterfaceobject (vlax-get-acad-object) "Shell.Application"))
						(setq fldr (vlax-invoke-method shll 'BrowseForFolder (vlax-get (vlax-get-acad-object) 'HWND) ttl flg))
					)
					(setq pth (vlax-get-property (vlax-get-property fldr 'Self) 'Path))
				)
			)
		)
		(foreach obj (list shll fldr)
			(if (and (eq (type obj) 'VLA-OBJECT) (not (vlax-object-released-p obj)))
				(vlax-release-object obj)
			)
		)
		pth
	)
	;; RETURNS SELECTED FOLDER FROM BROWSE MENU DIALOG BOX USING WINDOWS SCRIPT HOSTING
	;;
	;; Shell.Application BrowseForFolder Method Reference
	;; --------------------------------------------------
	;; Syntax:
	;; (vlax-invoke-method shell 'BrowseForFolder hwnd title options)
	;;
	;; Parameters:
	;; - hwnd: Window handle (can be 0)
	;; - title: Title displayed in the dialog
	;; - options: Integer value combining flags listed below
	;;
	;; Folder Dialog Flags:
	;; --------------------
	;; 0      BIF_RETURNONLYFSDIRS
	;;        Only return file system directories.
	;;
	;; 1      BIF_DONTGOBELOWDOMAIN
	;;        Prevent browsing below domain level.
	;;
	;; 2      BIF_STATUSTEXT
	;;        Display status text area under tree view.
	;;
	;; 4      BIF_RETURNFSANCESTORS
	;;        Return file system ancestors (e.g., Desktop).
	;;
	;; 8      BIF_EDITBOX
	;;        Include an edit box for manual path entry.
	;;
	;; 16     BIF_VALIDATE
	;;        Validate the path entered in the edit box.
	;;
	;; 32     BIF_NEWDIALOGSTYLE
	;;        Use the newer dialog style (resizable, "New Folder" button).
	;;
	;; 64     BIF_BROWSEINCLUDEURLS
	;;        Include URLs in the tree view.
	;;
	;; 128    BIF_USENEWUI
	;;        Combines BIF_NEWDIALOGSTYLE and BIF_EDITBOX.
	;;
	;; 256    BIF_UAHINT
	;;        Add user assistance hint text.
	;;
	;; 512    BIF_NONEWFOLDERBUTTON
	;;        Remove the "New Folder" button.
	;;
	;; 1024   BIF_NOTRANSLATETARGETS
	;;        Do not resolve symbolic links or shortcuts.
	;;
	;; 2048   BIF_BROWSEFORCOMPUTER
	;;        Allow browsing for computers.
	;;
	;; 4096   BIF_BROWSEFORPRINTER
	;;        Allow browsing for printers.
	;;
	;; 8192   BIF_BROWSEINCLUDEFILES
	;;        Include files in the tree view.
	;;
	;; 16384  BIF_SHAREABLE
	;;        Allow selection of shared folders.
	;;
	;; Combine flags using addition:
	;; Example: (+ 32 8) = BIF_NEWDIALOGSTYLE + BIF_EDITBOX
	;;
	;; References:
	;; https://learn.microsoft.com/en-us/windows/win32/shell/shell-browseforfolder
	;; https://learn.microsoft.com/en-us/windows/win32/api/shlobj_core/nf-shlobj_core-shbrowseforfoldera
	;; https://www.pinvoke.net/default.aspx/shell32.SHBrowseForFolder
	
	
	(defun WSH:ConfirmationDialog ( msg ttl tim flg / shll res )
		(vl-catch-all-apply
			'(lambda ( )
				(if (setq shll (vla-getinterfaceobject (vlax-get-acad-object) "WScript.Shell"))
					(setq res (vlax-invoke-method shll 'Popup msg tim ttl flg))
				)
			)
		)
		(if (and (eq (type shll) 'VLA-OBJECT) (not (vlax-object-released-p shll)))
			(vlax-release-object shll)
		)
		res
	)
	;; RETURNS INTEGER VALUE OF SELECTION IN DIALOG BOX USING WINDOWS SCRIPT HOSTING
	;; OUTPUT: 6 == "YES", 7 == "NO"
	;;
	;; WScript.Shell Popup Method Reference
	;; ------------------------------------
	;; Syntax:
	;; (vlax-invoke-method shell 'Popup message timeout title type)
	;;
	;; Parameters:
	;; - message: Text to display in the pop-up
	;; - timeout: Time in seconds before auto-close (0 = wait indefinitely)
	;; - title: Title of the pop-up window
	;; - type: Numeric value combining button and icon styles
	;;
	;; Button Options:
	;; 0  = OK
	;; 1  = OK/Cancel
	;; 2  = Abort/Retry/Ignore
	;; 3  = Yes/No/Cancel
	;; 4  = Yes/No
	;; 5  = Retry/Cancel
	;;
	;; Icon Options:
	;; 16 = Stop (Error)
	;; 32 = Question
	;; 48 = Exclamation (Warning)
	;; 64 = Information
	;;
	;; Combine button + icon values using addition:
	;; Example: 4 (Yes/No) + 48 (Warning) = 52
	;;
	;; Return Values:
	;; - 1 = OK
	;; - 2 = Cancel
	;; - 3 = Abort
	;; - 4 = Retry
	;; - 5 = Ignore
	;; - 6 = Yes
	;; - 7 = No
	;; - -1 = Timed out (no response)

	;; https://www.vbsedit.com/html/f482c739-3cf9-4139-a6af-3bde299b8009.asp
	;; http://www.lee-mac.com/popup.html
	;; https://learn.microsoft.com/en-us/previous-versions/windows/internet-explorer/ie-developer/windows-scripting/x83z1d9f(v=vs.84)?redirectedfrom=MSDN
	;; VISIT FOR MSDN BITCODE INFORMATION
	
	(setq ttl "Publish_Project")
	(setq dcl
		(list
			"// Temporary DCL file;"
			(strcat ttl " : dialog {")
			"	label = \"Publish Structural Project Dialog Control\";"
			"	: text {"
			"		value = \"**DEFAULT FILE OUTPUT DIRECTORY IS SET TO `CAD/Individual PDF's` UNLESS DIRECTORY LOCATION IS CHANGED OR NOT FOUND**\";"
			"	}"
			"	: row {"
			"		: column {"
			"			: boxed_column {"
			"				label = \"File Path\";"
			"				: row {"
			"					: toggle {"
			"						label = \"Publish to current file directory\";"
			"						value = \"0\";"
			"						key = \"cur_dir\";"
			"					}"
			"				}"
			"				: row {"
			"					: edit_box {"
			"						label = \"File Output Directory: \";"
			"						alignment = \"left\";"
			"						edit_width = 25;"
			"						key = \"out_dir\";"
			"						is_enabled = true;"
			"					}"
			"					: button {"
			"						label = \"Browse\";"
			"						alignment = \"left\";"
			"						fixed_width = true;"
			"						key = \"brws\";"
			"						is_enabled = true;"
			"					}"
			"				}"
			"			}"
			"			: boxed_column {"
			"				label = \"File Name\";"
			"				: row {"
			"					: toggle {"
			"						label = \"Use drawing file name\";"
			"						value = \"1\";"
			"						key = \"same\";"
			"					}"
			"				}"
			"				: edit_box {"
			"					label = \"Output File Name: \";"
			"					value = \"\";"
			"					alignment = \"left\";"
			"					edit_width = 39;"
			"					key = \"fnm\";"
			"					is_enabled = false;"
			"				}"
			"			}"
			"			: row {"
			"				: toggle {"
			"					label = \"Publish each layout individually\";"
			"					alignment = \"left\";"
			"					value = \"0\";"
			"					key = \"mlt\";"
			"				}"
			"				: toggle {"
			"					label = \"Open file location\";"
			"					alignment = \"right\";"
			"					value = \"1\";"
			"					key = \"ofl\";"
			"				}"
			"			}"
			"		}"
			"		: boxed_column {"
			"			label = \"Layout Details\";"
			"			: list_box {"
			"				label = \"Available layout names: \";"
			"				multiple_select = true;"
			"				width = 24;"
			"				key = \"lyts\";"
			"			}"
			"			: row {"
			"				: spacer {"
			"					width = 1;"
			"				}"
			"				: button {"
			"					label = \"Add to File List\";"
			"					width = 12;"
			"					fixed_width = true;"
			"					key = \"add\";"
			"				}"
			"				: spacer {"
			"					width = 1;"
			"				}"
			"			}"
			"		}"
			"		: boxed_column {"
			"			label = \"Output File Details\";"
			"			: list_box {"
			"				label = \"Output File List\";"
			"				key = \"fls_lst\";"
			"				width = 24;"
			"				value = \"\";"
			"				is_enabled = true;"
			"			}"
			"			: row {"
			"				: spacer {"
			"					width = 1;"
			"				}"
			"				: button {"
			"					label = \"Clear File List\";"
			"					width = 12;"
			"					fixed_width = true;"
			"					key = \"clr\";"
			"				}"
			"				: spacer {"
			"					width = 1;"
			"				}"
			"			}"
			"		}"
			"	}"
			"	errtile;"
			"	ok_cancel;"
			"}"
		)
	)
	
	(if
		(and
			dcl
			(setq tmp (vl-filename-mktemp "PUBL" nil ".dcl"))
			(setq des (open tmp "w"))
			(foreach line dcl (write-line line des))
			(not (close des))
			(> (setq dch (load_dialog tmp)) 0)
			(new_dialog ttl dch)
		)
	
		(progn
			;; ONLY LOAD FUNCTIONS FOR DIALOG CONTROLS IF DIALOG BOX CAN BE CREATED
			(defun DCL:RunDialogChecks ( / )
				(cond
					;; ADD CONDITIONAL LOGIC TO CHECK FOR HERE
					((or (null out_dir) (not (vl-file-directory-p out_dir)))
						(set_tile "error" "Error: Invalid output directory.")
					)
					((or (null fnm) (= (strlen (vl-string-right-trim " " fnm)) 0))
						(set_tile "error" "Error: File name cannot be blank.")
					)
					((and (not mlt) fls (> (length fls) 0))
						(if (and lyts (> (length lyts) 0))
							(if (= (WSH:ConfirmationDialog "Some layouts have not been assigned a file name. Do you want to continue?" "Continue?" 0 (+ 48 4)) 6) ;; 6 == "YES", 7 == "NO"
								(DCL:FinishDialog)
							)
							(DCL:FinishDialog)
						)
					)
					( t
						(setq fls (cons (cons fnm lyts) fls))
						(DCL:FinishDialog)
					)
				)
			)
			;; RUNS INNER FUNCTION DIALOG BOX CHECKS FOR ERRORS
			
			(defun DCL:Cancel ( / )
				(done_dialog 0)
				(if (findfile tmp)
					(vl-file-delete tmp)
				)
			)
			;; CLEANS UP BACKEND IF PROGRAM IS CANCELED
			
			(defun DBG:LogTesting ( / )
				(alert
					(strcat
						"\n"
						"Directory: " out_dir "\n"
						"File name: " fnm "\n"
						"File prefix: " (strcat fnm " - ") "\n"
						"Layouts: " (apply 'strcat (mapcar '(lambda ( f ) (strcat (car f) ": " (apply 'strcat (mapcar '(lambda ( str ) (strcat str " ")) (cdr f))) "\n")) fls))
						"Files: " (apply 'strcat (mapcar '(lambda ( str ) (strcat str " ")) (mapcar 'car fls))) "\n"
						"Multiple output: " (if mlt "True" "False") "\n"
						"Open explorer: " (if opn "True" "False") "\n"
					)
				)
			)
			;; OUTPUT LOG FOR TESTING PURPOSES ONLY
			
			(defun DCL:FinishDialog ( / )
			;;	(DBG:LogTesting)
				(done_dialog 1)
				
				(setq fls (mapcar '(lambda ( p ) (cons (strcat (car p) ".dwg") (cdr p))) fls))
				;; ADDS ".dwg" TO THE FILE NAMES IN THE LIST

				(setq
					$glb:out_dir* out_dir
					$glb:fls* fls
					$glb:mlt* mlt
					$glb:opn* opn
				)
				;; STORE USER SELECTIONS GLOBALLY FOR PUBLISH WRAPPER
				
				(if (findfile tmp)
					(vl-file-delete tmp)
				)
			)
			;; COMPLETES DIALOG FUNCION AND PASSES PARAMETERS TO GLOBAL WRAPPER FUNCTION LATER
			
			(defun DCL:ResetErrorTile ( / )
				(if (/= (get_tile "error") "")
					(set_tile "error" "")
				)
			)
			;; RESETS ERROR TILE, USED FOR CONDITIONAL STATEMENTS AND CLEAN UP PROCESSES
			
			;;;BUTTONS;;;
			(defun DCL:BrowseFolder ( key / )
				(cond
					((= key "brws")
						(if (setq out_dir (WSH:ReturnFolder "File Output Directory:" 512))
							(progn
								(setq out_dir (strcat out_dir "\\"))
								(set_tile "out_dir" out_dir)
							)
						)
					)
					( t
						(set_tile "error" "Invalid key value was passed. Check source code.")
					)
				)
			)
			;; VARIABLES NEED TO BE MAINTAINED IN GLOBAL SCOPE FOR OVERALL FUNCTION
			
			(defun DCL:AddToPlotList ( / slct )
				(cond
					((or (null fnm) (= (strlen (vl-string-right-trim " " fnm)) 0))
						(set_tile "error" "Error: File name cannot be blank.")
					)
					((or (null (get_tile "lyts")) (= (strlen (get_tile "lyts")) 0))
						(set_tile "error" "Error: No layouts selected for file assignment.")
					)
					((assoc fnm fls) ;; SEARCH FOR KEY (FILE NAME) IN FILES LIST
						(set_tile "error" "Error: Duplicate file name found. Please change the file name or clear the selection.")
					)
					( t
						(DCL:ResetErrorTile)
						(setq slct (mapcar '(lambda ( i ) (nth i lyts)) (read (strcat "(" (get_tile "lyts") ")"))))
						(setq fls (cons (cons fnm slct) fls))
						(setq lyts (vl-remove-if '(lambda ( lyt ) (member lyt slct)) lyts))
						(start_list "lyts" 3)
						(foreach lyt lyts (add_list lyt))
						(end_list)
						
						(start_list "fls_lst")
						(foreach fl fls (add_list (car fl)))
						(end_list)
					)
				)
			)
			;; ADDS GROUPD LAYOUTS TO A SPECIFIC OUTPUT FILE AND REMOVES THE SELECTED LAYOUTS FROM THE LAYOUTS LIST
			
			(defun DCL:ClearPlotList ( / )
				(DCL:ResetErrorTile)
				(start_list "fls_lst" 3)
				(end_list) ;; CLEAR FILES LIST
				(setq lyts (OrderedLayoutListNames) fls nil)
				(DCL:LayoutList)
			)
			;; CLEARS ALL POTENTIAL OUTPUT FILES FROM THE LIST AND RESETS THE LAYOUT LIST BOX
			
			;;;TOGGLES;;;
			(defun DCL:CurrentDirectoryToggle ( / )
				(if (= (get_tile "cur_dir") "1")
					(progn
						(set_tile "out_dir" (getvar "DWGPREFIX"))
						(mode_tile "out_dir" 1)
						(mode_tile "brws" 1)
					)
					(progn
						(set_tile "out_dir" "")
						(mode_tile "out_dir" 0)
						(mode_tile "brws" 0)
					)
				)
				(setq out_dir (get_tile "out_dir"))
			)
			;; USE SOURCE DIRECTORY AS DESTINATION DIRECTORY (IF TOGGLED ON)
			
			(defun DCL:IndividualPlotToggle ( / )
				(if (= (get_tile "mlt") "1")
					(progn
						(setq mlt t)
						(DCL:ClearPlotList)
						(mode_tile "fls_lst" 1)
						(mode_tile "clr" 1)
						(mode_tile "lyts" 1)
						(mode_tile "add" 1)
					)
					(progn
						(setq mlt nil)
						(mode_tile "fls_lst" 0)
						(mode_tile "clr" 0)
						(mode_tile "lyts" 0)
						(mode_tile "add" 0)
					)
				)
			)
			;; TOGGLES ON MULTIPLE PLOT TO PUBLISH EACH SHEET INDIVIDUALLY
			;; TURNS OFF OPTIONS FOR ADDING FILES TO PLOT LIST OR SELECTING SPECIFIC LAYOUTS
			
			(defun DCL:UseDrawingFileNameToggle ( / )
				(if (= (get_tile "same") "1")
					(progn
						(set_tile "fnm" (vl-filename-base (getvar "DWGNAME")))
						(mode_tile "fnm" 1)
					)
					(mode_tile "fnm" 0)
				)
				(setq fnm (get_tile "fnm"))
			)
			;; TOGGLES POTENTIAL FILE NAME FOR OUTPUT FILE BASED ON THE DRAWINGS FILE NAME
			
			(defun DCL:OpenFileLocationToggle ( / )
				(if (= (get_tile "ofl") "1")
					(setq opn t)
					(setq opn nil)
				)
			)
			
			;;;LIST BOXES;;;
			(defun DCL:LayoutList ( / )
				(start_list "lyts")
				(foreach lyt lyts (add_list lyt))
				(end_list)
			)
			;; ADDS LAYOUT NAMES TO LIST BOX FOR USER SELECTION
			
			;;;MAIN;;;
			(setq lyts (OrderedLayoutListNames))
			(DCL:LayoutList)
			(set_tile "fnm" (setq fnm (vl-filename-base (getvar "DWGNAME"))))
			(set_tile "out_dir" (setq out_dir (GetSubfolderPathInTarget  (getvar "DWGPREFIX") "CAD" "Individual PDF's" (strcat "C:\\Users\\" (getvar "LOGINNAME") "\\ANS Geo\\ANS Geo Projects - Documents\\3 - PROJECTS\\"))))
			(DCL:OpenFileLocationToggle)
			;; INITIALIZE VARIABLES AND DEFAULT SETTINGS
			
			(action_tile "accept" "(DCL:RunDialogChecks)")
			(action_tile "cancel" "(DCL:Cancel)")
			
			(action_tile "cur_dir" "(DCL:CurrentDirectoryToggle)")
			(action_tile "out_dir" "(setq out_dir $value)")
			(action_tile "fnm" "(setq fnm $value)")
			(action_tile "brws" "(DCL:BrowseFolder $key)")
			(action_tile "add" "(DCL:AddToPlotList)")
			(action_tile "clr" "(DCL:ClearPlotList)")
			
			(action_tile "mlt" "(DCL:IndividualPlotToggle)")
			(action_tile "same" "(DCL:UseDrawingFileNameToggle)")
			(action_tile "ofl" "(DCL:OpenFileLocationToggle)")
			
			(action_tile "lyts" "(setq res $value)")
			
			(if (= (start_dialog) 1)
				(if (vl-catch-all-error-p (setq err (vl-catch-all-apply 'PublishAfterDialog)))
					(princ (strcat "An error occured: " (vl-catch-all-error-message err)))
				)
			)
			(unload_dialog dch)
		)
		(prompt "\nError loading dialog box.")
	)
	
	(princ)
)
;; CREATES DIALOG CONTROL FOR USER TO FILL OUT INFORMATION FOR PLOTTING


(defun PUBLISHWRAPPER
	(
		;Arguments
		dir fls mlt opn
		/
		;Functions
		*error*
		GetSysVariables SetSysVariables WriteDSDPlot
		;Variables
		svardef curr
	)
	
	(defun *error* ( msg )
		(if curr (SetSysVariables curr))
		(if (not (member msg (list "Function cancelled" "quit / exit abort")))
			(vl-bt)
		)
		(if (vl-file-directory-p dsd)
			(vl-file-delete dsd)
		)
		(princ)
	)
	
	(defun GetSysVariables ( vars )
		(mapcar 'cons vars (mapcar 'getvar vars))
	)
	;; GETS SYSTEM VARIABLES BASED ON LIST 'VARS'
	
	(defun SetSysVariables ( svardef )
		(mapcar 'setvar (mapcar 'car svardef) (mapcar 'cdr svardef))
	)
	;; SETS SYSTEM VARIABLES, REQUIRES A KEY VALUE PAIR LIST TO SET VALUES TO
	
	
	(defun WriteDSDPlot ( dir fnm pfx lyns mlt / dsd file )
		(if
			(and
				(setq dsd (strcat (getvar "DWGPREFIX") "TEMP.DSD"))
				(setq file (open (strcat (getvar "DWGPREFIX") "TEMP.DSD") "w"))
				(eq (type file) 'FILE)
			) ;; SETS PATH AND OPENS DSD FILE FOR WRITING (USED FOR PUBLISHING)
			(progn
				;;;DSD FILE HEADER;;;
				(write-line "[DWF6Version]" file)
				(write-line "Ver=1" file)
				(write-line "[DWF6MinorVersion]" file)
				(write-line "MinorVer=1" file)
				
				;;;LAYOUTS / DWG TO PLOT;;;
				(foreach lyt lyns
					(write-line (strcat "[DWF6Sheet:" pfx lyt "]") file) ;; CREATES FILE SHEET NAME
					(write-line (strcat "DWG=" (getvar "DWGPREFIX") (vl-filename-base (getvar "DWGNAME")) ".dwg") file)
					(write-line (strcat "Layout=" lyt) file) ;; LAYOUT TO PLOT
					(write-line "Setup=" file) ;; PAGE SETUP NAME TO USE (BLANK -> DEFAULT SETUP *TIED TO TEMPLATE)
					(write-line (strcat "OriginalSheetPath=" (getvar "DWGPREFIX") (vl-filename-base (getvar "DWGNAME")) ".dwg") file)
					(write-line "Has Plot Port=0" file)
					(write-line "Has3DDWF=0" file)
				)
				
				;;;DSD FILE FOOTER;;;
				(write-line "[Target]" file)
				(if (not (eq (strcase (vl-filename-extension fnm) 1) ".dwf"))
					(if mlt (write-line "Type=5" file) (write-line "Type=6" file))
					(write-line "Type=1" file)
				)
				;; "TYPE=5" -> MULTIPLE OUTPUT FILES : "TYPE=6" -> SINGLE OUTPUT FILE : "TYPE=1" -> DWF FILE TYPE (SINGLE OUTPUT)
				(write-line (strcat "DWF=" dir fnm) file)
				(write-line (strcat "OUT=" dir) file)
				(write-line "PWD=" file)
				
				(close file)
				(command "_.DELAY" 2000) ;; SYSTEM DELAY BEFORE STARTING PUBLISH COMMAND SO DSD FILE CLOSES
				(command "-PUBLISH" dsd) ;; START PUBLISH COMMAND WITH TEMP.DSD FILE JUST CREATED
				(command "_.DELAY" 2000) ;; SYSTEM DELAY BEFORE DELETING TEMP.DSD SO PUBLISH ROUTINE HAS TIME TO RELEASE IT
				(vl-file-delete dsd)
			)
			(prompt "\nAn error occured when attempting to create / write temporary .dsd file. Publish failed.")
		)
	)
	;; PLOT TYPE NOTES:
	;; 1 = DWF TYPE
	;; 2 = PLOTTING DEVICE?
	;; 4 = DWFX TYPE
	;; 5 = MULTIPLE SHEETS
	;; 6 = SINGLE SHEET
	;; 7 = SVF FILES TYPE (CREATES A BUNCH OF FILES)

	;; [DIR]	- PATH WHERE .DWF / .PDF FILE(S) FROM PLOT WILL BE SAVED
	;; [FNM]	- FILE NAME FOR .DWF / .PDF FILE THAT WILL BE CREATED
	;; [PFX]	- PREFIX FOR EACH LAYOUT SHEET, STANDARD IS (STRCAT (GETVAR "DWGNAME") "-")
	;; [MLT]	- MULTIPLE SHEETS, T FOR YES NIL FOR NO
	;; MLT WILL NOT BE USED IF THE FILE IS SAVED AS A .DWF FILE.
	;; THE 'TYPE' FORMAT IN THE DSD FILE WILL DEFAULT TO 1 IN SUCH CASES.
	;; FUNCTION RETURNS THE RESULT OF DELETING THE DSD FILE (T IF SUCCESSFUL, ELSE NIL)
	
	
	;;;MAIN;;;
	(vl-load-com)
	
	(setq svardef
		(list
			(cons "FILEDIA" 0)
		)
	)
	
	(setq curr (GetSysVariables (mapcar 'car svardef)))
	;; GETS CURRENT SYSTEM VARIABLES
	(SetSysVariables svardef)
	;; SETS SYSTEM VARIABLES TO VALUES IN 'SVARDEF'
	
	(foreach fl fls (WriteDSDPlot dir (car fl) (strcat (vl-filename-base (car fl)) " - ") (cdr fl) mlt))
	
	(SetSysVariables curr)
	;; SETS SYSTEM VARIABLES BACK TO ORIGINAL VALUE
	
	(if opn (OpenFileExplorer dir))
)


(defun GetSubfolderPathInTarget  ( dir trgt sub base )
	(if (or (not base) (not (vl-file-directory-p base)))
		(setq base (getvar "DWGPREFIX"))
	)
	(setq dir (vl-string-right-trim "\\" dir))
	(while
		(and
			(> (strlen dir) (strlen base))
			(/= (vl-filename-base dir) trgt)
		)
		(setq dir (vl-filename-directory dir))
	)
	(if (member sub (vl-directory-files dir nil -1))
		(setq dir (strcat dir "\\" sub "\\"))
		(setq dir (getvar "DWGPREFIX"))
	)
)
;; ------------------------------------------------------------------------
;; GetSubfolderPathInTarget
;; ------------------------------------------------------------------------
;; Returns the full path to a specified subfolder (`sub`) located within a
;; target folder (`trgt`) somewhere in the ancestry of the given directory (`dir`).
;;
;; If the target folder is not found while walking up the directory tree,
;; or if the subfolder does not exist inside it, the function returns the
;; current drawing directory (`DWGPREFIX`) as a fallback.
;;
;; Parameters:
;; - dir  : Starting directory path to begin search
;; - trgt : Name of the target folder to locate
;; - sub  : Name of the subfolder expected inside the target folder
;; - base : Optional base directory to limit search scope (defaults to DWGPREFIX)
;;
;; Example:
;; (GetSubfolderPathInTarget "C:\\Projects\\A\\B\\C" "A" "Output" nil)
;; "C:\\Projects\\A\\Output\\" if "Output" exists in "A".
;; ------------------------------------------------------------------------



(defun PublishAfterDialog ( / )
	(PUBLISHWRAPPER $glb:out_dir* $glb:fls* $glb:mlt* $glb:opn*) ;; RUN PUBLISH FUNCTION
	(mapcar '(lambda ( sym ) (set sym nil))
		(list '$glb:out_dir* '$glb:fls* '$glb:mlt* '$glb:opn*)
	)
	(gc)
)
;; ------------------------------------------------------------------------
;; PublishAfterDialog
;; ------------------------------------------------------------------------
;; Executes the PUBLISHWRAPPER function using global variables set during
;; dialog interaction, then clears those variables to reset the environment.
;;
;; This function is intended to be called after a dialog has been loaded
;; and closed, ensuring that the publish operation does not conflict with
;; simultaneous access to the .DSD file or premature execution of the
;; publish routine.
;; ------------------------------------------------------------------------
