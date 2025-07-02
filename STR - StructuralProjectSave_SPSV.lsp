;; FUNCTION FOR SAVING A NEW PROJECT TO THE ANS PROJECTS DIRECTORY

(defun c:SPSV ( / )
	(c:STRUCTURALPROJECTSAVE)
)

(defun c:STRUCTURALPROJECTSAVE
	( 
		/ 
		;Functions
		*error*
		RemoveRootFolder MakeDirectories TitleCase
		;DCL Functions
		DCL:RunDialogChecks DCL:Cancel
		DCL:PopulateProjectList DCL:FilterClientList
		;DCL (Global Variables)
		res
		;Variables
		dir clnts pjcts clnt pjct fldr
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
		(if (not (member msg (list "Function cancelled" "quit / exit abort")))
			(vl-bt)
		) ;; FOR DEBUGGING
		(strcat "\nError: " msg)
	)
	
	(defun RemoveRootFolder ( dirs )
		(vl-remove-if
			'(lambda ( d ) (member d (list "." "..")))
			dirs
		)
	)
	;; REMOVES THE HIDDEN ROOT FOLDERS WITHIN A LIST OF DIRECTORIES, RETURNS NIL IF DIRECTORY DOES NOT EXIST
	
	(defun MakeDirectories ( root sbfs nst / main ret )
		(mapcar
			'(lambda ( sub )
				(if (not (vl-file-directory-p (setq main (strcat root "\\" sub))))
					(setq ret (vl-mkdir main))
				)
				(if nst (setq root main))
				ret
			)
			sbfs
		)
	)
	;; CREATES DIRECTORIES BASED ON LIST OF PASSED SUBFOLDERS
	;; IF NESTING (NST) IS TRUE, THE CREATED DIRECTORIES ARE NESTED, ELSE THE DIRECTORIES ARE CREATED FLATTENED
	
	(defun TitleCase ( str )
		(vl-list->string
			(mapcar
				'(lambda ( x y z )
					(if (= x 32) y z)
				)
				(cons 32 (vl-string->list str)) ; INITIALIZES THE BEGINING CHARACTER AS A SPACE
				(vl-string->list (strcase str))
				(vl-string->list (strcase str t))
			)
		)
	)
	;; RETURNS A STRING IN TITLE CASE FORMAT I.E. "This Is A Title"
	
	(vl-load-com)
	
	(setq dir
		(strcat
			(getenv "USERPROFILE")
			"\\ANS Geo"
			"\\ANS Geo Projects - Documents"
			"\\3 - PROJECTS"
		)
	)
	
	(setq ttl "Folder_Creator")
	(setq dcl
		(list
			"// Temporary DCL file;"
			(strcat ttl " : dialog {")
			"	label = \"Project Folder Creator Dialog Control\";"
			"	: text {"
			"		value = \"**Project names will populate when a client is selected.**\";"
			"	}"
			"	: row {"
			"		: boxed_column {"
			"			label = \"Client info\";"
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
			"		}"
			"		: column {"
			"			: column {"
			"				: list_box {"
			"					label = \"Client project names: \";"
			"					multiple_select = false;"
			"					width = 32;"
			"					key = \"pjct\";"
			"				}"
			"				: edit_box {"
			"					label = \"Sub-folder name: \";"
			"					edit_width = 25;"
			"					key = \"subf\";"
			"				}"
			"			}"
			"			spacer_1;"
			"			errtile;"
			"			ok_cancel;"
			"		}"
			"	}"
			"}"
		)
	)
	
	(if
		(and
			dcl
			(setq tmp (vl-filename-mktemp "FLDR" nil ".dcl"))
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
					((null clnt)
						(set_tile "error" "Error: A client name must be selected.")
					)
					((null pjct)
						(set_tile "error" "Error: A project name must be selected.")
					)
					((= (get_tile "subf") "")
						(set_tile "error" "Error: A subfolder name is required. Ex: \"10% Drawings\"")
					)
					(	(or
							(vl-file-directory-p (strcat dir "\\" (nth (read clnt) clnts) "\\" (nth (read pjct) pjcts) "\\" "3 - Maps Plans & Drawings" "\\" "CAD" "\\" "Structural" "\\" fldr))
							(vl-file-directory-p (strcat dir "\\" (nth (read clnt) clnts) "\\" (nth (read pjct) pjcts) "\\" "3 - Maps Plans & Drawings" "\\" "CAD" "\\" "Structural" "\\" (strcase fldr)))
							(vl-file-directory-p (strcat dir "\\" (nth (read clnt) clnts) "\\" (nth (read pjct) pjcts) "\\" "3 - Maps Plans & Drawings" "\\" "CAD" "\\" "Structural" "\\" (strcase fldr t)))
							(vl-file-directory-p (strcat dir "\\" (nth (read clnt) clnts) "\\" (nth (read pjct) pjcts) "\\" "3 - Maps Plans & Drawings" "\\" "CAD" "\\" "Structural" "\\" (TitleCase fldr)))
						)
						(alert
							(strcat
								"A directory for this project seems to already exist.\n\nThis command is specifically used for creating new drawing files.\n"
								"If you need to save over the existing drawing file, please open that file directly and save it using the built-in SAVE feature.\n"
								"Otherwise, please enter a new subfolder name within the dialog box to create a new project folder and file."
							)
						)
						(set_tile "error" "Error: Project directory location already exists.")
					)
					( t
						(done_dialog 1)
						(setq clnt (nth (read clnt) clnts)) ;; RETURN SELECTED CLIENT AND PROJECT NAME
						(setq pjct (nth (read pjct) pjcts))
						
						;(alert (strcat dir "\\" clnt "\\" pjct "\\" "3 - Maps Plans & Drawings" "\\" "CAD" "\\" "Structural" "\\" (TitleCase fldr)))
						
						(MakeDirectories
							(strcat dir "\\" clnt "\\" pjct "\\")
							(list
								"3 - Maps Plans & Drawings"
								"CAD"
							)
							t
						)
						;; BASED ON MOST PROJECTS I'VE SEEN, CAD FOLDER COMES FIRST - WILL NEED TO RE-WRITE CODE TO MATCH
						
						(MakeDirectories
							(strcat dir "\\" clnt "\\" pjct "\\" "3 - Maps Plans & Drawings" "\\" "CAD" "\\")
							(list
								"Structural"
								"Individual PDF's"
								"Markups"
								"Old Versions"
							)
							nil
						)

						(MakeDirectories
							(strcat dir "\\" clnt "\\" pjct "\\" "3 - Maps Plans & Drawings" "\\" "CAD" "\\" "Structural")
							(list
								"VOID"
								(TitleCase fldr)
							)
							nil
						)
						
						(vla-saveas
							(vla-get-activedocument (vlax-get-acad-object))
							(strcat
								dir
								"\\"
								clnt
								"\\"
								pjct
								"\\3 - Maps Plans & Drawings"
								"\\CAD"
								"\\Structural"
								"\\"
								(TitleCase fldr)
								"\\"
								(strcat pjct " - " fldr ".dwg")
							)
						) ;; SAVES DRAWING FILE
						
						(if (findfile tmp)
							(vl-file-delete tmp)
						)
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
			
			(defun DCL:PopulateProjectList ( / )
				(start_list "pjct")
				(end_list)
				(cond
					((null (vl-file-directory-p (strcat dir "\\" (nth (read clnt) clnts))))
						(set_tile "error" "Error: Directory to client name was not found.")
					)
					((null (setq pjcts (RemoveRootFolder (vl-directory-files (strcat dir "\\" (nth (read clnt) clnts)) nil -1))))
						(set_tile "error" "There are no projects associated with this client.") ;;;BOOKMARK;;; ;; GOING TO UPDATE THIS TO ALLOW USER TO CREATE FOLDER ???
					)
					( t
						(if (/= (get_tile "error") "") (set_tile "error" ""))
						(start_list "pjct")
						(foreach prj pjcts (add_list prj))
						(end_list)
					)
				)
			)
			;; UPDATES THE PROJECT LIST BASED ON SELECTED CLIENT
			
			(defun DCL:FilterClientList ( ptr )
			  	(setq clnts
				       (vl-remove-if-not
						'(lambda ( s )
							(eq (substr (strcase s) 1 (strlen ptr)) (strcase ptr))
						)
						(RemoveRootFolder (vl-directory-files dir nil -1))
					)
				)
				(start_list "clnt")
				(foreach clt clnts (add_list clt))
				(end_list)
			)
			;; FILTERS CLIENT LIST BASED ON MATCHING STRING VALUE FROM FILTER EDIT BOX
			
			;;;MAIN;;;
			(setq clnts (RemoveRootFolder (vl-directory-files dir nil -1))) ;; GETS THE CLIENTS FOLDER IF DIRECTORY IS FOUND, ELSE NIL
			;; INITIALIZE CLIENT LIST
			
			(start_list "clnt")
			(foreach clt clnts (add_list clt))
			(end_list)
			
			(action_tile "accept" "(DCL:RunDialogChecks)")
			(action_tile "cancel" "(DCL:Cancel)")
			
			(action_tile "clnt" "(setq clnt $value) (DCL:PopulateProjectList)")
			(action_tile "pjct" "(setq pjct $value)")
			(action_tile "fltr" "(DCL:FilterClientList $value)")
			(action_tile "subf" "(setq fldr $value)")
			
			(start_dialog)
			(unload_dialog dch)
		)
		(prompt "\nError loading dialog box.")
	)
	
	(princ)
)



;|
	;; FOLDER LIST ;;
	"CAD"
		"VOID"
		; FOLDER CREATED HERE WILL DEPEND ON WHAT THE VALUE IS FOR "subf". IF BLANK, THE CODE SHOULD ERROR AND TELL THE USER TO CREATE A SUBFOLDER NAME
		; THIS IS THE DEFAULT LOCATION FOR DRAWING FILES, A POP-UP WARNING WILL BE REQUIRED HERE NOTIFYING THE USER THAT IF THEY CHOOSE TO CONTINUE
		; ANY DRAWING FILES SAVED IN THIS LOCATION WITH THE SAME NAME WILL BE OVER-WRITTEN
		; THIS FUNCTION IS ONLY MEANT TO BE USED WHEN FIRST CREATING / SAVING A PROJECT FILE BUT CAN BE USED AS A "SAVEAS" SHORTCUT TO A NEWLY CREATED FOLDER
	"Individual PDF's"
		; EMPTY FOLDER INITIALLY
	"Markups"
		; EMPTY FOLDER INITIALLY
	"Old Versions"
		; EMPTY FOLDER INITIALLY
|;
;; COMMENTED OUT - FOR REFERENCE ONLY