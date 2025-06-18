;; A DCL GUI FOR COMPILING LISP FILES TO FAS FILES WITH MORE USER FUNCTIONALITY.

;; NOTES:
;; MINIMUM ACAD VERSION == 20.0s (LMS Tech) -> 2015

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                            ;;
;;  Program Name: XFAS Compiler                               ;;
;;  Version: 1.0.0                                            ;;
;;  Date: May 31, 2024                                        ;;
;;  By: Michael Shaffer                                       ;;
;;  With Attributions to:                                     ;;
;;                                                            ;;
;;  Description: Creates a graphic user interface (GUI) for   ;;
;;  the purpose of compiling .lsp file(s) from a selected     ;;
;;  source location to a selected destination location. a     ;;
;;  temporary file is created for the DCL pop-up box and is   ;;
;;  maintained by the function itself. The temporary location ;;
;;  is defaulted to the ' %TEMP% ' folder on the users        ;;
;;  computer with as a file name starting with ' XFAS ' and   ;;
;;  with ' .dcl ' as the file extension. This file is deleted ;;
;;  once the function no longer needs it.                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                            ;;
;;  Changelog:                                                ;;
;;   [1.0.0] - 05/31/2024                                     ;;
;;    -Original Release                                       ;;
;;                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  TERMS AND CONDITIONS OF USE:                              ;;
;;                                                            ;;
;;  This license and disclaimer statement constitutes a legal ;;
;;  agreement between you (either as an individual or a       ;;
;;  single entity) and Michael Shaffer (the "Author"), for    ;;
;;  this software product (the "Software").                   ;;
;;                                                            ;;
;;  By downloading, installing, copying, or otherwise using   ;;
;;  the software, you agree to be bound by all of the         ;;
;;  following terms and conditions of this license and        ;;
;;  disclaimer agreement.                                     ;;
;;                                                            ;;
;;  If you do not agree with all the terms and conditions of  ;;
;;  this agreement, you must immediately cease use of the     ;;
;;  Software and destroy all copies of the Software and all   ;;
;;  of its component or constituent parts in your possession  ;;
;;  or under your control.                                    ;;
;;                                                            ;;
;;  You may redistribute the Software providing you have      ;;
;;  written consent from the Author, and that no              ;;
;;  modifications are made to the original content.           ;;
;;                                                            ;;
;;  You may not charge any fees for the redistribution        ;;
;;  or use of this Software.                                  ;;
;;                                                            ;;
;;  The Software is provided "as is", and with all faults.    ;;
;;  All warranties, expressed or implied, including, but not  ;;
;;  limited to implied warranties of fitness for a particular ;;
;;  use or purpose are hereby disclaimed. There is no         ;;
;;  guarantee that the operation of this Software will be     ;;
;;  uninterrupted or error free.                              ;;
;;                                                            ;;
;;  You acknowledge and agree that your use of the Software   ;;
;;  is at your own risk.                                      ;;
;;                                                            ;;
;;  The Software is a copyrighted work and is protected by    ;;
;;  copyright law and international                           ;;
;;  copyright treaty.                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun c:XFAS
	(
		/
		;Functions
		*error*
		DeleteFile ReturnFolder OpenFileExplorer
		DCL:RunDialogChecks DCL:BrowseFolder DCL:ValidateOptions
		DCL:PopulateListBox DCL:SelectAllToggle DCL:SameDirectoryToggle
		;Variables
		usrs_list ttl dcl tmp des dch src_fldr dst_fldr lst res ext sts
	)
	
	(setq usrs_list
		(list
			"MichaelShaffer"
		)
	)
	
	(defun *error* ( / )
		(if des
			(close des)
		)
		(if (findfile tmp)
			(DeleteFile tmp)
		)
	)
	
	(defun DeleteFile ( file / tmp )
		(if (and (eq (type file) 'STR) (setq tmp (findfile file)))
			(vl-file-delete tmp)
		)
	)
	
	(defun ReturnFolder ( ttl flg / self path )
		(vl-catch-all-apply
			(function
				(lambda ( / shll fldr )
					(if
						(and
							(setq shll (vla-getinterfaceobject (vlax-get-acad-object) "Shell.Application"))
							(setq fldr (vlax-invoke-method shll 'BrowseForFolder (vlax-get (vlax-get-acad-object) 'HWND) ttl flg))
						)
						(setq
							self (vlax-get-property fldr 'Self)
							path (vlax-get-property self 'Path)
						)
					)
				)
			)
		)
		(mapcar
			'(lambda ( obj )
				(if (and (eq (type obj) 'VLA-OBJECT) (not (vlax-object-released-p obj)))
					(vlax-release-object obj)
				)
			)
			(list shll fldr self)
		)
		path
	)
	
	(defun OpenFileExplorer ( dir / shll ret )
		(setq shll (vla-getInterfaceObject (vlax-get-acad-object) "Shell.Application"))
		(setq ret (vl-catch-all-apply 'vlax-invoke (list shll 'Explore dir)))
		(if (not (vl-catch-all-error-p ret))
			(vlax-release-object shll)
		)
		nil
	)
	;; OPENS FILE EXPLORER TO PASSED DIRECTORY, SIMILAR TO USING BUILT-IN 'STARTAPP' FUNCTION
	;; FUNCTION ALWAYS RETURNS NIL, WILL OPEN EXPLORER WINDOW IF SUCCESSFUL
	
	(if (not (member (strcase (getvar "LOGINNAME")) (mapcar '(lambda ( n ) (strcase n)) usrs_list)))
		(prompt "You do not have permission to run this command. Contact CAD admin for permission.")
		(progn
			(setq ttl "lsp2fas")
			(setq dcl
				(list
					"// Temporary DCL file;"
					(strcat ttl " : dialog {")
					"	label = \"LSP To FAS Compiler\";"
					"	: text {"
					"		value = \"VLIDE must be open while using this program.\";"
					"	}"
					"	: boxed_column {"
					"		label = \"LSP Input\";"
					"		: row {"
					"			: edit_box {"
					"				label = \"LSP Source Directory: \";"
					"				alignment = \"left\";"
					"				edit_width = 25;"
					"				key = \"src_dir\";"
					"			}"
					"			: button {"
					"				label = \"Browse\";"
					"				alignment = \"left\";"
					"				fixed_width = true;"
					"				key = \"src_btn\";"
					"			}"
					"		}"
					"		: toggle {"
					"			label = \"Include all LSP's in directory\";"
					"			value = \"0\";"
					"			key = \"all\";"
					"		}"
					"		: list_box {"
					"			alignment = \"centered\";"
					"			multiple_select = true;"
					"			height = 6;"
					"			width = 60;"
					"			key = \"list\";"
					"		}"
					"	}"
					"	spacer_1;"
					"	: boxed_column {"
					"		label = \"FAS Output\";"
					"		: toggle {"
					"			label = \"Compile to same directory\";"
					"			value = \"1\";"
					"			key = \"same\";"
					"		}"
					"		: row {"
					"			: edit_box {"
					"				label = \"FAS Output Directory: \";"
					"				alignment = \"left\";"
					"				edit_width = 25;"
					"				is_enabled = false;"
					"				key = \"dst_dir\";"
					"			}"
					"			: button {"
					"				label = \"Browse\";"
					"				alignment = \"left\";"
					"				fixed_width = true;"
					"				is_enabled = false;"
					"				key = \"dst_btn\";"
					"			}"
					"		}"
					"	}"
					"	spacer_1;"
					"	errtile;"
					"	ok_cancel;"
					"}"
				)
			)
			(if
				(and
					dcl
					(setq tmp (vl-filename-mktemp "XFAS" nil ".dcl"))
					(setq des (open tmp "w"))
					(foreach line dcl (write-line line des))
					(not (close des))
					(> (setq dch (load_dialog tmp)) 0)
					(new_dialog ttl dch)
				)
				(progn
					;; DCL RUNNING CODES ;;
					;; FUNCTIONS CONTAIN VARIABLES IN GLOBAL SCOPE ;;
					(defun DCL:RunDialogChecks ( / )
						(cond
							; CHECK THAT DIRECTORIES ARE VALID
							((= (get_tile "src_dir") "")
								(set_tile "error" "Source directory cannot be blank.")
								nil
							)
							((= (get_tile "dst_dir") "")
								(set_tile "error" "Destination directory cannot be blank.")
								nil
							)
							((not (vl-file-directory-p (get_tile "src_dir")))
								(set_tile "error" "Source directory is not a valid directory.")
								nil
							)
							((not (vl-file-directory-p (get_tile "dst_dir")))
								(set_tile "error" "Destination directory is not a valid directory.")
								nil
							)
							; CHECK THAT 'DST_FLDR' VARIABLE IS NOT BLANK (GLOBAL TO THIS FILE)
							((null dst_fldr)
								(set_tile "error" "Destination folder could not be determined.")
								nil
							)
							; CHECK THAT SELECTED DIRECTORY CONTAINS ATLEAST ONE LSP FILE
							((null lst)
								(set_tile "error" "No LSP files were found in selected directory.")
								nil
							)
							; CHECK THAT ATLEAST ONE LSP IS SELECTED
							((or (null res) (= (strlen res) 0))
								(set_tile "error" "No LSP files were selected.")
								nil
							)
							;; EVERYTHING PASSED
							( t 
								(done_dialog 1)
								(setq res (mapcar '(lambda ( x ) (nth x lst)) (read (strcat "(" res ")"))))
								(foreach ls res
									(if (not (vlisp-compile 'st (strcat src_fldr "\\" ls) (strcat dst_fldr "\\" (vl-filename-base ls) ".fas")))
										(setq ext (cons ls ext))
									)
								)
								(if (not ext)
									(princ "\nLISP source code(s) compiled successfully.")
									(alert
										(strcat
											"The following source code LISP files did not compile:\n\n"
											(apply 'strcat (mapcar '(lambda ( str ) (strcat str "\n")) ext))
										)
									)
								)
								(OpenFileExplorer dst_fldr)
								;; ONLY OPEN FOLDER LOCATION OF DESTINATION IF 'OK' IS HIT
							)
						)
					)
					
					(defun DCL:ValidateOptions ( / )
						(DCL:SameDirectoryToggle)
						(setq lst (DCL:PopulateListBox src_fldr))
						(DCL:SelectAllToggle)
					)
					
					(defun DCL:BrowseFolder ( key / )
						(cond
							((= key "src_btn")
								(if (setq src_fldr (ReturnFolder "LSP Source Directory:" 512)) 
									(progn
										(set_tile "src_dir" src_fldr)
										(DCL:ValidateOptions)
									)
								)
							)
							((= key "dst_btn")
								(if (setq dst_fldr (ReturnFolder "FAS Destination Directory:" 512))
									(set_tile "dst_dir" dst_fldr)
								)
							)
							( t
								(set_tile "error" "Invalid key value was passed. Check source code.")
							)
						)
					)
					;; VARIABLES NEED TO BE MAINTAINED IN GLOBAL SCOPE FOR OVERALL FUNCTION
					
					;; POPULATE LIST BOX
					(defun DCL:PopulateListBox ( dir / )
						(start_list "list")
						(end_list)
						(if (setq lst (vl-directory-files dir "*.lsp" 1))
							(progn
								(if (/= (get_tile "error") "") (set_tile "error" ""))
								(start_list "list")
								(foreach item lst (add_list item))
								(end_list)
							)
							(set_tile "error" "No LSP files were found in selected directory.")
						)
						lst
					)
					
					;; USE SOURCE DIRECTORY AS DESTINATION DIRECTORY (IF TOGGLED ON)
					(defun DCL:SameDirectoryToggle ( / )
						(if (= (get_tile "same") "1")
							(progn
								(set_tile "dst_dir" (get_tile "src_dir"))
								(setq dst_fldr src_fldr)
								(mode_tile "dst_dir" 1)
								(mode_tile "dst_btn" 1)
							)
							(progn
								(if (/= (get_tile "dst_dir") "") (set_tile "dst_dir" ""))
								(mode_tile "dst_dir" 0)
								(mode_tile "dst_btn" 0)
							)
						)
					)
					
					;; SELECT ALL VALUES FROM LIST BOX (IF TOGGLED ON)
					(defun DCL:SelectAllToggle ( / i )
						(if (and lst (= (get_tile "all") "1"))
							(progn
								(repeat (setq i (length lst))
									(setq res (strcat (itoa (setq i (1- i))) (if res (strcat " " res) "")))
								)
								(set_tile "list" res)
							)
							(progn
								(setq res nil)
								(set_tile "list" "")
							)
						)
					)
					
					;; ADD TILE AND DIALOG INFORMATION HERE:
					(action_tile "src_btn" "(DCL:BrowseFolder $key)")
					(action_tile "dst_btn" "(DCL:BrowseFolder $key)")
					(action_tile "src_dir" "(progn (setq src_fldr $value) (DCL:ValidateOptions))")
					(action_tile "dst_dir" "(setq dst_fldr $value)")
					
					(action_tile "list" "(setq res $value)")
					
					(action_tile "same" "(DCL:SameDirectoryToggle)")
					(action_tile "all" "(DCL:SelectAllToggle)")
					
					(action_tile "accept" "(DCL:RunDialogChecks)")
					(action_tile "cancel" "(done_dialog 0)")
					
					(setq sts (start_dialog))
					(setq dch (unload_dialog dch))
				)
				(prompt "\nError loading dialog box.\n")
			)
		)
	)
	(DeleteFile tmp)
	(princ)
)

