;; LIBRARY FILE FOR WINDOWS SCRIPT HOSTING

(defun wsh:CopyFile ( src dst / fso )
	(if (and (findfile src) (vl-file-directory-p dst))
		(if (setq fso (vlax-create-object "Scripting.FileSystemObject"))
			(progn
				(vlax-invoke fso 'CopyFile src dst :vlax-true)
				(vlax-release-object fso)
				t
			)
		)
	)
)
;; COPIES FILE FROM SOURCE LOCATION TO DESTINATION LOCATION
;; [ src ]	== SOURCE FILE PATH TO COPY FILE FROM
;; [ dst ]	== DESTINATION LOCATION THE FILE WILL BE COPIED TO


(defun wsh:ReturnFolder ( ttl bit rt / shl fdr pth )
	(vl-catch-all-apply
		'(lambda ( )
			(if (and
					(setq shl (vla-getinterfaceobject (vlax-get-acad-object) "Shell.Application"))
					(setq fdr (vlax-invoke-method shl 'BrowseForFolder (vlax-get (vlax-get-acad-object) 'HWND) ttl bit rt))
				)
				(setq pth (vlax-get-property (vlax-get-property fdr 'Self) 'Path))
			)
		)
	)
	(foreach obj (list shl fdr)
		(if (and (eq (type obj) 'VLA-OBJECT) (not (vlax-object-released-p obj)))
			(vlax-release-object obj)
		)
	)
	pth
)
;; RETURNS SELECTED FOLDER FROM BROWSE MENU DIALOG BOX USING WINDOWS SCRIPT HOSTING
;; [ ttl ]	== STRING FOR POP-UP BOX TITLE NAME
;; [ bit ]	== SYSTEM FLAG FOR SPECIFIC BEHAVIOR (SEE DIALOG FLAGS LISTED FOR MORE INFORMATION)
;; [ rt  ]	== ROOT FILE PATH
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


(defun wsh:ConfirmationDialog ( msg ttl tim bit / shl res )
	(vl-catch-all-apply
		(function
			(lambda ( / shl )
				(if (setq shl (vla-getinterfaceobject (vlax-get-acad-object) "WScript.Shell"))
					(setq res (vlax-invoke-method shl 'Popup msg tim ttl bit))
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
		(list shl)
	)
	res
)
;; RETURNS INTEGER VALUE OF SELECTION IN DIALOG BOX USING WINDOWS SCRIPT HOSTING
;; OUTPUT: 6 == "YES", 7 == "NO"
;;
;; [ msg ]	== MESSAGE TO DISPLAY WITHIN THE POP-UP BOX
;; [ ttl ]	== STRING FOR POP-UP BOX TITLE NAME
;; [ tim ]	== INTEGER TIME VALUE FOR TIME-OUT IN SECONDS BEFORE AUTO-CLOSE
;; [ flg ]	== SYSTEM FLAG FOR SPECIFIC BEHAVIOR (SEE DIALOG FLAGS LISTED FOR MORE INFORMATION)
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
;; VISIT FOR MSDN BITCODE INFORMATIONA


