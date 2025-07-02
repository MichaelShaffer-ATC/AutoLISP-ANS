;; C:\_ACC\ACCDocs\ANS Geo\_ANSCAD\Project Files\Tool Palettes

(defun c:RLTP ( / )
	(c:RELOADTOOLPALETTES)
)

(defun c:RELOADTOOLPALETTES
	(
		/
		;Functions
		CopyFolder AddToolPalettePath
		;Variables
		dir cat-loc xpg-loc doc-loc
	)
	
	(defun CopyFolder ( src des ovr / fso rtn )
		(if (setq fso (vlax-create-object "scripting.filesystemobject"))
			(progn
				(setq rtn
					(not
						(or (zerop (vlax-invoke fso 'folderexists src))
							(vl-catch-all-error-p
								(vl-catch-all-apply 'vlax-invoke
									(list fso 'copyfolder src des (if ovr :vlax-true :vlax-false))
								)
							)
						)
					)
				)
				(vlax-release-object fso)
				rtn
			)
		)
	)
	;; COPIES FOLDER CONTENTS TO DESTINATION LOCATION AND OVER-WRITES FILES / DIRECTORIES IF SPECIFIED
	
	(defun AddToolPalettePath ( pth )
		(if (not (wcmatch (getvar '*_toolpalettepath) (strcat "*" pth "*")))
			(cond
				((= (strlen (getvar '*_toolpalettepath)) 0)
					(setvar '*_toolpalettepath pth)
				)
				( t
					(setvar '*_toolpalettepath (strcat (getvar '*_toolpalettepath) ";" pth))
				)
			)
		)
	)
	;; SETS TOOLPALETTE PATH SYSTEM VARIABLE
	
	(setq dir (strcat (getenv "USERPROFILE") "\\ANS Geo\\ANS Geo Central - Documents\\Structural\\CAD\\Details\\Tool Palettes\\"))
	(setq doc-loc (vl-registry-read "HKEY_CURRENT_USER\\Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\Shell Folders" "Personal"))
	
	(cond
		((not (vl-file-directory-p dir))
			(alert (strcat "Provided directory '" dir "' was not found."))
		)
		((not (eq (type doc-loc) 'STR))
			(alert "Incompatible data type returned for directory.")
		)
		((not (vl-file-directory-p doc-loc))
			(alert (strcat "Provided directory '" doc-loc "' was not found."))
		)
		( t
			(command-s "._TOOLPALETTESCLOSE")
			(setq cat-loc (strcat dir "Master-Catalog"))
			(setq xpg-loc (strcat dir "Master-Groups"))

			(CopyFolder cat-loc "C:\\ANS_ToolPalettes" 1) ;; CREATES THE FOLDER LOCATION AUTOMATICALLY AND OVER-WRITES ANY EXISTING FILES
			
			(CopyFolder xpg-loc doc-loc 1) ; COPIES TO DOCUMENTS LOCATION
			(princ "\nPalettes are being copied to Documents location.")
			(AddToolPalettePath "C:\\ANS_ToolPalettes\\Palettes")
			
			(command-s "._TOOLPALETTES")
			(command-s "._CUSTOMIZE")
		)
	)
	
	(princ)
	
)

;;;BOOKMARK;;;
;; NEED FULL PATH TO TOOL PALETTES
;; WOULD ALSO BE BENEFICIAL TO HAVE PALETTES ORGANIZED INTO GROUPS NOT JUST FOR CIVIL BUT FOR STRUCTURAL AS WELL
;; C:\Users\MichaelShaffer\AppData\Roaming\Autodesk\AutoCAD 2026\R25.1\enu\Support\ToolPalette\Palettes
