;; OPENS FILE LOCATION OF CURRENT DRAWING

(defun c:OFL ( / )
	(ECHO:LogWrapper "ANS-OpenFileLocation_OFL.lsp" "OFL" OpenFileLocationMain)
)


(defun c:OPENFILELOCATION ( / )
	(ECHO:LogWrapper "ANS-OpenFileLocation_OFL.lsp" "OPENFILELOCATION" OpenFileLocationMain)
)

(defun OpenFileLocationMain ( / )
	(if (not (OpenFileExplorer (getvar "DWGPREFIX")))
		(princ "\nError: Unable to open the current file's location in file explorer.")
	)
)

(defun OpenFileExplorer ( dir / shl res )
	(setq shl (vla-getinterfaceobject (vlax-get-acad-object) "Shell.Application"))
	(setq res (vl-catch-all-apply 'vlax-invoke-method (list shl 'Explore dir)))
	(vlax-release-object shl)
	(not (vl-catch-all-error-p res))
)


(Load:DescriptionLog "OPENFILELOCATION / OFL" "Opens current drawing's file directory window")