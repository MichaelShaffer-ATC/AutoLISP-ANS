;; OPENS FILE LOCATION OF CURRENT DRAWING

(defun c:OFL ( / )
	(c:OPENFILELOCATION)
)

(defun c:OPENFILELOCATION ( / )
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
