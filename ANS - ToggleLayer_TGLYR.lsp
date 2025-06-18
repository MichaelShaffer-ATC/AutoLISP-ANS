;; TURNS USER SELECTED LAYER ON OR OFF DEPENDING ON THE CURRENT STATE OF THE LAYER

(defun c:TGLYR ( / )
  	(c:TOGGLELAYER)
)

(defun c:TOGGLELAYER ( / RemoveWhiteSpaceEnds ToggleLayer lyrname )

  	(defun RemoveWhiteSpaceEnds ( str / wht )
		(setq wht (list " " "\t" "\n"))
		(while (and (not (zerop (strlen str))) (member (substr str (strlen str) (strlen str)) wht))
			(setq str (substr str 1 (1- (strlen str))))
		)
		(while (and (not (zerop (strlen str))) (member (substr str 1 1) wht))
			(setq str (substr str 2 (strlen str)))
		)
		str
	)
  	
	(defun ToggleLayer ( lyr )
		(setq lyrs (vla-get-layers (vla-get-activedocument (vlax-get-acad-object))))
	  	(if (vl-catch-all-error-p (setq lyr (vl-catch-all-apply 'vla-item (list lyrs lyr))))
			(prompt "Error: Layer not found in database.")
		  	(vla-put-layeron lyr (if (eq (vla-get-layeron lyr) :vlax-true) :vlax-false :vlax-true))
		)
	)
	
	(vl-load-com)

  	(while (= (strlen (setq lyrname (RemoveWhiteSpaceEnds (getstring "Enter layer name: " 1)))) 0)
	  	(princ "\nLayer name cannot be blank.\n")
	)

  	(ToggleLayer lyrname)

  	(princ)
)

;; MAY WNAT TO UPDATE THIS TO ALLOW FOR SELECTION OF LAYER THROUGH A POP-UP BOX RATHER THAN TYPING OUT THE LAYER NAME EACH TIME