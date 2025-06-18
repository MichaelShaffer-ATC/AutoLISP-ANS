;; APPLIES PLOT SETUP TO ALL SHEETS BASED ON THE CURRENT SHEET
;; THIS FUNCTION REQUIRES THE USER TO SET UP A PLOT SETUP FOR ATLEAST ONE SHEET MANUALLY PRIOR TO USING

(defun c:APSL ( / )
	(c:ApplyPlotSetupLayouts)
)

(defun c:ApplyPlotSetupLayouts ( / Adoc layouts layout-obj )

	(setq
		Adoc (vla-get-activedocument (vlax-get-acad-object))
		layouts (vla-get-layouts Adoc)
		layout-obj (vla-get-activelayout Adoc)
	)

	(if (not (eq (getvar "TILEMODE") 0))
		(setvar "TILEMODE" 0)
	)
	; FORCES TO PAPERSPACE TO APPLY PLOT STYLE CHANGES
	
	(foreach layout
		(vl-remove (vla-get-name layout-obj) (layoutlist))
		(vla-copyfrom (vla-item layouts layout) layout-obj)
	)
	; COPY PAGE SETUP INFORMATION TO ALL LAYOUTS
	
	(princ "\nDone applying plot setup to other layouts...")
	
	(princ)
	
)