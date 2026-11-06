;; APPLIES PLOT SETUP TO ALL SHEETS BASED ON THE CURRENT SHEET
;; THIS FUNCTION REQUIRES THE USER TO SET UP A PLOT SETUP FOR ATLEAST ONE SHEET MANUALLY PRIOR TO USING

(defun c:APSL ( / )
	(ECHO:LogWrapper "ANS-ApplyPlotSetupLayouts_APSL.lsp" "APSL" ApplyPlotSetupLayoutsMain)
)

(defun c:APPLYPLOTSETUPLAYOUTS ( / )
	(ECHO:LogWrapper "ANS-ApplyPlotSetupLayouts_APSL.lsp" "APPLYPLOTSETUPLAYOUTS" ApplyPlotSetupLayoutsMain)
)

(defun ApplyPlotSetupLayoutsMain ( / doc lyts lyt )
	
	(setq
		doc (vla-get-activedocument (vlax-get-acad-object))
		lyts (vla-get-layouts doc)
		lyt (vla-get-activelayout doc)
	)

	(if (not (eq (getvar "TILEMODE") 0))
		(setvar "TILEMODE" 0)
	)
	; FORCES TO PAPERSPACE TO APPLY PLOT STYLE CHANGES
	
	(foreach ly
		(vl-remove (vla-get-name lyt) (layoutlist))
		(vla-copyfrom (vla-item lyts ly) lyt)
	)
	; COPY PAGE SETUP INFORMATION TO ALL LAYOUTS
	
	(princ "\nDone applying plot setup to other layouts...")
	
	(princ)
	
)


(Load:DescriptionLog "APPLYPLOTSETUPLAYOUTS / APSL" "Applies current layout plot settings to all layouts")