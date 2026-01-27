;; FUNCTION FOR UPDATING STATE MAP BLOCK TO PASSED STATE
;; IF FUNCTION IS RUN WITHOUT AN ARGUMENT AS A COMMAND, THE USER IS PROMPTED TO SELECT THE STATE
;; IN ORDER TO SAVE DRAWING SPACE, THE BLOCK IS BURSTED AFTER THE STATE BLOCK VISIBILITY IS CHANGED

;; FOR FUNCTIONS DEFINED OUTSIDE OF THIS SCOPE:
;; FOR STD SPECIFIC FUNCTIONS, REFER TO THE "LIB - STD Sub-Functions.lsp" FILE -> (std:)

(defun c:UpdateStateMap ( )
	(UpdateStateMap nil)
)


(defun UpdateStateMap ( stn / psc blk )
	(if (null stn)
		(setq stn (car (std:MultiSelectListBox "Select a state:" (mapcar 'car (glb:StatesList)) nil)))
	)
	(setq psc (vla-get-paperspace (vla-get-activedocument (vlax-get-acad-object))))
	(vlax-for obj psc
		(if (and
				(= (vla-get-objectname obj) "AcDbBlockReference")
				(= (vla-get-effectivename obj) "G-TTLB-USA~")
				(= (vla-get-isdynamicblock obj) :vlax-true)
			)
			(setq blk obj)
		)
	)
	(if blk
		(std:SetBlockVisibility blk (strcase stn))
	)
	
	(princ)
)