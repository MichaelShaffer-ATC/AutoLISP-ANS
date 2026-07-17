(defun c:SETVISRETAINMODE ( / )
  	(if (/= (getvar 'visretain) 1)
	  	(setvar 'visretain 1)
	)
	(if (/= (getvar 'visretainmode) 240)
	  	(setvar 'visretainmode 240)
	)
  	(prompt "VISRETAIN and VISRETAINMODE have been set to: 1 | 240")
  	(princ)
)

(c:SETVISRETAINMODE) ;; GLOBAL


(Load:DescriptionLog "SETVISRETAINMODE" "Sets 'visretain' system variables")
