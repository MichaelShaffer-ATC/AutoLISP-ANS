;; GLOBAL SYSTEM VARIABLE SETTING FILE
;; GETS CALLED AUTOMATICALLY FROM THE AUTO-LOADER
;; THIS FUNCTION IS SET GLOBALLY FOR ACCESS TO MULTIPLE ACADDOC FILE CALLS

(defun Load:SetSystemVariables ( / Load:DisplaySystemLogChange )
	
	(defun Load:DisplaySystemLogChange ( kvp / sys new old )
		(setq
			sys (car kvp)
			new (cdr kvp)
			old (getvar sys)
		)
		(princ (strcat "\n" sys " was automatically updated: " (itoa old) " --> " (itoa new) "! ")) ;; SPACE LEFT FOR COMMAND LINE RESPONSES FOR SOME FIELD CHANGES ON REGEN
	)
	
	(mapcar
		'(lambda ( s / sys val ) (if (/= (getvar (setq sys (car s))) (setq val (cdr s))) (progn (Load:DisplaySystemLogChange s) (setvar sys val))))
		(list
			(cons "FILEDIA" 1)		;; FILE DIALOG
			(cons "PDFSHX" 0)		;; PDF META-DATA
			(cons "WHIPTHREAD" 3)	;; TRIGGER MULTI-THREADING FOR REGEN AND REDRAW
			(cons "OLEFRAME" 2)		;; SHOW, DON'T PLOT
			(cons "IMAGEFRAME" 2)	;; SHOW, DON'T PLOT
			(cons "WIPEOUTFRAME" 2)	;; SHOW, DON'T PLOT
			(cons "REGENMODE" 1)	;; TRIGGERS AUTO REGENERATION
			(cons "DIMASSOC" 2)		;; AUTOMATICALLY ASSOCIATES DIMENSIONS TO OBJECTS
			(cons "FIELDEVAL" 31)	;; FIELDS UPDATE ON OPEN (1), SAVE (2), PLOT (4), ETRANSMIT (8), AND REGEN / REDRAW (16)
		)
	)
	(princ)
)