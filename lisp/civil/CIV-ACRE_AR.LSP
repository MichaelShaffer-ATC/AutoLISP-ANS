;; CALCULATES AREA OF A POLYGON

(defun c:AR ( / )
	(ECHO:LogWrapper "CIV-ACRE_AR.lsp" "AR" AcreMain)
)

(defun c:ACRE ( / )
	(ECHO:LogWrapper "CIV-ACRE_AR.lsp" "ACRE" AcreMain)
)


(defun AcreMain ( / OS ENT Area a )

	(setq OS (getvar "osmode"))
	(setvar "osmode" 512)
	(setq ENT (entsel "\nSelect Polygon "))
	(setvar "osmode" OS)

	(command ".area" "e" ENT)
	(setq Area (getvar "area"))
	(setq Acre 43560)
	(setq a (/ area acre))
	(princ "    ")
	(princ a)
	(princ " Acres         ")
	(princ area)
	(princ "  S.F.")

	(princ)

)


(Load:DescriptionLog "ACRE / AR" "Calculates area of a selected polygon")