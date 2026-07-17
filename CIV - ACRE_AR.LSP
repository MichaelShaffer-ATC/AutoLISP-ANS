;; CALCULATES AREA OF A POLYGON

(defun c:ar ( )
	(c:acre)
)

(defun c:acre ( / OS ENT Area a )
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