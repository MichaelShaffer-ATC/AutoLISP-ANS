(defun c:tw (/ oldos llc lrc urc rva va)
  (setvar "cmdecho" 0)
  (setq oldos
    (getvar "osmode")
  )
  (setvar "osmode" 1)
  (prompt "\nBranson's View Twist Macro.")
  (command "mspace")
  (setq llc
    (getpoint "\nLower Left Corner of View: ")
  )
  (setq lrc
    (getpoint llc "\nLower Right Corner of View: ")
  )
  (setq urc 
    (getpoint "\nUpper Right Corner of View: ")
  )
  (setvar "osmode" 0)
  (setq rva 
    (/ 
      (* 
        (angle llc lrc)
      180)
    pi)
  )
  (setq va
    (- 360 rva)
  )
  (setq ref 
    (ssget)
  )
  (command "dview" ref "" "tw" va ""
           "view" "u" "n"
           "view" "w" "Plan" llc urc
           "view" "r" "plan"
           "pspace")
  (setvar "osmode" oldos)
  (prompt "\nThank you for your continued support.")
  (princ)
)


(Load:DescriptionLog "TW" "Rotates modelspace viewport view based on selected points")