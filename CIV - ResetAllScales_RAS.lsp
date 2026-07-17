(defun C:ResetAllScales ( / )
	(C:RAS)
)
(defun C:RAS ( / xpt)
 (command "-scalelistedit"
   "reset"
   "yes"
   "exit")
 (command "CANNOSCALE"   "1\" = 1'")
 (setq xpt (getvar "EXPERT"))
 (setvar "EXPERT" 5)
 (command "-scalelistedit"
  "delete" "*"
  "add" "1\" = 10'" "1:10"
  "add" "1\" = 20'" "1:20"
  "add" "1\" = 30'" "1:30"
  "add" "1\" = 40'" "1:40"
  "add" "1\" = 50'" "1:50"
  "add" "1\" = 60'" "1:60"
  "add" "1\" = 80'" "1:80"
  "add" "1\" = 100'" "1:100"
  "add" "1\" = 200'" "1:200"
  "add" "1\" = 400'" "1:400"
  "add" "1\" = 500'" "1:500"
  "add" "1\" = 600'" "1:600"
  "add" "1\" = 800'" "1:800"
  "add" "1\" = 1000'" "1:1000"
  "Exit")
 (setvar "EXPERT" xpt)
 (princ)
)

(Load:descriptionLog "ResetAllScales / RAS" "Imports Civil Standard Scales")