(defun C:P2B (/ ss n blk pl); = Polylines [to] Blocks
  (if (setq ss (ssget "_:L" '((0 . "*POLYLINE"))))
    (repeat (setq n (sslength ss))
      (command
        "_.block" (setq blk (strcat "P2B" (itoa n))); increment Block name
          "_none" (vlax-curve-getStartPoint (setq pl (ssname ss (setq n (1- n)))))
          pl ""
        "_.insert" blk "_none" "@" "" "" ""
      ); command
    ); repeat
  ); if
); defun


(Load:DescriptionLog "P2B" "Polyline to block")