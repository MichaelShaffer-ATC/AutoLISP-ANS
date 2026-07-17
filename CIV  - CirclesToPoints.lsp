(defun c:CircleCentersToPoints ( / ssCircles i circleObj center )
  (vl-load-com)

  ;; Get all circles in the drawing
  (setq ssCircles (ssget "X" '((0 . "CIRCLE"))))
  (if (not ssCircles)
    (progn (princ "\nNo circles found in drawing.") (exit)))

  (setq i 0)

  ;; Loop through each circle and place a POINT at its center
  (while (< i (sslength ssCircles))
    (setq circleObj (vlax-ename->vla-object (ssname ssCircles i)))
    (setq center (vlax-get circleObj 'Center))

    ;; Create standard AutoCAD POINT
    (entmakex (list '(0 . "POINT") (cons 10 center)))

    (setq i (1+ i))
  )

  (princ (strcat "\n✅ Done. Created " (itoa i) " AutoCAD points at circle centers."))
  (princ)
)


(Load:DescriptionLog "CIRCLECENTERSTOPOINTS" "Creates a point at all circle centers")