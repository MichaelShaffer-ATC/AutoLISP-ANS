(defun c:IsoIntersectingPolysCircles ( / ssCircles ssPolys allEnts i j circleObj polyObj intpts polyHits circleHits ent allHits toShow toHide)
  (vl-load-com)

  ;; Get all circles
  (setq ssCircles (ssget "X" '((0 . "CIRCLE"))))
  (if (not ssCircles)
    (progn (princ "\nNo circles found.") (exit)))

  ;; Get all 2D polylines
  (setq ssPolys (ssget "X" '((0 . "LWPOLYLINE"))))
  (if (not ssPolys)
    (progn (princ "\nNo polylines found.") (exit)))

  (setq i 0
        polyHits '()
        circleHits '())

  ;; Loop through polylines
  (while (< i (sslength ssPolys))
    (setq polyObj (vlax-ename->vla-object (ssname ssPolys i)))
    (setq j 0)

    ;; Check each poly against all circles
    (while (< j (sslength ssCircles))
      (setq circleObj (vlax-ename->vla-object (ssname ssCircles j)))
      (setq intpts (vlax-invoke polyObj 'IntersectWith circleObj acExtendNone))

      (if (> (length intpts) 0)
        (progn
          (if (not (member (ssname ssPolys i) polyHits))
            (progn
              (redraw (ssname ssPolys i) 3)
              (setq polyHits (cons (ssname ssPolys i) polyHits))
            )
          )
          (if (not (member (ssname ssCircles j) circleHits))
            (setq circleHits (cons (ssname ssCircles j) circleHits))
          )
        )
      )
      (setq j (1+ j))
    )
    (setq i (1+ i))
  )

  ;; Combine intersecting entities
  (setq allHits (append polyHits circleHits))

  ;; Build list of all entities in drawing
  (setq allEnts (ssget "X"))
  (setq i 0 toHide '() toShow '())

  (while (< i (sslength allEnts))
    (setq ent (ssname allEnts i))
    (if (member ent allHits)
      (setq toShow (cons (vlax-ename->vla-object ent) toShow))
      (setq toHide (cons (vlax-ename->vla-object ent) toHide))
    )
    (setq i (1+ i))
  )

  ;; Hide all non-matching entities
  (foreach e toHide (vlax-put-property e 'Visible :vlax-false))

  ;; Show matching entities (in case they were hidden before)
  (foreach e toShow (vlax-put-property e 'Visible :vlax-true))

  (princ (strcat "\nDone. Isolated " (itoa (length toShow)) " intersecting polylines and circles."))
  (princ)
)


(Load:DescriptionLog "ISOINTERSECTINGPOLYSCIRCLES" "Isolates circles at selected polyline intersection(s)")