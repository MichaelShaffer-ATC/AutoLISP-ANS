;;; ============================================================
;;; GND-HVSTD.LSP
;;; Inserta un bloque GND_ROD en el centro geométrico (centroide del área)
;;; de cada polilínea similar a la seleccionada (LWPolyline o Polyline).
;;; exportado del WINIGS, Aplica Standard de linea al GNDgrid.
;;; y purga layers inecesarios.
;;; ============================================================


(vl-load-com)

;; crear bloque GND_ROD si no existe
(defun create_gndrod_block ( / doc blks blk cen c1 c2 hatch loop)

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq blks (vla-get-Blocks doc))

  (if (not (tblsearch "BLOCK" "GND_ROD"))
    (progn
      (setq cen (vlax-3d-point '(0 0 0)))
      (setq blk (vla-add blks cen "GND_ROD"))

      (setq c1 (vla-addcircle blk cen 1.0))
      (vla-put-layer c1 "0")
      (vla-put-color c1 0)
      (vla-put-linetype c1 "Continuous")

      (setq c2 (vla-addcircle blk cen 2.0))
      (vla-put-layer c2 "0")
      (vla-put-color c2 0)
      (vla-put-linetype c2 "Continuous")

      (setq hatch (vla-addhatch blk acHatchPatternTypePreDefined "SOLID" :vlax-true))
      (vla-put-layer hatch "0")
      (vla-put-color hatch 0)

      (setq loop (vlax-make-safearray vlax-vbObject '(0 . 0)))
      (vlax-safearray-put-element loop 0 c1)

      (vla-appendouterloop hatch loop)
      (vla-evaluate hatch)
    )
  )
)

(defun ensure_ground_layer ( / doc lays)

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq lays (vla-get-Layers doc))

  ;; cargar dashed si no existe
  (if (not (tblsearch "LTYPE" "DASHED"))
    (command "-linetype" "load" "DASHED" "acad.lin" "")
  )

  ;; crear layer si no existe
  (if (not (tblsearch "LAYER" "GROUNDING_GRID"))
    (progn
      (setq lay (vla-add lays "GROUNDING_GRID"))
      (vla-put-color lay 3)
      (vla-put-linetype lay "DASHED")
    )
    (progn
      (setq lay (vla-item lays "GROUNDING_GRID"))
      (vla-put-color lay 3)
      (vla-put-linetype lay "DASHED")
    )
  )
)

(defun purge_all ()
  (command "-purge" "la" "*" "n")
  (command "-purge" "lt" "*" "n")
)

(defun get_poly_width (ent / obj)
  (setq obj (vlax-ename->vla-object ent))
  (vla-get-ConstantWidth obj)
)

(defun insert_gnd_at_center (ent / obj minpt maxpt cen ins)

  (setq obj (vlax-ename->vla-object ent))

  (vla-getboundingbox obj 'minpt 'maxpt)

  (setq minpt (vlax-safearray->list minpt))
  (setq maxpt (vlax-safearray->list maxpt))

  (setq cen (mapcar '(lambda (a b) (/ (+ a b) 2.0)) minpt maxpt))

  (setq ins
    (vla-insertblock
      (vla-get-modelspace
        (vla-get-ActiveDocument (vlax-get-acad-object)))
      (vlax-3d-point cen)
      "GND_ROD"
      1.0 1.0 1.0 0.0
    )
  )

  (vla-put-layer ins "GROUNDING_GRID")

  (entdel ent)
)

(defun c:ApllyGroundingHV-Standards ( / )
  (c:GND-HVSTD)
)

(defun c:GND-HVSTD ( / refss width i refent ss j ent)

  (create_gndrod_block)
  (ensure_ground_layer)

  (prompt "\nSelect reference polylines with desired width: ")
  (setq refss (ssget '((0 . "LWPOLYLINE"))))

  (if refss
    (progn

      ;; tomar width de la primera seleccion
      (setq refent (ssname refss 0))
      (setq width (get_poly_width refent))

      ;; buscar todas las polilineas con ese width
      (setq ss (ssget "_X" (list '(0 . "LWPOLYLINE") (cons 43 width))))

      (if ss
        (progn
          (setq j 0)

          (while (< j (sslength ss))

            (setq ent (ssname ss j))

            (insert_gnd_at_center ent)

            (setq j (1+ j))
          )
        )
      )

    )
  )

  (purge_all)

  (princ "\nGround rods generated.")
  (princ)
)

(Load:DescriptionLog "GND-HVSTD" "Applies grounding rods to HV standard polylines.")
