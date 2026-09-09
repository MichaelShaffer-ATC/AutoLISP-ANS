;;; ============================================================
;;; GND-WELD.LSP
;;; Inserta GND-WELD en TODAS las intersecciones entre líneas
;;; seleccionadas incluyendo endpoints. *No detecta polilineas.*
;;; Solo omite puntos donde ya exista GND_ROD o GND_WELD.
;;; ============================================================

(vl-load-com)

;; ------------------------------------------------
;; crear bloque GND_WELD si no existe
;; ------------------------------------------------
(defun create_gndweld_block (/ doc blks blk pts pl)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq blks (vla-get-Blocks doc))
  (if (not (tblsearch "BLOCK" "GND_WELD"))
    (progn
      (setq blk (vla-add blks (vlax-3d-point '(0 0 0)) "GND_WELD"))
      (setq pts (vlax-make-safearray vlax-vbDouble '(0 . 7)))
      (vlax-safearray-fill pts (list -0.25 -0.25 0.25 -0.25 0.25 0.25 -0.25 0.25))
      (setq pl (vla-addlightweightpolyline blk pts))
      (vla-put-closed pl :vlax-true)
      (vla-put-linetype pl "Continuous")
      (vla-put-constantwidth pl 0.5)
    )
  )
)

;; ------------------------------------------------
;; obtener segmentos de un objeto como lista de
;; pares ((x1 y1)(x2 y2))
;; Para LINE: un solo segmento
;; Para LWPOLYLINE: N segmentos entre vértices
;; ------------------------------------------------
(defun get_segments (obj / sp ep coords segs n i x1 y1 x2 y2)
  (cond

    ((= (vla-get-objectname obj) "AcDbLine")
     (setq sp (vlax-get obj 'StartPoint))
     (setq ep (vlax-get obj 'EndPoint))
     (list (list (list (car sp) (cadr sp))
                 (list (car ep) (cadr ep))))
    )

    ((= (vla-get-objectname obj) "AcDbPolyline")
     (setq coords (vlax-get obj 'Coordinates))
     (setq n (/ (length coords) 2))
     (setq segs '())
     (setq i 0)
     (while (< i (1- n))
       (setq x1 (nth (* i 2) coords))
       (setq y1 (nth (+ (* i 2) 1) coords))
       (setq x2 (nth (* (1+ i) 2) coords))
       (setq y2 (nth (+ (* (1+ i) 2) 1) coords))
       (setq segs (append segs (list (list (list x1 y1) (list x2 y2)))))
       (setq i (1+ i))
     )
     (if (= (vlax-get obj 'Closed) :vlax-true)
       (progn
         (setq x1 (nth (* (1- n) 2) coords))
         (setq y1 (nth (+ (* (1- n) 2) 1) coords))
         (setq x2 (car coords))
         (setq y2 (cadr coords))
         (setq segs (append segs (list (list (list x1 y1) (list x2 y2)))))
       )
     )
     segs
    )

  )
)

;; ------------------------------------------------
;; interseccion matematica entre dos segmentos 2D
;; AHORA incluye endpoints (t y u pueden ser 0 o 1)
;; retorna (x y) si se cruzan, nil si no
;; ------------------------------------------------
(defun seg_intersect (seg1 seg2 / x1 y1 x2 y2 x3 y3 x4 y4
                               dx1 dy1 dx2 dy2 denom t_val u_val
                               ix iy tol)

  (setq tol 1e-9)

  (setq x1 (car  (car  seg1)))  (setq y1 (cadr (car  seg1)))
  (setq x2 (car  (cadr seg1)))  (setq y2 (cadr (cadr seg1)))
  (setq x3 (car  (car  seg2)))  (setq y3 (cadr (car  seg2)))
  (setq x4 (car  (cadr seg2)))  (setq y4 (cadr (cadr seg2)))

  (setq dx1 (- x2 x1))  (setq dy1 (- y2 y1))
  (setq dx2 (- x4 x3))  (setq dy2 (- y4 y3))

  (setq denom (- (* dx1 dy2) (* dy1 dx2)))

  (if (> (abs denom) tol)
    (progn
      (setq t_val (/ (+ (* (- x3 x1) dy2) (* (- y1 y3) dx2)) denom))
      (setq u_val (/ (+ (* (- x3 x1) dy1) (* (- y1 y3) dx1)) denom))

      ;; CAMBIO: rango [0, 1] inclusivo — endpoints ahora se detectan
      (if (and (>= t_val (- 0.0 1e-6)) (<= t_val (+ 1.0 1e-6))
               (>= u_val (- 0.0 1e-6)) (<= u_val (+ 1.0 1e-6)))
        (progn
          (setq ix (+ x1 (* t_val dx1)))
          (setq iy (+ y1 (* t_val dy1)))
          (list ix iy)
        )
      )
    )
  )
)

;; ------------------------------------------------
;; obtener todos los puntos de interseccion entre
;; dos objetos usando calculo matematico
;; ------------------------------------------------
(defun get_intersections_math (obj1 obj2 / segs1 segs2 pts s1 s2 pt)
  (setq segs1 (get_segments obj1))
  (setq segs2 (get_segments obj2))
  (setq pts '())
  (foreach s1 segs1
    (foreach s2 segs2
      (setq pt (seg_intersect s1 s2))
      (if pt
        (setq pts (append pts (list pt)))
      )
    )
  )
  pts
)

;; ------------------------------------------------
;; deduplicar puntos muy cercanos entre si
;; evita insertar doble en el mismo lugar
;; ------------------------------------------------
(defun dedup_points (ptlist tol / result pt duplicate found)
  (setq result '())
  (foreach pt ptlist
    (setq found nil)
    (foreach existing result
      (if (< (distance pt existing) tol)
        (setq found t)
      )
    )
    (if (not found)
      (setq result (append result (list pt)))
    )
  )
  result
)

;; ------------------------------------------------
;; verificar si ya existe bloque en el punto
;; UNICA condicion de exclusion
;; ------------------------------------------------
(defun block_exists_at_point (pt tol / ss)
  (setq ss
    (ssget "_C"
      (list (- (car pt) tol) (- (cadr pt) tol) 0.0)
      (list (+ (car pt) tol) (+ (cadr pt) tol) 0.0)
      '((0 . "INSERT")(2 . "GND_ROD,GND_WELD"))
    )
  )
  ss
)

;; ------------------------------------------------
;; insertar weld en Z=0
;; ------------------------------------------------
(defun insert_weld (pt)
  (vla-insertblock
    (vla-get-modelspace (vla-get-ActiveDocument (vlax-get-acad-object)))
    (vlax-3d-point (list (car pt) (cadr pt) 0.0))
    "GND_WELD"
    1.0 1.0 1.0 0.0
  )
)

;; ================================================================
;; COMANDO PRINCIPAL
;; ================================================================
(defun c:Incert_Grounding_Weld_Points( / )
  (c:GND-WELD)
)

(defun c:GND-WELD (/ ss i j obj1 obj2 ptlist allpts pt
                       totalLines totalIntersections totalInserted totalSkipped)

  (create_gndweld_block)

  (prompt "\nSeleccione líneas o polilíneas: ")
  (setq ss (ssget '((0 . "LINE,LWPOLYLINE"))))

  (if ss
    (progn
      (setq totalLines         (sslength ss))
      (setq totalIntersections 0)
      (setq totalInserted      0)
      (setq totalSkipped       0)

      (prompt (strcat "\nObjetos detectados: " (itoa totalLines)))

      (setq i 0)
      (while (< i totalLines)
        (setq obj1 (vlax-ename->vla-object (ssname ss i)))
        (setq j (1+ i))

        (while (< j totalLines)
          (setq obj2 (vlax-ename->vla-object (ssname ss j)))

          ;; calcular intersecciones matematicamente
          (setq allpts (get_intersections_math obj1 obj2))

          ;; deduplicar por si dos segmentos adyacentes comparten endpoint
          (setq allpts (dedup_points allpts 0.01))

          (foreach pt allpts
            (setq totalIntersections (1+ totalIntersections))

            ;; UNICA condicion de skip: ya existe bloque ahi
            (if (not (block_exists_at_point pt 0.05))
              (progn
                (insert_weld pt)
                (setq totalInserted (1+ totalInserted))
                (prompt (strcat "\n  -> Weld en: ("
                          (rtos (car pt) 2 4) ", "
                          (rtos (cadr pt) 2 4) ")"))
              )
              (progn
                (setq totalSkipped (1+ totalSkipped))
                (prompt (strcat "\n  -- Omitido (bloque existente) en: ("
                          (rtos (car pt) 2 4) ", "
                          (rtos (cadr pt) 2 4) ")"))
              )
            )
          )

          (setq j (1+ j))
        )
        (setq i (1+ i))
      )

      (prompt (strcat "\n\nIntersecciones detectadas: " (itoa totalIntersections)))
      (prompt (strcat "\nWELDs insertados:          " (itoa totalInserted)))
      (prompt (strcat "\nOmitidos (ya existían):    " (itoa totalSkipped)))
      (prompt "\n")
    )
    (prompt "\nNo se seleccionaron objetos.")
  )
  (princ)
)

;; ================================================================
;; COMANDO DEBUG
;; ================================================================
(defun c:ANSGNDWELDDEBUG (/ ss obj1 obj2 pts)
  (prompt "\n[DEBUG] Seleccione 2 líneas: ")
  (setq ss (ssget '((0 . "LINE,LWPOLYLINE"))))
  (if (and ss (>= (sslength ss) 2))
    (progn
      (setq obj1 (vlax-ename->vla-object (ssname ss 0)))
      (setq obj2 (vlax-ename->vla-object (ssname ss 1)))
      (setq pts (get_intersections_math obj1 obj2))
      (setq pts (dedup_points pts 0.01))
      (prompt (strcat "\nIntersecciones encontradas: " (itoa (length pts))))
      (foreach pt pts
        (prompt (strcat "\n  -> (" (rtos (car pt) 2 4) ", " (rtos (cadr pt) 2 4) ")"))
      )
    )
  )
  (prompt "\n--- FIN DEBUG ---\n")
  (princ)
)

(Load:DescriptionLog "GND-WELD" "Inserts welding points at intersections of HV lines. (does not detect polylines, only lines)")