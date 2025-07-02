;; HIGHLIGHTS SPECFIC SELECTED CIRCLE TYPES (BASED ON FIRST SELECTION AND LAYER) THAT INTERSECT WITH A SELECTED LINE TYPE (POLYLINE)
;; THE MORE THE USER ZOOMS IN, THE MORE ACCURATE THE RESULTS OF THIS PROGRAM WILL BE RETURNED

(defun c:ISOIPC ( / )
	(c:IsoIntersectingPolysCircles)
)

(defun c:IsoIntersectingPolysCircles
	( 
		/
		;Functions
		*error*
		GetSysVariables SetSysVariables
		Create2DCoordinateList ObjectZoomFromEntity
		;Variables
		doc svardef curr
		lin_type cir_type lin_lyr cir_lyr
		lins cirs tmp crds
	)
	
	(defun *error* ( msg )
		(if doc
			(vla-endundomark doc)
		)
		(if curr
			(SetSysVariables curr)
		)
		(if (not (member msg (list "Function cancelled." "quit / exit abort")))
			(vl-bt)
		)
		(princ msg)
	)
	
	(defun GetSysVariables ( vars )
		(mapcar 'cons vars (mapcar 'getvar vars))
	)
	;; GETS SYSTEM VARIABLES BASED ON LIST OF SYSTEM VARIABLE NAMES
	
	
	(defun SetSysVariables ( svardef )
		(mapcar 'setvar (mapcar 'car svardef) (mapcar 'cdr svardef))
	)
	;; SETS SYSTEM VARIABLES USING DOTTED PAIR OF SYSTEM VARIABLE NAMES AND VALUES
	
	(defun Create2DCoordinateList ( lst )
		(if lst
			(cons (list (car lst) (cadr lst)) (Create2DCoordinateList (cddr lst)))
		)
	)
	;; TAKES A FLAT LIST AND CONVERTS IT INTO 2D COORDINATES
	
	(defun ObjectZoomFromEntity ( ent / obj ll ur )
		(setq obj (if (eq (type ent) 'ENAME) (vlax-ename->vla-object ent) ent))
		(vla-getboundingbox obj 'll 'ur)
		(vla-zoomwindow (vlax-get-acad-object) ll ur)
	)
	;; ZOOMS THE CURRENT VIEW TO A PASSED ENTITY OBJECT
	
	(setq doc (vla-get-activedocument (vlax-get-acad-object)))
	(vla-startundomark doc)
	
	(if (/= (getvar "TILEMODE") 1)
		(setvar "TILEMODE" 1)
	)
	;; FORCE TO MODEL SPACE
	
	(setq svardef
		(list
			(cons "OSMODE" 0)
		)
	)
	
	(setq curr (GetSysVariables (mapcar 'car svardef)))
	
	(while (not lin_type)
		(setq lin_type (entsel "Select polyline type for intersection check:"))
		(if (and lin_type (not (eq (cdr (assoc 0 (entget (car lin_type)))) "LWPOLYLINE")))
			(progn
				(setq lin_type nil) ;; START OVER
				(princ "\nSelected entity was not a POLYLINE.\n")
			)
		)
	)
	(while (not cir_type)
		(setq cir_type (entsel "Select circle type for intersection check:"))
		(if (and lin_type (not (eq (cdr (assoc 0 (entget (car cir_type)))) "CIRCLE")))
			(progn
				(setq cir_type nil) ;; START OVER
				(princ "\nSelected entity was not a CIRCLE.\n")
			)
		)
	)
	
	(setq lin_lyr (cdr (assoc 8 (entget (car lin_type)))))
	(setq cir_lyr (cdr (assoc 8 (entget (car cir_type)))))

	(if (setq lins (ssget "X" (list (cons 0 "LWPOLYLINE") (cons 410 "Model") (cons 8 lin_lyr))))
		(progn
			(ObjectZoomFromEntity (car cir_type)) ;; ZOOM VIEW FOR CONSISTENT BEHAVIOR
			(setq lins (mapcar 'vlax-ename->vla-object (mapcar 'cadr (ssnamex lins))))
			(SetSysVariables svardef) ;; SETS SYSTEM VARIABLES TO DEFINED VALUES WITHIN 'SVARDEF'
			(foreach lin lins
				(setq crds (Create2DCoordinateList (safearray-value (vlax-variant-value (vla-get-coordinates lin)))))
				(if (setq tmp (ssget "_F" crds (list (cons 0 "CIRCLE") (cons 410 "Model") (cons 8 cir_lyr))))
					(progn
						(setq tmp (mapcar 'vlax-ename->vla-object (mapcar 'cadr (ssnamex tmp))))
						(mapcar
							'(lambda ( x ) (if (not (null (safearray-value (vlax-variant-value (vla-intersectwith lin x acExtendNone))))) (setq cirs (cons x cirs))))
							tmp
						)
					)
				)
				(setq tmp nil) ;; RESET FOR NEXT ITERATION
			)
			(foreach cir cirs
				(vla-highlight cir :vlax-true)
			)
			(vla-zoomprevious (vlax-get-acad-object))
			(princ (strcat "\nFound " (itoa (length cirs)) " hits in model. Use 'REGEN' to remove highlights."))
		)
	)
	(if curr
		(SetSysVariables curr)
	)
	(vla-endundomark doc)
	(princ)
)
