(setq lyts (vla-get-layouts (vla-get-activedocument (vlax-get-acad-object))))

(setq lyt (vla-item lyts "S-001"))

(vlax-dump-object lyt)
(vla-get-name lyt)


(if (setq tblk (ssget "_X" (list (cons 0 "INSERT") (cons 2 "*Title Block*") (cons 410 (vla-get-name lyt)))))
  	(setq tblk (mapcar 'vlax-ename->vla-object (mapcar 'cadr (ssnamex tblk))))
)


;;(ssget "_X" (append (list (cons 0 "INSERT") (cons 2 "*Title Block*") (cons -4  "<OR")) (mapcar '(lambda ( x ) (cons 410 x)) (mapcar 'vla-get-name res) (list (cons -4 "OR>"))))


(mapcar
  	'(lambda ( x )
	   	(if (eq (vla-get-hasattributes x) :vlax-true)
		  	(progn
				(setq atts (vlax-safearray->list (vlax-variant-value (vla-getattributes x))))
			  	;; FUNCTION FOR EACH ATTRIBUTE VALUE
			)
		)
	)
	tblk
)
