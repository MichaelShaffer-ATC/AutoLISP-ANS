;; LOCK ALL VIEWPORTS

(defun c:LVP ( / ss )
	(if (setq ss (ssget "X" (list (cons 0 "VIEWPORT") (cons -4 "<NOT") (cons 69 1) (cons -4 "NOT>"))))
		(progn
			(mapcar '(lambda ( x ) (vla-put-displaylocked (vlax-ename->vla-object x) :vlax-true)) (mapcar 'cadr (ssnamex ss)))
			(princ "\nAll Viewports Locked!")
		)
		(princ "\nNo viewports were found.")
	)
	(princ)
)

