;; Copy current layout page setup to all layout tabs
(vl-load-com)
(defun c:CPS ( / Adoc Layts clyt )
  (setq	aDoc  (vla-get-activedocument (vlax-get-acad-object))
	Layts (vla-get-layouts aDoc)
	clyt  (vla-get-activelayout aDoc)
  )
  (foreach itm (vl-remove (vla-get-name clyt) (layoutlist))
    (vla-copyfrom (vla-item Layts itm) clyt)
  )
  (princ)
)


(Load:DescriptionLog "CPS" "Copy page setup to all layouts")