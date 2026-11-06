
(defun c:SHOWALL ( / )
	(ECHO:LogWrapper "CIV-ShowAll.lsp" "SHOWALL" ShowAllMain)
)


(defun ShowAllMain ( / ss i obj )
  (setq ss (ssget "X"))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq obj (vlax-ename->vla-object (ssname ss i)))
        (vlax-put-property obj 'Visible :vlax-true)
        (setq i (1+ i))
      )
      (princ "\nAll entities are now visible.")
    )
  )
  (princ)
)


(Load:DescriptionLog "SHOWALL" "Sets visibility of all objects to 'true'")
