(defun c:CTAL ( / *error* _ss2lst llst olst )
  ;; Copy object(s) To All Layouts (excluding current and Model)
  ;; Alan J. Thompson, 11.23.11 / 2012.09.05

  (vl-load-com)

  (defun *error* (msg)
    (and *AcadDoc* (vla-endundomark *AcadDoc*))
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*QUIT*,")))
      (princ (strcat "\nError: " msg))
    )
  )

  (defun _ss2lst (ss / i l)
    (if ss
      (repeat (setq i (sslength ss))
        (setq l (cons (vlax-ename->vla-object (ssname ss (setq i (1- i)))) l))
      )
    )
  )

  (vla-startundomark
    (cond (*AcadDoc*)
          ((setq *AcadDoc* (vla-get-activedocument (vlax-get-acad-object))))
    )
  )

  (if (eq (getvar 'CVPORT) 1)
    (progn (princ "\nSelect object(s) to copy to all other layouts: ")
           (if (setq llst (list "Model" (getvar 'CTAB))
                     olst (_ss2lst (ssget "_:L" (list (cons 410 (getvar 'CTAB)))))
               )
             (vlax-for layout (vla-get-layouts *AcadDoc*)
               (if (not (member (vla-get-name layout) llst))
                 (vlax-invoke *AcadDoc* 'CopyObjects olst (vla-get-block layout))
               )
             )
           )
    )
    (alert "\n ** Command cannot be executed in ModelSpace! **")
  )

  (*error* nil)
  (princ)
)


(Load:DescriptionLog "CTAL" "Copy objects to all layouts")