;;;Posted by Jeff Mishler 
;;;forums.autodesk.com - Visual LISP AutoLISP and General Customization
;;;01-20-2005
(defun get_space ()
  (if (= (getvar "cvport") 1)
    (vla-get-paperspace (vla-get-activedocument (vlax-get-acad-object)))
    (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object)))
    )
  );; get_space

(defun convert2dsolids (/ 2dcoords coords count hatch obj pline space ss)
  (and (setq ss (ssget '((0 . "SOLID"))))
       (setq count -1)
       (setq space (get_space))
       (while (< (setq count (1+ count))
		 (sslength ss))
	 (setq obj (vlax-ename->vla-object (ssname ss count)))
	 (setq coords (vlax-get obj 'coordinates))
	 (setq 2dcoords (list (car coords)(cadr coords)))
	 (setq coords (cdr (cdr (cdr coords))))
	 (setq 2dcoords (append 2dcoords (list (car coords)(cadr coords))))
	 (setq coords (cdr (reverse (cdr (cdr (cdr coords))))))
	 (setq 2dcoords (append 2dcoords (list (cadr coords)(car coords))))
	 (setq coords (reverse coords)) (setq 2dcoords (append 2dcoords (list (car coords)(cadr coords))))
	 (setq pline (vlax-invoke space 'addlightweightpolyline 2dcoords))
	 (vla-put-closed pline :vlax-true)
	 (setq hatch (vlax-invoke space 'addhatch acHatchPatternTypePredefined "SOLID" :vlax-true))
	 (vlax-invoke hatch 'appendouterloop (list pline))
	 (vla-put-color pline (vla-get-color obj))
	 (vla-put-color hatch (vla-get-color obj))
	 (vla-put-layer pline (vla-get-layer obj))
	 (vla-put-layer hatch (vla-get-layer obj))
	 (vla-delete obj)
         (vla-delete pline)
	 )
       )
  );; convert2dsolids

(defun c:SolidToHatch ()
  (convert2dsolids)
  (princ)
);; SolidToHatch



(Load:DescriptionLog "SOLIDTOHATCH" "Converts 2D solids to hatch")