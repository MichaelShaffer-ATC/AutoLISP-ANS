;; USED TO RE-EDIT THE EXISTING DRAWING PROPERTIES BASED ON TEXT FILE
;; THE DRAWING PROPERTIES TEXT FILE SHOULD BE SET UP IN THE FOLLOWING WAY USING ":" AS A DELIMETER BETWEEN KEY AND VALUE:
;; Drawing Key 1: VALUE
;; Drawing Key 2: VALUE
;; Drawing Key 3: VALUE
;; THE CODE ASKS THE USER TO SELECT THE TEXT FILE IN ORDER TO NOT HAVE TO HARDCODE PATH OR FILE NAME
;; IT'S UP TO THE DISCRETION OF THE USER TO SELECT THE CORRECT FILE AND TO MAKE SURE THE SELECTED FILE IS FORMATTED PROPERLY

(defun c:UpdateDrawingProperties ( / dir )
	(setq dir "C:\\_ACC\\ACCDocs\\FastGrid\\ANS_STD\\Project Files\\04_DISCIPLINES\\05_STRUCTURAL\\01_CAD_Standards\\02_Base_Templates\\Drawing Property Order.txt")
	(UpdateDrawingProperties dir)
)

(defun UpdateDrawingProperties ( dir / dwgp txt ParsePropertyData ClearDrawingProperties AddDrawingProperties )
	
	(defun ParsePropertyData ( file / line pos key val lst )
		(setq file (open file "r"))
		(if file
			(progn
				(while (and (setq line (read-line file)) (> (strlen line) 0))
					(if (setq pos (vl-string-search ":" line))
						(setq
							key (substr line 1 pos)
							val (substr line (+ pos 2))
							lst (cons (cons key val) lst)
						)
					)
				)
				(close file)
			)
		)
		lst
	)
	
	(defun ClearDrawingProperties ( dwgp / i )
		(repeat (setq i (vla-numcustominfo dwgp))
			(vl-catch-all-apply 'vla-removecustombyindex (list dwgp (setq i (1- i))))
		)
	)
	
	(defun AddDrawingProperties ( dwgp lst )
		(foreach p lst
			(vl-catch-all-apply 'vla-addcustominfo (list dwgp (car p) (cdr p)))
		)
	)
	
	(if (or (null dir) (not (setq txt (findfile dir))))
		(setq txt (getfiled "Select The Formatted Drawing Property File" "C:\\" "txt" 8))
	)
	(if (setq dwgp (vla-get-summaryinfo (vla-get-activedocument (vlax-get-acad-object))))
		(progn
			(ClearDrawingProperties dwgp)
			(setq lst (ParsePropertyData txt))
			(AddDrawingProperties dwgp (reverse lst))
			t
		)
	)
)
