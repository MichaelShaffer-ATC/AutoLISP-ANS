;; UPDATE SHEET INDEX BASED ON DRAWING INFORMATION AND SHEETS AVAILABLE
;; UPDATED CODE BASED ON TEMPLATE AND DRAWING INFORMATION CHANGES, TIED TO "REVX" FUNCTION

(defun c:USI ( / )
	(c:UPDATESHEETINDEX)
)

(defun c:UPDATESHEETINDEX
	(
		/
		;Functions
		*error* ClearTableObject MappedLayoutList
		GetTitleBlock ReturnAttributeValue FilterAttributesByTagString
		;Variables
		tbl row tblk ttl rev date
	)
	
	(defun *error* ( msg )
		(if (not (member msg (list "Function cancelled" "quit / exit abort")))
			(vl-bt)
		)
		msg
	)
	
	(defun ClearTableObject ( tbl row )
	  	(if (> (vla-get-rows tbl) row)
			(vla-deleterows tbl row (1- (vla-get-rows tbl)))
		)
	)
	;; DELETE A PASSED TABLE OBJECTS ROWS STARTING AT A PASSED ROW (INTEGER)
	
	(defun MappedLayoutList ( / lst )
		(vlax-for lyt (vla-get-layouts (vla-get-activedocument (vlax-get-acad-object)))
			(setq lst (cons (cons (vla-get-name lyt) lyt) lst))
		)
		(cdr (vl-sort lst '(lambda ( x y ) (< (vla-get-taborder (cdr x)) (vla-get-taborder (cdr y))))))
	)
	;; RETURNS MAPPED LIST OF LAYOUT NAMES WITH THE CORRECPONDING LAYOUT OBJECTS (EXCLUDES "MODEL")
	
	(defun GetTitleBlock ( sht / sel blk )
		(if (setq sel (ssget "_X" (list (cons 0 "INSERT") (cons 2 "*Title Block*") (cons 410 sht))))
			(progn
				(setq blk (vlax-ename->vla-object (cadar (ssnamex sel))))
				(if (eq :vlax-true (vla-get-hasattributes blk))
					blk
				)
			)
		)
	)
	;; RETURNS TITLE BLOCK OBJECT BASSED ON PASSED STRING "SHT"
	
	(defun ReturnAttributeValue ( blk tag )
		(car
			(vl-remove 'nil
				(mapcar
					'(lambda ( att )
						(if (eq tag (vla-get-tagstring att))
							(vla-get-textstring att)
						)
					)
					(FilterAttributesByTagString tag blk)
				)
			)
		)
	)
	;; RETURNS ATTRIBUTE VALUE OF PASSED STRING (tag) IN PASSED BLOCK OBJECT (BLK) IF IT EXISTS
	
	(defun FilterAttributesByTagString ( tag blk / ptn )
		(setq ptn (strcat tag "*"))
		(vl-remove-if-not
			'(lambda ( att )
				(wcmatch (vla-get-tagstring att) ptn)
			)
			(vlax-safearray->list (vlax-variant-value (vla-getattributes blk)))
		)
	)
	;; FILTERS OUT ATTRIBUTES THAT DO NOT MATCH TAG STRING
	
	
	(if (and
			(setq tbl (ssget "_X" (list (cons 0 "ACAD_TABLE") (cons 1 "*SHEET INDEX*"))));"{\\fArial|b1|i0|c0|p34;SHEET INDEX}"
			(setq tbl (vlax-ename->vla-object (cadar (ssnamex tbl))))
			(not (vl-catch-all-error-p (vl-catch-all-apply 'ClearTableObject (list tbl (setq row 2)))))
		)
		(progn
			(foreach lyt (MappedLayoutList)
				(vla-insertrows tbl (1+ row) 0.4013 1)
				(vla-settext tbl row 0 (car lyt))
				(if (setq tblk (GetTitleBlock (car lyt)))
					(vla-settext tbl row 1 (if (null (setq ttl (ReturnAttributeValue tblk "SHEET_TITLE"))) "XXXXX" ttl))
					(vla-settext tbl row 1 "XXXXX") ;; TITLE BLOCK INFORMATION NOT FOUND
				)
				(setq row (1+ row))
			)
		)
		(prompt "\n'Sheet Index' table was not found.")
	)
	(princ)
)

;(vla-DeleteRows tbl 2 (1- (vla-get-rows tbl)))
;(vla-insertrows tbl row hgt 1)