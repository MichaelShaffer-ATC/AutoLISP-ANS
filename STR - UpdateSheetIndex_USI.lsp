;; UPDATE SHEET INDEX BASED ON DRAWING INFORMATION AND SHEETS AVAILABLE

(defun c:USI ( / )
	(c:UPDATESHEETINDEX)
)

(defun c:UPDATESHEETINDEX
	(
		/
		;Functions
		*error* ClearTableObject MappedLayoutList
		GetTitleBlock ReturnAttributeValue FormatDate
		;Variables
		tbl row tblk ttl flg dwgps rev date
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
	
	(defun GetTitleBlock ( sht / sel )
		(if (setq sel (ssget "_X" (list (cons 0 "INSERT") (cons 2 "*Title Block*") (cons 410 sht))))
			(vlax-ename->vla-object (cadar (ssnamex sel)))
		)
	)
	;; RETURNS TITLE BLOCK OBJECT BASSED ON PASSED STRING "SHT"
	
	(defun ReturnAttributeValue ( blk str )
		(if (eq (type blk) 'VLA-OBJECT)
			(car
				(vl-remove 'nil
					(mapcar
						'(lambda ( att )
							(if (eq str (vla-get-tagstring att)) (vla-get-textstring att))
						)
						(vlax-safearray->list (vlax-variant-value (vla-getattributes blk)))
					)
				)
			)
		)
	)
	;; RETURNS ATTRIBUTE VALUE OF PASSED STRING (STR) IN PASSED BLOCK OBJECT (BLK) IF IT EXISTS
	
	(defun FormatDate ( dtnum frmt )
		(setq dtnum (itoa dtnum))
		(cond
			((eq frmt "MM/DD/YYYY") ;; "20250519"
				(strcat (substr dtnum 5 2) "/" (substr dtnum 7 2) "/" (substr dtnum 1 4))
			) ;; "05/19/2025"
			( t
				(prompt "\nInvalid format.")
			)
		)
	)
	;; FORMATS THE INTEGER VALUE FOR NUM INTO A STRING VALUE IN SPECIFIED FORMAT
	
	
	(if (and
			(setq tbl (ssget "_X" (list (cons 0 "ACAD_TABLE") (cons 1 "*DRAWING SET LIST*"))));"{\\fArial|b1|i0|c0|p34;DRAWING SET LIST}"
			(setq tbl (vlax-ename->vla-object (cadar (ssnamex tbl))))
			(not (vl-catch-all-error-p (vl-catch-all-apply 'ClearTableObject (list tbl (setq row 2)))))
		)
		(progn
			(setq flg (= (vla-get-columns tbl) 4)) ;; TABLE IS EQUAL TO 4 COLUMNS
			(foreach lyt (MappedLayoutList)
				(vla-insertrows tbl (1+ row) 0.4013 1)
				(vla-settext tbl row 0 (car lyt))
				(if (setq tblk (GetTitleBlock (car lyt)))
					(vla-settext tbl row 1 (if (null (setq ttl (ReturnAttributeValue tblk "SHEET_TITLE"))) "XXXXX" ttl))
					(vla-settext tbl row 1 "XXXXX") ;; TITLE BLOCK NOT FOUND
				)
				(if (and flg tblk)
					(progn
						(vla-settext tbl row 2 (ReturnAttributeValue tblk "REV"))
						(vla-settext tbl row 3 (FormatDate (fix (getvar "CDATE")) "MM/DD/YYYY"))
					)
				) ;; SKIPPED IF TITLE BLOCK IS NOT FOUND OR IF 'flg' IS NOT SET
				(setq row (1+ row))
			)
			(prompt "\n'Drawing Set List' table not found.")
		)
	)
	
	(princ)
)

;(vla-DeleteRows tbl 2 (1- (vla-get-rows tbl)))
;(vla-insertrows tbl row hgt 1)