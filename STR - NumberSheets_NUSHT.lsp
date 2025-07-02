;; NUMBER SHEETS BASED ON LAYOUT TAB POSITION USING ATTRIBUTE DATA IN ANS TITLE BLOCK

(defun c:NUSHT ( / )
	(c:NUMBERSHEETS)
)

(defun c:NUMBERSHEETS ( / *error* ReplaceAttributeValue ReturnBlockLayoutName ReturnLayoutFromCollection ReturnCustomProperty SetCustomProperty sel )
	
	(defun *error* ( msg )
		(if (not (member msg (list "Function cancelled" "quit / exit abort")))
			(vl-bt)
		)
		msg
	)
	
	(defun ReplaceAttributeValue ( blk tag new )
		(mapcar
			'(lambda ( att )
				(if (eq tag (vla-get-tagstring att)) (vla-put-textstring att new))
			)
			(vlax-safearray->list (vlax-variant-value (vla-getattributes blk)))
		)
	)
	;; REPLACES A PASSED BLOCK OBJECTS ATTRIBUTE "TAG" VALUE WITH "NEW" VALUE
	
	(defun ReturnBlockLayoutName ( blk )
		(if (eq (type blk) 'VLA-OBJECT)
			(cdr (assoc 410 (entget (vlax-vla-object->ename blk))))
		)
	)
	;; RETURNS THE LAYOUT STRING NAME BASED ON A PASSED BLOCK OBJECT
	
	(defun ReturnLayoutFromCollection ( key / lyts rtn )
		(setq lyts (vla-get-layouts (vla-get-activedocument (vlax-get-acad-object))))
		(if (not
				(vl-catch-all-error-p
					(setq rtn (vl-catch-all-apply 'vla-item (list lyts key)))
				)
			)
			rtn
		)
	)
	;; RETRIEVES A LAYOUT OBJECT FROM A COLLECTION OF LAYOUTS BASED ON PASSED STRING FOR THE LAYOUT NAME (KEY)
	
	(defun ReturnCustomProperty ( key / ret )
		(vl-catch-all-apply
			'vla-GetCustomByKey
			(list (vla-get-summaryinfo (vla-get-activedocument (vlax-get-acad-object))) key 'ret)
		)
		ret
	)
	;; RETURNS CUSTOM PROPERTY KEY VALUE IF KEY EXISTS, ELSE NIL
	
	(defun SetCustomProperty ( key val )
		(vl-catch-all-error-p
			(vl-catch-all-apply
				'vla-SetCustomByKey
				(list (vla-get-summaryinfo (vla-get-activedocument (vlax-get-acad-object))) key val)
			)
		)
	)
	;; SETS THE CUSTOM PROPERTY KEY TO A SPECIFIED VALUE (VAL) IF KEY EXISTS AND RETURNS T, ELSE NIL
	
	
	(if (null (setq sel (ssget "_X" (list (cons 0 "INSERT") (cons 2 "*Title Block*")))))
		(prompt "\nError: Proper title block not found.")
		(progn
			(mapcar
				'(lambda ( blk )
					(ReplaceAttributeValue blk "SHT" (vla-get-taborder (ReturnLayoutFromCollection (ReturnBlockLayoutName blk))))
				)
				(mapcar 'vlax-ename->vla-object (mapcar 'cadr (ssnamex sel)))
			)
			(SetCustomProperty "Total Sheets" (itoa (1- (vla-get-count (vla-get-layouts (vla-get-activedocument (vlax-get-acad-object)))))))
			(vl-cmdf "REGEN")
		)
	)
	
	(princ)
	
)