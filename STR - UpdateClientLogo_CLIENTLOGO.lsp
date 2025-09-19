;; CHANGE VISIBILITY OF 0NESTED BLOCK

(defun c:CLIENTLOGO ( / )
	(UpdateClientLogo t)
)

(defun UpdateClientLogo
	(
		run
		/
		;Functions
		GetDynamicProperty GetNestedBlockFromParent
		;Variables
		clnt tblk bname logo prp
		
	)

	(defun GetNestedBlockFromParent ( blk bnm / dfn obj ret )
		(setq dfn (vl-catch-all-apply 'vla-item (list (vla-get-blocks (vla-get-activedocument (vlax-get-acad-object))) (vla-get-effectivename blk))))
		(if (not (vl-catch-all-error-p dfn))
			(vlax-for obj dfn
				(if
					(and
						(= (vla-get-objectname obj) "AcDbBlockReference")
						(= (strcase (vla-get-effectivename obj)) (strcase bnm))
					)
					(setq ret obj)
				)
			)
		)
		ret
	)
	;; RETURN BLOCK REFERENCE FROM PARENT BLOCK BY NAME
	
	(defun GetDynamicProperty ( blk dnm / prps ret )
		(if (/= (vlax-variant-type (setq prps (vla-getdynamicblockproperties blk))) vlax-vbNull)
			(foreach prp (vlax-safearray->list (vlax-variant-value prps))
				(if (= (vla-get-propertyname prp) dnm)
					(setq ret prp)
				)
			)
		)
		ret
	)
	;; RETURNS DYNAMIC PROPERTY BASED ON PASSED DYNAMIC PROPERTY NAME 'DNM'
	
	(setq clnt (if run (LoadDCLBox) (ParseClientFromPath)))
	;; ESTABLISH CLIENT NAME HERE
	
	(if (setq tblk (ssget "_X" (list (cons 0 "INSERT") (cons 2 "*Title Block*"))))
		(progn
			(setq tblk (vlax-ename->vla-object (cadar (ssnamex tblk))))
			(setq bname "Client Logo")
			(if (and (setq logo (GetNestedBlockFromParent tblk bname)) (setq prp (GetDynamicProperty logo "Visibility1"))) ;;;CHANGE TO PROPER VISIBILITY DISCRIPTION NAME;;;
				(vla-put-value prp (vlax-make-variant clnt vlax-vbString))
				(princ "\nNested 'Client Logo' block not found in title block.")
			) ;; CHANGE VISIBILITY OF LOGO TO 'CLNT' ;; CHECK IF VISIBILITY EXISTS IN BLOCK OBJECT
		)
		(princ "\nValid title block was not found.")
	)
	(vl-cmdf "REGEN")
	(princ)
)
;; RUN IS USED TO DETERMINE IF THE DCL BOX SHOULD DISPLAY FOR USER SELECTION
;; CODE IS CALLABLE FROM OTHER FUNCTIONS USING (UpdateClientLogo nil)


; GET TITLE BLOCK
; CHECK THAT 'CLIENT LOGO' BLOCK EXISTS IN TITLE BLOCK
; CHANGE VISIBILITY OF LOGO BLOCK TO MATCH CLIENT NAME
; USER SELECT VERSION IF CALLED FROM COMMAND LINE