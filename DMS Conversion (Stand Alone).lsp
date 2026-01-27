(defun DMS_Conversion ( ltlg / ZeroPad Convert )
	(defun ZeroPad ( val len )
		(while (< (strlen val) len)
			(setq val (strcat "0" val))
		)
		val
	) ;; ADD 0 PADDING TO BEGINNING OF STRING UNTIL 'LEN' IS MET
	(defun Convert ( val ltf / sw dg mn )
		(if (minusp val) (setq sw t))
		(strcat
			(ZeroPad (itoa (setq dg (fix (abs val)))) 2) (chr 176) " "		;; DEGREES "°"
			(ZeroPad (itoa (fix (setq mn (* (- (abs val) dg) 60)))) 2) "' "	;; MINUTES "'"
			(ZeroPad (rtos (* (abs (- (fix mn) mn)) 60) 2 2) 5) "\" "		;; SECONDS "\""
			(if ltf
				(if (not sw) "N" "S")
				(if (not sw) "E" "W")
			)
		)
	)
	(if (and (listp ltlg) (car ltlg) (cdr ltlg))
		(progn
			(if (= (type (car ltlg)) 'STR)
				(setq ltlg (cons (atof (car ltlg)) (cdr ltlg)))
			)
			(if (= (type (cdr ltlg)) 'STR)
				(setq ltlg (cons (car ltlg) (atof (cdr ltlg))))
			)
			(cons (Convert (car ltlg) t) (Convert (cdr ltlg) nil))
		)
	)
)

