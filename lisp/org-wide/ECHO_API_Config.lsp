;; URL AND PRIVATE KEY FOR ECHO API ACCESS

;;; TESTING LOCALLY ;;;
; (setq *echo-log-url* "https://localhost:5001/lisp-logs")	;; TESTING LOCAL PORT
; (setq *echo-log-key* nil) 								;; API KEY LEFT UN-SET

;; OPEN-SEARCH METRICS TRACKER: https://echo.ans-team.com/opensearch/goto/a4d569f4d54cbbce9e4011ef27d1d102

(setq *ECHO:Log-URL* "https://app.api.ans-team.com/lisp-logs")		;; PRODUCTION URL
(setq *ECHO:Log-Key* "VBo5RHmOPkJRdR-nlfqv2O5rQjFPEopXZ3RPnwLO8B8") ;; PRODUCTION API KEY

(if (not (boundp '*ECHO:Log-Script*))		(setq *ECHO:Log-Script*			"lisp"))
(if (not (boundp '*ECHO:Log-Command*))		(setq *ECHO:Log-Command*		nil))	; LABEL; FALLS BACK TO 'CMDNAMES'
(if (not (boundp '*ECHO:Log-MaxBuffer*))	(setq *ECHO:Log-MaxBuffer*		25))	; AUTO-FLUSH WHEN THE BUFFER REACHES THIS MANY RECORDS
(if (not (boundp '*ECHO:Log-MaxCapacity*))	(setq *ECHO:Log-MaxCapacity*	1000))	; HARD CAP; OLDEST RECORDS ARE DROPPED PAST THIS
(if (not (boundp '*ECHO:Log-Buffer*))		(setq *ECHO:Log-Buffer*			nil))
(if (not (boundp '*ECHO:Log-InFlight*))		(setq *ECHO:Log-InFlight*		nil))	; ASYNC REQUESTS AWAITING REAP