;; AUTOLOAD COMPILED .LSP (.FAS) FILES FOR USER
;; PLACE ACADDOC FILE IN SUPPORT LOCATION FOR AUTO LOADING
;; ALL .LSP / .FAS FILES MUST BE LOCATED IN 'DIR' LOCATION NOTED IN CODE

(defun c:lisp_load ( / dsp dir Load:Functions )
	(setq dsp "HV")
	(setq dir (strcat "C:\\_ACC\\ACCDocs\\ANS Team\\ANS_STD\\Project Files\\05_SOFTWARE_SUPPORT\\AutoLISP\\" dsp "\\Load"))
	; C:\_ACC\ACCDocs\ANS Team\ANS_STD\Project Files\05_SOFTWARE_SUPPORT\AutoLISP\[ DISCIPLINE ]\Load\Support
	
	(if (/= (getvar 'secureload) 0)
		(setvar 'secureload 0)
	)
	;; SECURELOAD VALUES:
	;; 0 = LOADS ALL EXECUTABLES WITHOUT WARNING. NOT RECOMMENDED FOR SECURITY
	;; 1 = (DEFAULT) LOADS EXECUTABLES FROM TRUSTED LOCATIONS ONLY. SHOWS WARNING PROMPT TO USER IF FILE IS IN AN UNTRUSTED LOCATION. USER AUTHORIZATION REQUIRED
	;; 2 = LOADS EXECUTABLES ONLY IF THEY ARE IN A TRUSTED LOCATION SPECIFIED IN THE TRUSTEDPATHS SYSTEM VARIABLE. IGNORES ALL ATTEMPTS TO LOAD IF NOT SPECIFIED IN TRUSTEDPATHS
	
	(defun Load:Functions ( dsp dir )
		(if (null (vl-file-directory-p dir))
			(princ (strcat "\nCompiled \"" dsp "\" AutoLISP functions failed to load. Directory not found.\nDirectory: \"" dir "\""))
			(progn
				(princ (strcat "\nLoading compiled \"" dsp "\" AutoLISP files...\n\n"))
				(mapcar '(lambda ( x ) (setq Load:FileName x) (load (strcat dir "\\" x))) (vl-directory-files dir "*.fas"))
			)
		)
		(princ)
	)
	
	(Load:Functions dsp dir)
	(setq dir
		(strcat
			(substr dir 1 (- (strlen dir) (strlen (strcat "\\" dsp "\\Load"))))
			"\\ANS\\Global"
		)
	)
	(Load:Functions "General ANS" dir)
	;; ONLY LOADING COMPILED .LSP (.FAS) CODES FOR SECURITY
	;; PREVENTS USERS FROM RUNNING MALICIOUS CODE SINCE IT WILL NEED TO BE CHECKED AND THEN COMPILED
	;; THIS ALSO PREVENTS RUNNING THE ACADDOC.LSP FILE IN AN ENDLESS LOOP IF LOADED FROM ROOT LOCATION "Support"
	
	;(mapcar '(lambda ( x ) (vl-cmdf "_NETLOAD" (strcat dir "DLL\\" x))) (vl-directory-files (strcat dir "DLL Loadout\\") "*.dll"))
	;; FOR FUTURE REFERENCE TO LOAD .DLL FILES WHEN APPLICABLE
	
	(if Load:FileName (setq Load:FileName nil)) ;; GLOBAL VARIABLE FOR DESCRIPTION LOGGER
	
	(princ)
)


(defun Load:DescriptionLog ( cmd desc )
	(princ
	  	(strcat
		  	"\n>> LOADED: " (strcase Load:FileName)
			"\n   - Callback:    " (strcase cmd)
			"\n   - Description: " desc
			"\n------------------------------------------------"
		)
	)
	(princ)
)

(vl-load-com) ;; GLOBALLY LOAD ACTIVE-X CONTROLS
(c:lisp_load)

;; AUTOLOAD .LSP / .FAS COMMANDS