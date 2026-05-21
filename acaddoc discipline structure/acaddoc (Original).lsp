;; AUTOLOAD COMPILED .LSP (.FAS) FILES FOR USER
;; PLACE ACADDOC FILE IN SUPPORT LOCATION FOR AUTO LOADING
;; ALL .LSP / .FAS FILES MUST BE LOCATED IN 'DIR' LOCATION NOTED IN CODE

(defun c:lisp_load ( / dirs Lisp_Load:AddTrustedPath )
	(setq dirs
		(list
			"C:\\_ACC\\ACCDocs\\ANS Team\\ANS_STD\\Project Files\\04_DISCIPLINES\\05_STRUCTURAL\\01_CAD_Standards\\07_Lisp_Routines\\Load"
			"C:\\_ACC\\ACCDocs\\Fastgrid\\ANS_STD\\Project Files\\04_DISCIPLINES\\05_STRUCTURAL\\01_CAD_Standards\\07_Lisp_Routines\\Load"
		)
	)
	;; "C:\\_ACC\\ACCDocs\\Fastgrid\\ANS_STD\\Project Files\\04_DISCIPLINES\\05_STRUCTURAL\\01_CAD_Standards\\07_Lisp_Routines\\Load"
	;; "C:\\_ACC\\ACCDocs\\ANS Team\\ANS_STD\\Project Files\\04_DISCIPLINES\\05_STRUCTURAL\\01_CAD_Standards\\07_Lisp_Routines\\Load"
	;; SPECIFY LOCATION OF LISP FUNCTIONS TO LOAD ON START
	;; ACC LOCATION TO BE USED AS STANDARD LOADING LOCATION, MULTIPLE PATHS MAY EXIST FOR THE SAME LOCATION
	
	(defun Lisp_Load:AddTrustedPath ( pth )
		(if (not (wcmatch (getvar 'trustedpaths) (strcat "*" pth "*")))
			(cond
				((= (strlen (getvar 'trustedpaths)) 0)
					(setvar 'trustedpaths pth)
				)
				( t
					(setvar 'trustedpaths (strcat (getvar 'trustedpaths) ";" pth))
				)
			)
		)
	)

	(if (/= (getvar 'secureload) 2)
		(setvar 'secureload 2)
	)
	;; SECURELOAD VALUES:
	;; 0 = LOADS ALL EXECUTABLES WITHOUT WARNING. NOT RECOMMENDED FOR SECURITY
	;; 1 = (DEFAULT) LOADS EXECUTABLES FROM TRUSTED LOCATIONS ONLY. SHOWS WARNING PROMPT TO USER IF FILE IS IN AN UNTRUSTED LOCATION. USER AUTHORIZATION REQUIRED
	;; 2 = LOADS EXECUTABLES ONLY IF THEY ARE IN A TRUSTED LOCATION SPECIFIED IN THE TRUSTEDPATHS SYSTEM VARIABLE. IGNORES ALL ATTEMPTS TO LOAD IF NOT SPECIFIED IN TRUSTEDPATHS
	
	(princ "\nLoading compiled AutoLISP files...")
	
	(mapcar
		'(lambda ( dir )
			(Lisp_Load:AddTrustedPath dir)
			(Lisp_Load:AddTrustedPath (strcat dir "\\" "Support"))
			(mapcar '(lambda ( x ) (load (strcat dir "\\" x))) (vl-directory-files dir "*.fas"))
		)
		dirs
	)
	;; ONLY LOADING COMPILED .LSP (.FAS) CODES FOR SECURITY
	;; PREVENTS USERS FROM RUNNING MALICIOUS CODE SINCE IT WILL NEED TO BE CHECKED AND THEN COMPILED
	;; THIS ALSO PREVENTS RUNNING THE ACADDOC.LSP FILE IN AN ENDLESS LOOP IF LOADED FROM ROOT LOCATION "Support"
	
	;(mapcar '(lambda ( x ) (vl-cmdf "_NETLOAD" (strcat dir "DLL\\" x))) (vl-directory-files (strcat dir "DLL Loadout\\") "*.dll"))
	;; FOR FUTURE REFERENCE TO LOAD .DLL FILES WHEN APPLICABLE
	(princ)
)

(vl-load-com) ;; GLOBALLY LOAD ACTIVE-X CONTROLS
(c:lisp_load)

;; AUTOLOAD .LSP / .FAS COMMANDS