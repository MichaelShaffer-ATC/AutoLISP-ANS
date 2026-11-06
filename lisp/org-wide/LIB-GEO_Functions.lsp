;; THIS FILE CONTAINS GEOLOCATION FUNCTIONS THAT ARE GLOBALLY AVAILABLE TO ALL OTHER LISP FUNCTIONS
;; FUNCTIONS DEFINED WITHIN THIS FILE READ A DRAWING'S GEOLOCATION AND CONVERT BETWEEN GRID COORDINATES AND LATITUDE / LONGITUDE
;; THIS FILE IS INTENTIONALLY PURE ASCII: AUTOCAD'S LISP READER ABORTS SILENTLY ON NON-ASCII BYTES
;; WORKS IN PLAIN AUTOCAD AND CIVIL 3D, NO MAP 3D (ade_proj*) FUNCTIONS ARE USED

;; HOW AUTOCAD STORES GEOLOCATION:
;;	MODEL SPACE EXTENSION DICTIONARY -> "ACAD_GEOGRAPHICDATA" -> GEODATA OBJECT
;;	(70 . 2)		COORDINATE TYPE, 2 == PROJECTED GRID (THE ONLY TYPE SUPPORTED)
;;	(10 X Y Z)		DESIGN POINT, DRAWING COORDINATES
;;	(11 LON LAT Z)	REFERENCE POINT, DEGREES (LONGITUDE FIRST)
;;	(40 . SCALE)	DRAWING DISPLAY UNIT SCALE, NOT USED: 0.3048 IS SHOWN FOR US SURVEY FEET TOO
;;	(303 . XML)		COORDINATE SYSTEM DEFINITION XML, SPLIT INTO CHUNKS, THE LAST CHUNK IS (301 . XML)
;;
;; THE XML HOLDS EVERY PROJECTION PARAMETER, SO NO ZONE TABLE IS NEEDED
;; COORDINATE SYSTEM UNIT NAMES FOLLOW AUTODESK'S CS-MAP LIBRARY: "FOOT" == US SURVEY FOOT, "IFOOT" == INTERNATIONAL FOOT
;; DRAWING COORDINATES ARE TREATED AS GRID COORDINATES, CHECKED ON READ: THE DESIGN POINT MUST CONVERT TO THE REFERENCE POINT
;;
;; SUPPORTED PROJECTIONS: LAMBERT CONIC CONFORMAL (2SP), USED BY MOST US STATE PLANE ZONES
;; !REVISIT! TRANSVERSE MERCATOR ZONES ARE READ BUT NOT CONVERTED, ADD WHEN A PROJECT NEEDS ONE
;;
;; USE:
; (setq geo (geo:ReadGeoData (vla-get-activedocument (vlax-get-acad-object))))
; (geo:SaveGeoFile "C:\\...\\PPP-GEOLOCATION.txt" geo)	;; KEEP IT FOR DRAWINGS THAT ARE NOT GEOLOCATED
; (setq geo (geo:LoadGeoFile "C:\\...\\PPP-GEOLOCATION.txt"))
; (geo:ToLatLong geo '(2645640.082 6731955.904))		;; -> (32.1153468184 -96.3129023665)
; (geo:FromLatLong geo 32.1153468184 -96.3129023665)	;; -> (2645640.082 6731955.904)
; (geo:LastError)										;; -> REASON THE LAST CALL RETURNED NIL

(if (not (boundp '$geo-LastError)) (setq $geo-LastError nil))
;; !IMPORTANT! THIS VARIABLE MUST REMAIN GLOBAL, HOLDS THE LAST ERROR MESSAGE (NIL IF THE LAST READ SUCCEEDED)



;;; ERRORS AND UNITS ;;;

(defun geo:LastError ( )
	$geo-LastError
)
;; RETURNS THE LAST ERROR MESSAGE, NIL IF THE LAST READ SUCCEEDED


(defun geo:SetError ( msg )
	(setq $geo-LastError msg)
	nil
)
;; STORES AN ERROR MESSAGE FOR geo:LastError, ALWAYS RETURNS NIL SO IT CAN END A FAILED FUNCTION
;; [ msg ]	== ERROR MESSAGE STRING


(defun geo:UnitFactor ( uom / )
	(if (= 'STR (type uom))
		(cdr
			(assoc (strcase uom)
				(list
					(cons "METER" 1.0)
					(cons "METRE" 1.0)
					(cons "FOOT" (/ 1200.0 3937.0)) ;; CS-MAP "FOOT" IS THE US SURVEY FOOT
					(cons "US-FOOT" (/ 1200.0 3937.0))
					(cons "USSFOOT" (/ 1200.0 3937.0))
					(cons "IFOOT" 0.3048)
					(cons "INTERNATIONAL FOOT" 0.3048)
				)
			)
		)
	)
)
;; RETURNS METERS PER UNIT FOR A CS-MAP UNIT NAME, NIL IF THE UNIT IS NOT KNOWN
;; [ uom ]	== UNIT NAME FROM THE COORDINATE SYSTEM XML (EX. "FOOT")



;;; READING GEOLOCATION ;;;

(defun geo:ParseCsXml ( xml / Elements Attr Text Num pcs prj axs ell prm uf a b ecc mth epsg out
						p1 p2 p0 l0 e0 n0 m1 m2 t1 t2 t0 cn cf rho0 )
	
	(defun Elements ( src tag / pos beg end cls ch len out )
		(setq pos 0 len (strlen tag))
		(while (setq beg (vl-string-search (strcat "<" tag) src pos))
			(setq ch (substr src (+ beg len 2) 1)) ;; NEXT CHARACTER MUST END THE TAG NAME, SO "<Ellipsoid" DOES NOT MATCH "<EllipsoidId>"
			(cond
				((not (member ch (list " " ">")))
					(setq pos (1+ beg))
				)
				((null (setq end (vl-string-search ">" src beg)))
					(setq pos (strlen src))
				)
				((null (setq cls (vl-string-search (strcat "</" tag ">") src end)))
					(setq pos (1+ end))
				)
				( t
					(setq
						out (cons (cons (substr src (+ beg len 2) (- end beg len 1)) (substr src (+ end 2) (- cls end 1))) out)
						pos (+ cls len 3)
					)
				)
			)
		)
		(reverse out)
	)
	;; RETURNS ( ATTRIBUTES . CONTENT ) FOR EACH <tag> ELEMENT IN DOCUMENT ORDER, ELEMENTS OF THE SAME TAG MUST NOT BE NESTED
	
	(defun Attr ( atr nam / pos end )
		(if (and (= 'STR (type atr)) (setq pos (vl-string-search (strcat nam "=\"") atr)))
			(progn
				(setq pos (+ pos (strlen nam) 2))
				(if (setq end (vl-string-search "\"" atr pos))
					(substr atr (1+ pos) (- end pos))
				)
			)
		)
	)
	;; RETURNS THE VALUE OF ATTRIBUTE [ nam ] FROM AN ATTRIBUTE STRING, NIL IF IT IS NOT THERE
	
	(defun Text ( src tag / )
		(if (= 'STR (type src))
			(cdar (Elements src tag))
		)
	)
	;; RETURNS THE CONTENT OF THE FIRST <tag> ELEMENT, NIL IF THERE IS NONE
	
	(defun Num ( str / )
		(if (= 'STR (type str))
			(distof (vl-string-trim " \t\r\n" str) 2)
		)
	)
	;; RETURNS A NUMBER FROM ELEMENT CONTENT, NIL IF IT IS NOT A NUMBER
	
	(setq
		pcs (car (Elements xml "ProjectedCoordinateSystem"))
		prj (Text (cdr pcs) "Projection")
		mth (Text prj "OperationMethodId")
		axs (car (Elements (cdr pcs) "Axis"))
		uf (geo:UnitFactor (Attr (car axs) "uom"))
		ell (Text xml "Ellipsoid")
		a (Num (Text ell "SemiMajorAxis"))
	)
	(foreach itm (Elements xml "Alias")
		(if (= "COORDINATESYSTEM" (strcase (cond ((Attr (car itm) "type")) (""))))
			(setq epsg (Attr (car itm) "id"))
		)
	) ;; EPSG CODE, FOR DISPLAY ONLY
	(foreach itm (Elements (cond (prj) ("")) "ParameterValue")
		(setq prm
			(cons
				(cons
					(strcase (vl-string-trim " \t\r\n" (cond ((Text (cdr itm) "OperationParameterId")) (""))))
					(list
						(Num (cdar (Elements (cdr itm) "Value")))
						(Attr (caar (Elements (cdr itm) "Value")) "uom")
					)
				)
				prm
			)
		)
	) ;; ( "PARAMETER NAME" VALUE UOM ) ITEMS
	(cond
		((null pcs)
			(geo:SetError "Geolocation is not a projected coordinate system.")
		)
		((null mth)
			(geo:SetError "Coordinate system has no projection method.")
		)
		((null uf)
			(geo:SetError (strcat "Coordinate system unit is not known: " (cond ((Attr (car axs) "uom")) ("none"))))
		)
		((null a)
			(geo:SetError "Coordinate system ellipsoid has no semi-major axis.")
		)
		((null (setq b
				(cond
					((Num (Text ell "SemiMinorAxis")))
					((setq b (Num (Text ell "InverseFlattening")))
						(* a (- 1.0 (/ 1.0 b)))
					)
				)
			))
			(geo:SetError "Coordinate system ellipsoid has no semi-minor axis or flattening.")
		)
		( t
			(setq
				ecc (sqrt (- 1.0 (/ (* b b) (* a a))))
				out
					(list
						(cons "code" (Attr (car pcs) "id"))
						(cons "epsg" epsg)
						(cons "method" mth)
						(cons "unit" (Attr (car axs) "uom"))
						(cons "ufac" uf)
						(cons "xml" xml)
					)
			)
			(if (wcmatch (strcase mth) "LAMBERT CONIC CONFORMAL (2SP)*")
				(progn
					(mapcar
						'(lambda ( sym key / itm )
							(setq itm (assoc key prm))
							(set sym
								(cond
									((null (cadr itm))
										nil
									)
									((= "DEGREE" (strcase (cond ((caddr itm)) (""))))
										(* pi (/ (cadr itm) 180.0))
									)
									((geo:UnitFactor (caddr itm))
										(* (cadr itm) (geo:UnitFactor (caddr itm))) ;; LENGTHS TO METERS
									)
								)
							)
						)
						'(p1 p2 l0 p0 e0 n0)
						(list
							"LATITUDE OF 1ST STANDARD PARALLEL" "LATITUDE OF 2ND STANDARD PARALLEL"
							"LONGITUDE OF FALSE ORIGIN" "LATITUDE OF FALSE ORIGIN" "FALSE EASTING" "FALSE NORTHING"
						)
					)
					(if (vl-every 'numberp (list p1 p2 l0 p0 e0 n0))
						(progn
							(setq
								m1 (geo:LccM p1 ecc)
								m2 (geo:LccM p2 ecc)
								t1 (geo:LccT p1 ecc)
								t2 (geo:LccT p2 ecc)
								t0 (geo:LccT p0 ecc)
								cn (if (equal p1 p2 1e-12)
										(sin p1) ;; EQUAL PARALLELS, THE CONE TOUCHES AT ONE LATITUDE
										(/ (- (log m1) (log m2)) (- (log t1) (log t2)))
									)
								cf (/ m1 (* cn (expt t1 cn)))
								rho0 (* a cf (expt t0 cn))
							)
							(append out (list (list "lcc" a ecc cn cf rho0 l0 e0 n0)))
						)
						(geo:SetError "Lambert projection is missing a standard parallel, origin, or false easting / northing.")
					)
				)
				out ;; READ BUT NOT CONVERTIBLE, geo:ToLatLong REPORTS THE METHOD
			)
		)
	)
)
;; READS A COORDINATE SYSTEM DEFINITION (AUTODESK / OSGEO XML, AS STORED IN GEODATA) INTO AN ASSOCIATION LIST
;; RETURNS ( ("code" . "TX83-NCF") ("epsg" . "2276") ("method" . ...) ("unit" . "FOOT") ("ufac" . METERS-PER-UNIT) ("xml" . ...) ("lcc" ...) )
;; "lcc" == ( A ECCENTRICITY N F RHO0 LON0 FALSE-EASTING FALSE-NORTHING ), ANGLES IN RADIANS, LENGTHS IN METERS, ONLY FOR LAMBERT 2SP
;; RETURNS NIL IF THE XML IS NOT A USABLE PROJECTED COORDINATE SYSTEM (SEE geo:LastError)
;; [ xml ]	== COORDINATE SYSTEM XML STRING


(defun geo:LccM ( phi ecc / )
	(/ (cos phi) (sqrt (- 1.0 (expt (* ecc (sin phi)) 2))))
)
;; LAMBERT HELPER, RETURNS m FOR A LATITUDE (RADIANS) ON THE ELLIPSOID


(defun geo:LccT ( phi ecc / esn )
	(setq esn (* ecc (sin phi)))
	(/
		(/ (sin (- (* 0.25 pi) (* 0.5 phi))) (cos (- (* 0.25 pi) (* 0.5 phi)))) ;; TAN( PI/4 - PHI/2 )
		(expt (/ (- 1.0 esn) (+ 1.0 esn)) (* 0.5 ecc))
	)
)
;; LAMBERT HELPER, RETURNS t FOR A LATITUDE (RADIANS) ON THE ELLIPSOID


(defun geo:BuildGeo ( xml dsg ref / cs out chk )
	(setq $geo-LastError nil)
	(cond
		((not (and (= 'STR (type xml)) (vl-consp dsg) (vl-consp ref)))
			(geo:SetError "Geolocation is missing its coordinate system, design point, or reference point.")
		)
		((null (setq cs (geo:ParseCsXml xml)))
			nil ;; geo:ParseCsXml SET THE ERROR
		)
		( t
			(setq out (append (list (cons "design" dsg) (cons "reference" ref)) cs))
			(cond
				((null (assoc "lcc" out))
					out ;; NOT CONVERTIBLE YET, NOTHING TO CHECK
				)
				((> (setq chk (geo:CheckGeoData out)) 0.01)
					(geo:SetError
						(strcat
							"Geolocation check failed: the design point is " (rtos chk 2 3) " units from its reference point."
							" Check the coordinate system and units."
						)
					)
				)
				( t
					(append out (list (cons "check" chk)))
				)
			)
		)
	)
)
;; BUILDS A GEOLOCATION LIST FROM A COORDINATE SYSTEM XML, DESIGN POINT, AND REFERENCE POINT, THEN RUNS THE SAFETY CHECK
;; SHARED BY geo:ReadGeoData (FROM A DRAWING) AND geo:LoadGeoFile (FROM A SAVED FILE)
;; RETURNS THE geo:ParseCsXml LIST PLUS ("design" X Y Z), ("reference" LON LAT Z), AND ("check" . DISTANCE IN DRAWING UNITS)
;; FAILS IF THE DESIGN POINT DOES NOT CONVERT TO WITHIN 0.01 DRAWING UNITS OF THE REFERENCE POINT (WRONG ZONE OR UNITS)
;; RETURNS NIL IF THE GEOLOCATION IS NOT USABLE (SEE geo:LastError)
;; [ xml ]	== COORDINATE SYSTEM XML STRING
;; [ dsg ]	== DESIGN POINT ( X Y Z ), DRAWING COORDINATES
;; [ ref ]	== REFERENCE POINT ( LON LAT Z ), DEGREES


(defun geo:ReadGeoData ( doc / ms obj dxf )
	(setq $geo-LastError nil)
	(cond
		((or
				(vl-catch-all-error-p (setq ms (vl-catch-all-apply 'vla-get-modelspace (list doc))))
				(= :vlax-false (vla-get-hasextensiondictionary ms)) ;; CHECK FIRST, GETEXTENSIONDICTIONARY WOULD CREATE AN EMPTY ONE
				(vl-catch-all-error-p
					(setq obj (vl-catch-all-apply '(lambda ( ) (vla-item (vla-getextensiondictionary ms) "ACAD_GEOGRAPHICDATA"))))
				)
			)
			(geo:SetError "Drawing is not geolocated.")
		)
		((or
				(vl-catch-all-error-p (setq dxf (vl-catch-all-apply '(lambda ( ) (entget (vlax-vla-object->ename obj))))))
				(null dxf)
			)
			(geo:SetError "Geolocation data could not be read.")
		)
		((/= 2 (cdr (assoc 70 dxf)))
			(geo:SetError "Geolocation is not a projected grid, only projected coordinate systems are supported.")
		)
		( t
			(geo:BuildGeo
				(apply 'strcat
					(append
						(mapcar 'cdr (vl-remove-if-not '(lambda ( itm ) (= 303 (car itm))) dxf))
						(list (cond ((cdr (assoc 301 dxf))) (""))) ;; 301 HOLDS THE LAST CHUNK
					)
				)
				(cdr (assoc 10 dxf))
				(cdr (assoc 11 dxf))
			)
		)
	)
)
;; READS THE GEOLOCATION OF A DRAWING, [ doc ] MAY BE THE ACTIVE DOCUMENT OR AN OBJECTDBX DOCUMENT
;; RETURNS THE geo:BuildGeo LIST, NIL IF THE DRAWING IS NOT GEOLOCATED OR THE GEOLOCATION IS NOT USABLE (SEE geo:LastError)
;; ENTGET ON AN OBJECTDBX DOCUMENT WORKS (CONFIRMED 10-2026: PPPSETUP READ THE ORCA FALLS X-ARRAY GEOLOCATION THIS WAY)



;;; CONVERSIONS ;;;

(defun geo:ToLatLong ( geo pnt / lcc a ecc cn cf rho0 l0 e0 n0 uf x y r tt th phi prv esn idx )
	(cond
		((not (setq lcc (cdr (assoc "lcc" geo))))
			(geo:SetError (strcat "Projection is not supported yet: " (cond ((cdr (assoc "method" geo))) ("unknown"))))
		)
		( t
			(mapcar 'set '(a ecc cn cf rho0 l0 e0 n0) lcc)
			(setq
				uf (cdr (assoc "ufac" geo))
				x (- (* uf (car pnt)) e0)
				y (- rho0 (- (* uf (cadr pnt)) n0))
				r (* (if (minusp cn) -1.0 1.0) (sqrt (+ (* x x) (* y y))))
				th (if (minusp cn) (atan (- x) (- y)) (atan x y))
				tt (expt (/ r (* a cf)) (/ 1.0 cn))
				phi (- (* 0.5 pi) (* 2.0 (atan tt)))
				idx 0
			)
			(while (and (< idx 15) (or (null prv) (not (equal phi prv 1e-14))))
				(setq
					prv phi
					esn (* ecc (sin phi))
					phi (- (* 0.5 pi) (* 2.0 (atan (* tt (expt (/ (- 1.0 esn) (+ 1.0 esn)) (* 0.5 ecc))))))
					idx (1+ idx)
				)
			) ;; CONVERGES IN A FEW STEPS
			(list
				(* 180.0 (/ phi pi))
				(* 180.0 (/ (+ (/ th cn) l0) pi))
			)
		)
	)
)
;; CONVERTS A GRID POINT TO ( LATITUDE LONGITUDE ) IN DECIMAL DEGREES, WEST LONGITUDE IS NEGATIVE
;; RETURNS NIL IF THE PROJECTION IS NOT SUPPORTED (SEE geo:LastError)
;; [ geo ]	== RESULT OF geo:ReadGeoData
;; [ pnt ]	== ( X Y ) OR ( X Y Z ) IN DRAWING UNITS, Z IS IGNORED


(defun geo:FromLatLong ( geo lat lon / lcc a ecc cn cf rho0 l0 e0 n0 uf r th )
	(cond
		((not (setq lcc (cdr (assoc "lcc" geo))))
			(geo:SetError (strcat "Projection is not supported yet: " (cond ((cdr (assoc "method" geo))) ("unknown"))))
		)
		( t
			(mapcar 'set '(a ecc cn cf rho0 l0 e0 n0) lcc)
			(setq
				uf (cdr (assoc "ufac" geo))
				r (* a cf (expt (geo:LccT (* pi (/ lat 180.0)) ecc) cn))
				th (* cn (- (* pi (/ lon 180.0)) l0))
			)
			(list
				(/ (+ e0 (* r (sin th))) uf)
				(/ (+ n0 (- rho0 (* r (cos th)))) uf)
			)
		)
	)
)
;; CONVERTS LATITUDE / LONGITUDE (DECIMAL DEGREES) TO A GRID POINT ( X Y ) IN DRAWING UNITS
;; RETURNS NIL IF THE PROJECTION IS NOT SUPPORTED (SEE geo:LastError)
;; [ geo ]	== RESULT OF geo:ReadGeoData
;; [ lat ]	== LATITUDE, DEGREES
;; [ lon ]	== LONGITUDE, DEGREES (WEST IS NEGATIVE)


(defun geo:CheckGeoData ( geo / ref dsg pnt )
	(if (and
			(setq ref (cdr (assoc "reference" geo)))
			(setq dsg (cdr (assoc "design" geo)))
			(setq pnt (geo:FromLatLong geo (cadr ref) (car ref)))
		)
		(distance (list (car pnt) (cadr pnt)) (list (car dsg) (cadr dsg)))
	)
)
;; CONVERTS THE REFERENCE POINT (LAT / LONG) TO GRID AND MEASURES HOW FAR IT LANDS FROM THE DESIGN POINT
;; A CORRECT ZONE AND UNIT GIVES A NEAR-ZERO DISTANCE, A WRONG FOOT DEFINITION IS OFF BY FEET, A WRONG ZONE BY MILES
;; RETURNS THE DISTANCE IN DRAWING UNITS, NIL IF THE PROJECTION IS NOT SUPPORTED
;; [ geo ]	== RESULT OF geo:ReadGeoData


(defun geo:SameSystem ( ga gb / )
	(and
		ga
		gb
		(= 'STR (type (cdr (assoc "code" ga))))
		(= (strcase (cdr (assoc "code" ga))) (strcase (cond ((cdr (assoc "code" gb))) (""))))
		(= (strcase (cond ((cdr (assoc "unit" ga))) (""))) (strcase (cond ((cdr (assoc "unit" gb))) (""))))
	)
)
;; RETURNS T IF TWO GEOLOCATIONS USE THE SAME COORDINATE SYSTEM AND UNIT, SO THEIR DRAWINGS LINE UP AT 0,0,0 WITHOUT A TRANSFORM
;; RETURNS NIL IF THEY DIFFER OR EITHER IS MISSING, CHECK FOR A MISSING ONE FIRST WHEN THAT IS ALLOWED
;; [ ga gb ]	== RESULTS OF geo:ReadGeoData OR geo:LoadGeoFile



;;; GEOLOCATION FILES ;;;

(defun geo:SaveGeoFile ( pth geo / des err dsg ref )
	(setq
		dsg (cdr (assoc "design" geo))
		ref (cdr (assoc "reference" geo))
	)
	(cond
		((not (and (= 'STR (type (cdr (assoc "xml" geo)))) dsg ref))
			nil
		)
		((setq des (open pth "w"))
			(setq err
				(vl-catch-all-apply
					'(lambda ( )
						(write-line ";; GEOLOCATION - GENERATED FILE, READ WITH geo:LoadGeoFile (LIB-GEO_Functions.lsp)" des)
						(write-line (strcat ";; COORDINATE SYSTEM: " (cond ((cdr (assoc "code" geo))) ("unknown")) (if (cdr (assoc "epsg" geo)) (strcat " (EPSG " (cdr (assoc "epsg" geo)) ")") "")) des)
						(write-line (strcat "DESIGN " (apply 'strcat (mapcar '(lambda ( v ) (strcat (rtos v 2 12) " ")) dsg))) des)
						(write-line (strcat "REFERENCE " (apply 'strcat (mapcar '(lambda ( v ) (strcat (rtos v 2 15) " ")) ref))) des)
						(write-line "XML" des)
						(write-line (cdr (assoc "xml" geo)) des)
					)
				)
			)
			(close des)
			(not (vl-catch-all-error-p err))
		)
	)
)
;; SAVES A GEOLOCATION (FROM geo:ReadGeoData) TO A TEXT FILE, SO DRAWINGS THAT ARE NOT GEOLOCATED CAN STILL CONVERT COORDINATES
;; NUMBERS ARE WRITTEN WITH RTOS AT FULL PRECISION, PRINTING A LIST WOULD ROUND THEM TO 6 SIGNIFICANT DIGITS
;; RETURNS T IF SUCCESSFUL, ELSE NIL
;; [ pth ]	== FULL FILE PATH
;; [ geo ]	== RESULT OF geo:ReadGeoData


(defun geo:LoadGeoFile ( pth / des lin xml dsg ref val )
	(setq $geo-LastError nil)
	(cond
		((not (and (= 'STR (type pth)) (setq pth (findfile pth)) (setq des (open pth "r"))))
			(geo:SetError "Geolocation file was not found.")
		)
		( t
			(vl-catch-all-apply
				'(lambda ( )
					(while (setq lin (read-line des))
						(cond
							(xml
								(setq xml (cons lin xml))
							)
							((= "XML" lin)
								(setq xml (list "")) ;; EVERYTHING AFTER THIS LINE IS THE XML
							)
							((wcmatch lin "DESIGN *,REFERENCE *")
								(setq val (vl-catch-all-apply 'read (list (strcat "(" (substr lin (+ 2 (vl-string-search " " lin))) ")"))))
								(if (and (not (vl-catch-all-error-p val)) (vl-every 'numberp val))
									(if (wcmatch lin "DESIGN *")
										(setq dsg val)
										(setq ref val)
									)
								)
							)
						)
					)
				)
			) ;; A FAILED READ STILL FALLS THROUGH TO CLOSE, SO THE FILE IS NEVER LEFT OPEN
			(close des)
			(geo:BuildGeo
				(if xml (apply 'strcat (mapcar '(lambda ( itm ) (strcat itm "\n")) (cdr (reverse xml)))))
				dsg
				ref
			)
		)
	)
)
;; READS A GEOLOCATION FILE WRITTEN BY geo:SaveGeoFile AND RUNS THE SAME SAFETY CHECK AS geo:ReadGeoData
;; RETURNS THE geo:BuildGeo LIST, NIL IF THE FILE IS MISSING OR NOT USABLE (SEE geo:LastError)
;; [ pth ]	== FULL FILE PATH


