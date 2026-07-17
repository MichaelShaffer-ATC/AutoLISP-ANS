;;;                                                                    ;
;;;  XDATA_VARIANTS.LSP                                                ;
;;;                                                                    ;
;;;  Copyright 1987, 1988, 1990, 1992, 1994, 1996, 1997, 1998, 1999    ;
;;;  by Autodesk, Inc. All Rights Reserved.                            ;
;;;                                                                    ;
;;;  You are hereby granted permission to use, copy and modify this    ;
;;;  software without charge, provided you do so exclusively for       ;
;;;  your own use or for use by others in your organization in the     ;
;;;  performance of their normal duties, and provided further that     ;
;;;  the above copyright notice appears in all copies and both that    ;
;;;  copyright notice and the limited warranty and restricted rights   ;
;;;  notice below appear in all supporting documentation.              ;
;;;                                                                    ;
;;;  Incorporation of any part of this software into other software,   ;
;;;  except when such incorporation is exclusively for your own use    ;
;;;  or for use by others in your organization in the performance of   ;
;;;  their normal duties, is prohibited without the prior written      ;
;;;  consent of Autodesk, Inc.                                         ;
;;;                                                                    ;
;;;  Copying, modification and distribution of this software or any    ;
;;;  part thereof in any form except as expressly provided herein is   ;
;;;  prohibited without the prior written consent of Autodesk, Inc.    ;
;;;                                                                    ;
;;;  AUTODESK PROVIDES THIS SOFTWARE "AS IS" AND WITH ALL FAULTS.      ;
;;;  AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF           ;
;;;  MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK,       ;
;;;  INC. DOES NOT WARRANT THAT THE OPERATION OF THE SOFTWARE          ;
;;;  WILL BE UNINTERRUPTED OR ERROR FREE.                              ;
;;;                                                                    ;
;;;  Restricted Rights for US Government Users.  This software         ;
;;;  and Documentation are provided with RESTRICTED RIGHTS for US      ;
;;;  US Government users.  Use, duplication, or disclosure by the      ;
;;;  Government is subject to restrictions as set forth in FAR         ;
;;;  12.212 (Commercial Computer Software-Restricted Rights) and       ;
;;;  DFAR 227.7202 (Rights in Technical Data and Computer Software),   ;
;;;  as applicable.  Manufacturer is Autodesk, Inc., 111 McInnis       ;
;;;  Parkway, San Rafael, California 94903.                            ;
;;;                                                                    ;

;;;--------------------------------------------------------------------;
;;;  This file shows how to use the new VARIANT and SAFEARRAY          ;
;;;  variables.                                                        ;
;;;--------------------------------------------------------------------;


;;;--------------------------------------------------------------------;
;;;  First of all we have to init the ActiveX interface.               ;
;;;--------------------------------------------------------------------;
(vl-load-com)


;;;--------------------------------------------------------------------;
;;;  Register an application name for the XData.                       ;
;;;--------------------------------------------------------------------;
(regapp "BSDI_LD_072804")

;;;--------------------------------------------------------------------;
;;;  For ActiveX functions, we need to define a global variable which  ;
;;;  "points" to the Model Space portion of the active drawing.  This  ;
;;;  variable, named *ModelSpace* will be created at load time.        ;
;;;--------------------------------------------------------------------;
(setq *ModelSpace*
  (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object)))
) ;_ end of setq

;;;--------------------------------------------------------------------;
;;;  For ActiveX functions, we need to define a global variable which  ;
;;;  "points" to the AutoCAD application object.  This variable, named ;
;;;  *AcadApp* will be created at load time.                           ;
;;;--------------------------------------------------------------------;
(setq *AcadApp*
  (vlax-get-acad-object)
) ;_ end of setq

;;;--------------------------------------------------------------------;
;;;  For the ActiveX Getxxx functions, we need to define a global      ;
;;;  variable which "points" to the AutoCAD Utility object.  This      ;
;;;  variable, named *AcadApp* will be created at load time.           ;
;;;--------------------------------------------------------------------;
(setq *AcadUtility*
  (vla-get-Utility (vla-get-ActiveDocument *AcadApp*))
) ;_ end of setq

;;;--------------------------------------------------------------------;
;;;  This function builds a Lisp list from the two parameters.         ;
;;;  The first parameter is a safearray and contains shorts. These     ;
;;;  shorts are the DXF group codes.                                   ;
;;;  The second parameter is a safearray and contains variants. These  ;
;;;  variants contains the DXF values.                                 ;
;;;--------------------------------------------------------------------;
(defun GetList (DxfTypes DxfValues / LispList Counter Code VarValue Value)
  ;; Get Array bounds
  (if (/= DxfTypes nil)
    (progn
      (setq ListList nil)
      ;; Get the dimension of the safearray
      (setq lBound (vlax-safearray-get-l-bound DxfValues 1)
	    uBound (vlax-safearray-get-u-bound DxfValues 1)
	    Counter lBound)
      (while (<= Counter uBound)
	(setq Code     (vlax-safearray-get-element DxfTypes Counter)
	      VarValue (vlax-safearray-get-element DxfValues Counter)
	      Counter (1+ Counter))
	;; VarValue contains the variant, but we need the Lisp value of it
	(setq Value (vlax-variant-value VarValue))
	;; Create the list
	(setq LispList (append LispList (list (cons Code Value))))
      ) ;_ end of while
    ) ;_ end of progn
    (setq LispList nil)
  ) ;_ end of if
  LispList
) ;_ end of defun

;;;--------------------------------------------------------------------;
;;;  This function builds two VARIANTS from the two parameters.        ;
;;;  The first parameter is a list specifying the DXF group codes, the ;
;;;  second list specifies the DXF values.                             ;
;;;  After converting the parameters into safearrays, this function    ;
;;;  creates two variants containing the arrays.                       ;
;;;--------------------------------------------------------------------;
(defun BuildArrays (DxfTypes dxfValues / ListLength Counter
		                         Code VarValue
		                         ArrayTypes ArrayValues
		    			 VarTypes VarValues Result)
  ;; Get length of the lists
  (setq ListLength (1- (length DxfTypes)))
  ;; Create the safearrays for the dxf group code and value
  (setq ArrayTypes (vlax-make-safearray vlax-vbInteger (cons 0 ListLength))
	ArrayValues (vlax-make-safearray vlax-vbVariant (cons 0 ListLength)))
  ;; Set the array elements
  (setq Counter 0)
  (while (<= Counter ListLength)
    (setq Code (nth Counter DxfTypes)
	  VarValue (vlax-make-variant (nth Counter DxfValues)))
    (vlax-safearray-put-element ArrayTypes Counter Code)
    (vlax-safearray-put-element ArrayValues Counter VarValue)
    (setq counter (1+ counter))
  ) ;_ end of while
  ;; Create the two VARIANTs
  (setq VarTypes  (vlax-make-variant ArrayTypes)
	VarValues (vlax-make-variant ArrayValues))
  ;; Create a (Lisp) list which contains the two safearrays and
  ;; return this list.
  (setq Result (list VarTypes VarValues))
  Result
) ;_ end of defun

;;;--------------------------------------------------------------------;
;;;  This function uses the ActiveX function GetEntity to let the user ;
;;;  select an entity.                                                 ;
;;;--------------------------------------------------------------------;
(defun VlaxSelectEntity (/ VlaxEntity ent)
  (setq ent (car (entsel "\nSelect Entity: ")))
  (setq VlaxEntity (vlax-ename->vla-object ent))
  VlaxEntity
) ;_ end of defun


;;;********************************************************************;
;;;     Function: C:GetXData     The Main function                     ;
;;;--------------------------------------------------------------------;
;;;  Description: This function lets the user select an entity. Then   ;
;;;               it extracts the XData of the selected entity using   ;
;;;               AutoCAD's ActiveX interface.                         ;
;;;********************************************************************;
(defun C:GetXData (/ VlaxEntity Point Types Values xdatas)
  ;; Let the user select an entity.
  (setq VlaxEntity (VlaxSelectEntity))
  (if (/= VlaxEntity nil)
    (progn
      ;; Get the XData
      (vla-getXData VlaxEntity '"" 'Types 'Values)
      ;; Types is a safearray which contains the DXF group codes
      ;; and Values is a safearray which contains the data values.
      ;; Types contains shorts and Values contains Variants.
      ;; Let us extract the information
      (setq xdatas (GetList Types Values))
      ;; Now print the 'normal' Lisp list
      (princ "\nXData attached to the selected entity:\n")
      (print xdatas)
      (princ "\n")
    ) ;_ end of progn
  ) ;_ end of if
  (princ)				; exit quietly
) ;_ end of defun


;;;********************************************************************;
;;;     Function: C:SetXData     The Main function                     ;
;;;--------------------------------------------------------------------;
;;;  Description: This function lets the user select an entity.        ;
;;;               Then it attaches an XData string to the entity using ;
;;;               AutoCAD's ActiveX interface.                         ;
;;;********************************************************************;
(defun C:SetXData (/ VlaxEntity String DxfTypes DxfValues)
  ;; Let the user select an entity
  (setq VlaxEntity (VlaxSelectEntity))
  (if (/= VlaxEntity nil)
    (progn
      ;; Get the string to attach
      (setq String (getstring T "Enter XData string: "))
      (if (/= String nil)
	(progn
	  ;; Create two safearrays for the ActiveX method SetXData
	  (setq xdatas (BuildArrays '(1001 1000) (list "BSDI_LD_072804" String)))
	  ;; Extract the two variants from the returned (Lisp) list
	  (setq DxfTypes (nth 0 xdatas)
		DxfValues (nth 1 xdatas))
	  ;; Set the Xdata
	  (vla-setXData VlaxEntity DxfTypes DxfValues)
          (princ "\nXData attached to the selected entity.\n")
          (princ "\nUse 'GetXData' to get the XData.")
	) ;_ end of progn
      ) ;_ end of if
    ) ;_ end of progn
  ) ;_ end of if
  (princ)				; exit quietly
) ;_ end of defun


;;; Display a message to let the user know the command name
(princ "\nType SetXData to attach XData to an entity.")
(princ "\nType GetXData to show the XData attached to an entity.")
(princ)


(Load:DescriptionLog "GETXDATA" "Returns selected entity's XData")
(Load:DescriptionLog "SETXDATA" "Sets selected entity's XData")