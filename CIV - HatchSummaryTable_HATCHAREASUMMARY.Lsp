(defun c:HATCHAREASUMMARY (/ ss layerName hatch areas totalArea pt tbl i labelPos txtObj reactorList bbox minPt maxPt textPos areaInFeet areaInAcres)
  (setq layerName (getstring "\nEnter the hatch layer name: ")) ; Prompt user for hatch layer
  (setq ss (ssget "X" (list (cons 0 "HATCH") (cons 8 layerName)))) ; Select all hatches on that layer
  
  (if ss
    (progn
      (setq areas '() totalArea 0 pt (getpoint "\nSelect table insertion point: ")) ; Initialize lists and table location
      (princ "\nProcessing hatches...") ; Debugging output

      ;; Loop through each hatch to get area and generate IDs
      (setq i 0)
      (while (< i (sslength ss))
        (setq hatch (ssname ss i))
        (setq hatchObj (vlax-ename->vla-object hatch))

        (if hatchObj
            (progn
              (setq areaInFeet (vla-get-area hatchObj)) ; Get hatch area in square feet
              (setq areaInAcres (/ areaInFeet 43560)) ; Convert area to acres

              ;; Debug output to check the area
              (princ (strcat "\nHatch " (itoa i) " - Area (SQ FEET): " (rtos areaInFeet 2 2) ", Area (Acres): " (rtos areaInAcres 2 4)))

              ;; Generate unique hatch ID and store in areas list
              (setq hatchID (itoa (1+ i))) ; Generate unique number for hatch ID
              (setq areas (append areas (list (list hatchID (rtos areaInFeet 2 2) (rtos areaInAcres 2 4)))))

              (setq totalArea (+ totalArea areaInFeet)) ; Sum total area
            )
            (princ (strcat "\nError: Hatch object " (itoa i) " is invalid."))
        )

        (setq i (1+ i)) ; Move to the next hatch
      )

      ;; Debugging output to verify if areas list is populated
      (if areas
          (progn
            (princ "\nAreas List (before table creation): ")
            (foreach row areas
              (princ (strcat "\nHatch ID: " (car row) ", Area (SQ FEET): " (cadr row) ", Area (Acres): " (caddr row)))
            )
          )
          (princ "\nError: Areas list is empty.")
      )

      ;; Create Table Object if areas are populated
      (if areas
          (progn
            (setq tbl (vla-addtable
                        (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object)))
                        (vlax-3d-point pt)
                        (+ (length areas) 2) ; Rows: header + data + total
                        3 ; Columns (Hatch ID, Area (SQ FEET), Area (Acres))
                        12 ; Row height
                        50 ; Column width
                      )
            )

            ;; Set Table Headers
            (vla-settext tbl 0 0 "Hatch ID")
            (vla-settext tbl 0 1 "Hatch Area (SQ FEET)")
            (vla-settext tbl 0 2 "Hatch Area (Acres)")

            ;; Populate Table with data
            (setq i 1)
            (foreach row areas
              (progn
                (vla-settext tbl i 0 (car row)) ; Hatch ID
                (vla-settext tbl i 1 (cadr row)) ; Hatch Area (SQ FEET)
                (vla-settext tbl i 2 (caddr row)) ; Hatch Area (Acres)
                (setq i (1+ i))
              )
            )

            ;; Add Total Row
            (vla-settext tbl i 0 "TOTAL")
            (vla-settext tbl i 1 (rtos totalArea 2 2)) ; Total area in SQ FEET
            (vla-settext tbl i 2 (rtos (/ totalArea 43560) 2 4)) ; Total area in Acres

            (princ "\nTable created successfully with hatch areas.")
          )
          (princ "\nError: No hatches found on the specified layer.")
      )
    )
    (princ "\nNo hatches found on the specified layer.")
  )
  (princ)
)


(Load:DescriptionLog "HATCHAREASUMMARY" "Creates hatch summary table")