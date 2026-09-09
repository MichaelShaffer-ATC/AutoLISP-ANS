;; ============================================================
;;  OUS2GA-HV_STD.lsp
;;  Autor  : Samuel.fuentes@ans-team.com
;;  Version: 1.0 / Mayo 2024
;;
;;  DESCRIPCION GENERAL:
;;    Comando: OUS2GA-HV_STD
;;    Objetivo: Limpiar y estandarizar vistas de Subestaciones HV de OUS a archivos X-Ref.
;;
;;  FLUJO DE USO:
;;    1) Carga Visual LISP / COM.
;;    2) Selecciona todas las entidades.
;;    3) Aplica propiedades por capa.
;;    4) Crea capa HV-PHYSICAL si no existe.
;;    5) Mueve la selección a la capa HV-PHYSICAL.
;;    6) Establece objetos en bloques por capa.
;;    7) Ejecuta Overkill para limpiar duplicados.
;;    8) Purga elementos innecesarios.
;;    9) Elimina capas no HV.
;;    10) Purga nuevamente.
;;    11) Ajusta el zoom a los límites.
;;    12) Elimina objetos "punto" no necesarios.
;;
;; ============================================================

(defun OUS2GA-HV_STD-SelectAll ()
  (ssget "_X")
)

(defun OUS2GA-HV_STD-ForceSelectionByLayer (ss)
  
  (if ss
    (setq idx 0)
    (while (< idx (sslength ss))
      (setq obj (vlax-ename->vla-object (ssname ss idx)))
      (vla-put-Color obj acByLayer)
      (vla-put-Linetype obj "ByLayer")
      (vla-put-Lineweight obj acLnWtByLayer)
      (setq idx (1+ idx))
    )
    
  )
)

(defun OUS2GA-HV_STD-CreateLayerIfMissing (layerName colorIndex)
  (if (not (tblsearch "LAYER" layerName))
    (command "-layer" "M" layerName "")
  )
  (command "-layer" "C" colorIndex layerName "")
)

(defun OUS2GA-HV_STD-MoveSelectionToLayer (ss layerName)
  (if ss
    (command "CHPROP" ss "" "LA" layerName "")
  )
)

(defun OUS2GA-HV_STD-SetObjectsInBlocksByLayer ()
  (command "-setbylayer" "ALL" "" "YES" "YES")
)

(defun OUS2GA-HV_STD-OverkillAll (max_iterations)
  (setq i 0)
  (while (< i max_iterations)
    (setq count_before (sslength (ssget "X")))
    (command "-overkill" "ALL" "" "")
    (setq count_after (sslength (ssget "X")))
    (if (= count_before count_after)
      (setq i max_iterations) ; No se eliminaron objetos, salir del bucle
      (setq i (1+ i))
    )
  )
)

(defun OUS2GA-HV_STD-PurgeAll ()
  (command "-purge" "REGAPPS" "*" "N")
  (command "-purge" "ALL" "*" "N")
  (command "-purge" "ALL" "*" "N")
)

(defun OUS2GA-HV_STD-DeleteNonHvLayers ()
  (setq lay (tblnext "LAYER" T))
  (while lay
    (setq name (cdr (assoc 2 lay)))
    (if (and (/= name "0") (/= name "HV-PHYSICAL"))
        (progn
          (command "-layer" "SET" "0" "")
          (command "-laydel" "N" name "")
        )
    )
    (setq lay (tblnext "LAYER"))
  )
)

(defun OUS2GA-HV_STD-ZoomExtends ()
  (command "ZOOM" "E")
)

(defun OUS2GA-HV_STD-DeletePoints ()
  (setq ss_points (ssget "X" '((0 . "POINT"))))
  (if ss_points
    (command "ERASE" ss_points "")
  )
)

(defun c:OpenUtilities2GeneralArrangement ( / )
  (c:OUS2GA)
)
;;Principal función para ejecutar la limpieza y estandarización
(defun c:OUS2GA (/ ss)
  (vl-load-com)

  (prompt "\nHV CLEAN STARTED...")

  (setq ss (OUS2GA-HV_STD-SelectAll))
  (OUS2GA-HV_STD-ForceSelectionByLayer ss)
  (OUS2GA-HV_STD-CreateLayerIfMissing "HV-PHYSICAL" "170")
  (OUS2GA-HV_STD-MoveSelectionToLayer ss "HV-PHYSICAL")
  (OUS2GA-HV_STD-SetObjectsInBlocksByLayer)
  (OUS2GA-HV_STD-OverkillAll 9)
  (OUS2GA-HV_STD-PurgeAll)
  (OUS2GA-HV_STD-DeleteNonHvLayers)
  (OUS2GA-HV_STD-PurgeAll)
  (OUS2GA-HV_STD-ZoomExtends)
  (OUS2GA-HV_STD-DeletePoints)

  (prompt "\nHV CLEAN COMPLETED.")
  (princ)
)

(Load:DescriptionLog "OpenUtilities2GeneralArrangement / OUS2GA" "Clean the drawing exported from OUS and apply Layer & Color Standarized by ANS to HV Physical substations")
