(in-package :hexhammer)

;; Better do still bodies of water first -- DONE

;;; NOTE: Bezier control points inside hex at 50%
;; of distance between verts or R, whichever longer. (should say shorter??)

;;; Rivers should live in the same hashtable as other paths
;; However the construction of infrastructure should not affect
;; the graphical position of rivers.

;; All pathways should be ordered AT EACH HEX-VERTEX POINT
;; Pathway drawing must be done after other terrain features
;; 


(defvar *paths* (make-hash-table :test 'equal))
;; use pointspec (crd hex-vertex) as keys

(defclass point-river ()
  (size
   downstream ; the direction of flow: hex-vertex but not :CEN
   ))

(defclass point-paths ()
  (
   ;; The deciding feature of all paths. Most likely largest river.
   ;; Could also be first piece of infra built.
   master
   ;; list of paths left of master (based on downstream)
   ;; ordered by closest first. Maybe tree with infrastructure.
   left 
   right
   ))

