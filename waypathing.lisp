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

'(defstruct crd-river
  (entry)
  (exit)
  (size))

'(defstruct crd-paths
  ;;;; MASTER: Probably largest river.
  ;; Can't be null.
  ;; All other paths must be subordinate to this one.
  (master)
  ;; Ordering by right/left of master maybe ???
  )
