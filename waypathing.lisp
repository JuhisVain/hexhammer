(in-package :hexhammer)

;; Better do still bodies of water first -- DONE

;;; NOTE: Bezier control points inside hex at 50%
;; of distance between verts or R, whichever longer. (should say shorter??)

;;; Rivers should live in the same hashtable as other paths
;; However the construction of infrastructure should not affect
;; the graphical position of rivers.

(defstruct crd-paths
  (rivers)
  (infrastructure))

(defstruct rivers
  (trunks nil :type list)
  (tributaries nil :type list))

(defstruct river-waypoint
  (crd nil :type crd)
  (point nil :type hex-vertex))

(defstruct river
  (size :stream) ; does this belong here?
  (entry nil :type (or null river-waypoint))
  (exit nil :type (or null river-waypoint)))

(defstruct infra-waypoint
  (crd nil :type crd)
  (point nil :type direction))

(defstruct infrastructure
  (entry nil :type (or null infra-waypoint))
  (exit nil :type (or null infra-waypoint)))

#| ; obsolete
(defun testrivers ()
  (setf (gethash (crd 43 37) *rivers*)
	'((((crd 44 38) :E :stream) ((crd 44 39) :N :stream))
	  ((crd 43 36) :S :stream)))
  (setf (gethash (crd 43 36) *rivers*)
	'((((crd 43 37) :N :stream))
	  ((crd 42 37) :W :stream)))
  (setf (gethash (crd 42 37) *rivers*)
	'((((crd 43 36) :SSE :stream))
	  ((crd 42 36) :S :stream))))
|#
