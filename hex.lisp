(in-package :hexhammer)

(defstruct hex
  (elevation 0 :type elevation)
  (N-edge NIL :type edge)
  (NW-edge NIL :type edge)
  (SW-edge NIL :type edge)
  (S-edge NIL :type edge)
  (SE-edge NIL :type edge)
  (NE-edge NIL :type edge))

(defstruct edge
  (feature nil :type (or null edge-feature))
  ;;; Vertex elevation:
  (west 0 :type elevation)
  (middle 0 :type elevation)
  (east 0 :type elevation))

(defstruct edge-feature
  )

(defun hex-edge (hex direction)
  (declare (type hex hex)
	   (type direction direction))
  (ecase direction
    (:N (hex-N-edge hex))
    (:NW (hex-NW-edge hex))
    (:SW (hex-SW-edge hex))
    (:S (hex-S-edge hex))
    (:SE (hex-SE-edge hex))
    (:NE (hex-NE-edge hex))))

(defun hex-vertex (hex vert-direction)
  (declare (type hex hex)
	   (type hex-vertex vert-direction))
  (ecase vert-direction
    (:CEN (hex-elevation hex))
    (:NNW (edge-west (hex-N-edge hex)))
    (:N (edge-middle (hex-N-edge hex)))
    (:NNE (edge-east (hex-N-edge hex)))
    (:NE (edge-middle (hex-NE-edge hex)))
    (:E (edge-east (hex-NE-edge hex)))
    (:SE (edge-middle (hex-SE-edge hex)))
    (:SSE (edge-west (hex-SE-edge hex)))
    (:S (edge-middle (hex-S-edge hex)))
    (:SSW (edge-west (hex-S-edge hex)))
    (:SW (edge-middle (hex-SW-edge hex)))
    (:W (edge-west (hex-SW-edge hex)))
    (:NW (edge-middle (hex-NW-edge hex)))))

(defun set-hex-vertex (hex vert-direction new-vert-value)
  (declare (type hex hex)
	   (type hex-vertex vert-direction)
	   (type elevation new-vert-value))
  (ecase vert-direction
    (:CEN (setf (hex-elevation hex) new-vert-value))
    (:NNW (setf (edge-west (hex-N-edge hex)) new-vert-value))
    (:N (setf (edge-middle (hex-N-edge hex)) new-vert-value))
    (:NNE (setf (edge-east (hex-N-edge hex)) new-vert-value))
    (:NE (setf (edge-middle (hex-NE-edge hex)) new-vert-value))
    (:E (setf (edge-east (hex-NE-edge hex)) new-vert-value))
    (:SE (setf (edge-middle (hex-SE-edge hex)) new-vert-value))
    (:SSE (setf (edge-west (hex-SE-edge hex)) new-vert-value))
    (:S (setf (edge-middle (hex-S-edge hex)) new-vert-value))
    (:SSW (setf (edge-west (hex-S-edge hex)) new-vert-value))
    (:SW (setf (edge-middle (hex-SW-edge hex)) new-vert-value))
    (:W (setf (edge-west (hex-SW-edge hex)) new-vert-value))
    (:NW (setf (edge-middle (hex-NW-edge hex)) new-vert-value))))

(defsetf hex-vertex set-hex-vertex)
