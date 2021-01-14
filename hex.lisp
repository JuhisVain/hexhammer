(in-package :hexhammer)

(defstruct hex
  (elevation 0 :type elevation)
  (N-edge NIL :type (or edge null))
  (NW-edge NIL :type (or edge null))
  (SW-edge NIL :type (or edge null))
  (S-edge NIL :type (or edge null))
  (SE-edge NIL :type (or edge null))
  (NE-edge NIL :type (or edge null)))

(defstruct edge
  ;;; Vertex elevation:
  (west 0 :type elevation) ;; TODO: west and east should be boxed
  (middle 0 :type elevation)
  (east 0 :type elevation))

(defstruct contours
  (left 0 :type (signed-byte 8))
  (range 0 :type (signed-byte 8))
  (water-left 0 :type (signed-byte 8))
  (water-right 0 :type (signed-byte 8))
  (deque nil :type range-deque))

(defun record-contours (hex left right divisor)
  (let* ((left-ele (round (hex-vertex hex left)
			  divisor))
	 (right-ele (round (hex-vertex hex right)
			   divisor))
	 (difference (- right-ele left-ele))
	 (contours
	   (make-contours
	    :left left-ele
	    :range difference
	    :water-left (vertex-underwater hex left)
	    :water-right (vertex-underwater hex right)
	    :deque (make-range-deque))))
    (cond ((> difference 0)
	   (loop for elevation from (1+ left-ele) to right-ele
		 do (push-right elevation (contours-deque contours))))
	  ((< difference 0)
	   (loop for elevation from left-ele downto (1+ right-ele)
		 do (push-right elevation (contours-deque contours)))))
    contours))

;; test:
'(defun record (left right)
  (let* ((left-ele (round left
			  1))
	 (right-ele (round right
			   1))
	 (difference (- right-ele left-ele))
	 (contours
	   (make-contours
	    :left left-ele
	    :range difference
	    :deque (make-linkage))))
    (cond ((> difference 0)
	   (loop for elevation from (1+ left-ele) to right-ele
		 do (push-right elevation (contours-deque contours))))
	  ((< difference 0)
	   (loop for elevation from left-ele downto (1+ right-ele)
		 do (push-right elevation (contours-deque contours)))))
    contours))

(defun contour-index (elevation contours)
  (let ((index
	  (if (>= (contours-range contours) 0)
	      (- elevation (contours-left contours) 1)
	      (abs (- elevation (contours-left contours))))))
    (if (and (<= 0 index)
	     (< index (abs (contours-range contours))))
	index)))

(defun is-contour-of (elevation contours)
  (or (= elevation
	 (peek-left (contours-deque contours)))
      (= elevation
	 (peek-right (contours-deque contours)))))

'(defun is-contour-of (elevation contours)
  (or (= elevation
	(link-this (linkage-leftmost (contours-deque contours))))
      (= elevation
	 (link-this (linkage-rightmost (contours-deque contours))))))

#|  (funcall (if (>= (contours-range contours) 0)
	       #'< #'>=)
	   (contours-left contours)
	   elevation
	   (+ 1
	      (contours-left contours)
(contours-range contours))))|#

(defvar *vertex-underwater* (make-hash-table :test 'equal))
;; Each underwater vert should have an integer water surface altitude
(defun vertex-underwater (hex dir)
  (or (gethash (list hex dir) *vertex-underwater*)
      0))

(defun sink (crd dir depth)
  (let ((hex (hex-at crd *world*)))
    (setf (gethash (list hex dir) *vertex-underwater*)
	  (+ (hex-vertex hex dir)
	     depth))))

(defun sinktest ()
  ;;Note to self: I've got 3 verts per edge, MORON!
  (sink (crd 55 26) :e 0)
  (sink (crd 55 26) :ne 0)
  (sink (crd 55 26) :se 1)
  (sink (crd 55 26) :ssw 0)
  (sink (crd 55 26) :s 1)
  (sink (crd 55 26) :sse 1)
  
  (sink (crd 54 26) :e 0)
  (sink (crd 54 26) :se 1)
  (sink (crd 54 26) :sse 0)
  
  (sink (crd 55 25) :cen 1)
  (sink (crd 55 25) :sw 0)
  (sink (crd 55 25) :w 0)
  (sink (crd 55 25) :nw 1)
  (sink (crd 55 25) :nnw 0)
  (sink (crd 55 25) :n 1)
  (sink (crd 55 25) :nne 1)
  (sink (crd 55 25) :ne 0)
  (sink (crd 55 25) :e 0)
  (sink (crd 55 25) :se 0)
  (sink (crd 55 25) :sse 0)
  (sink (crd 55 25) :s 1)
  (sink (crd 55 25) :ssw 1)

  (mapcar #'(lambda (dir dep)
	      (sink (crd 54 25) dir dep))
	  '(:ne :ne :e :se :sse :s)
	  '(0 0 0 0 0 0))

  (mapcar #'(lambda (dir dep)
	      (sink (crd 56 27) dir dep))
	  '(:sw :ssw :s :sse :se :e)
	  '(0 0 0 0 1 0)))

(defmacro probe-contours ((at-left at-right) var
			  &body body)
  `(when (eql (the (or fixnum null) (peek-left (contours-deque ,at-right)))
	      (the (or fixnum null) (peek-right (contours-deque ,at-left))))
     (do ((,var (the (or fixnum null) (peek-left (contours-deque ,at-right)))
		(the (or fixnum null) (peek-left (contours-deque ,at-right)))))
	 ((or
	   (null (peek-left (contours-deque ,at-right)))
	   (null (peek-right (contours-deque ,at-left)))
	   ))
       (progn (pop-left (contours-deque ,at-right))
	      (pop-right (contours-deque ,at-left)))
       ;; Example thickness change:
       (if (= (mod ,var 10) 0)
	   (cairo:set-line-width 1.0)
	   (cairo:set-line-width 0.5))
       (if (<= ,var (1+ (max (contours-water-left ,at-right) ;; seems excessive
			     (contours-water-right ,at-left)
			     (contours-water-left ,at-left)
			     (contours-water-right ,at-right))))
	   (cairo:set-source-rgb 0.2 0.2 1.0)
	   (cairo:set-source-rgb 0.5 0.5 0.5))
       ,@body
       (cairo:stroke)
       )))

(defun hex-at (crd world)
  (gethash crd (world-map world)))

(defun (setf hex-at) (new-hex crd world)
  (setf (gethash crd (world-map world)) new-hex))

(defun hex-edge (hex direction)
  (declare (type (or hex null) hex)
	   (type direction direction))
  (when hex
    (ecase direction
      (:N (hex-N-edge hex))
      (:NW (hex-NW-edge hex))
      (:SW (hex-SW-edge hex))
      (:S (hex-S-edge hex))
      (:SE (hex-SE-edge hex))
      (:NE (hex-NE-edge hex)))))

(defun (setf hex-edge) (hex direction new-edge)
  (declare (type hex hex)
	   (type direction direction)
	   (type edge new-edge))
  (ecase direction
    (:N (setf (hex-N-edge hex) new-edge))
    (:NW (setf (hex-NW-edge hex) new-edge))
    (:SW (setf (hex-SW-edge hex) new-edge))
    (:S (setf (hex-S-edge hex) new-edge))
    (:SE (setf (hex-SE-edge hex) new-edge))
    (:NE (setf (hex-NE-edge hex) new-edge))))

(defun hex-vertex (hex vert-direction)
  (declare ;(type hex hex)
   (type hex-vertex vert-direction))
  (when (not hex)
    (return-from hex-vertex))
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

(defun (setf hex-vertex) (hex vert-direction new-vert-value)
  (declare (type hex hex)
	   (type hex-vertex vert-direction)
	   (type elevation new-vert-value))
  (ccase vert-direction
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
