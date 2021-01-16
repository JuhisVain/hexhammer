(in-package :hexhammer)

(deftype pointspec ()
  `(cons crd (cons (member ,@+vertex-directions+) null)))
(defstruct (point (:constructor point (elevation &optional (water 0))))
  (elevation 0 :type elevation)
  (water 0 :type elevation))

(defstruct (hex (:constructor
		    make-hex (cen &optional nne ne e se sse s ssw sw w nw nnw n)))
  
  (cen (point 0 0) :type point)
  (nne (point 0 0) :type point)
  (ne (point 0 0) :type point)
  (e (point 0 0) :type point)
  (se (point 0 0) :type point)
  (sse (point 0 0) :type point)
  (s (point 0 0) :type point)
  (ssw (point 0 0) :type point)
  (sw (point 0 0) :type point)
  (w (point 0 0) :type point)
  (nw (point 0 0) :type point)
  (nnw (point 0 0) :type point)
  (n (point 0 0) :type point)
  #|
  (N-edge NIL :type (or edge null))
  (NW-edge NIL :type (or edge null))
  (SW-edge NIL :type (or edge null))
  (S-edge NIL :type (or edge null))
  (SE-edge NIL :type (or edge null))
  (NE-edge NIL :type (or edge null))
  |#
  )

(defun chomp-append (&rest lists)
  (labels ((copy (count list)
	     (when (> count 0)
	       (cons (first list)
		     (copy (1- count) (cdr list))))))
    (mapcan #'(lambda (x) (copy 2 x))
	    lists)))

(defun make-hex-by-edge
    (elevation &key ne-edge se-edge s-edge sw-edge nw-edge n-edge)
  (apply #'make-hex
	 elevation
	 (chomp-append ne-edge se-edge s-edge
		       sw-edge nw-edge n-edge)))

#|
(defstruct edge
  (west 0 :type elevation)
  (middle 0 :type elevation)
  (east 0 :type elevation))
|#
(defstruct contours
  (left 0 :type (signed-byte 8))
  (range 0 :type (signed-byte 8))
  (water-left 0 :type (signed-byte 8))
  (water-right 0 :type (signed-byte 8))
  (deque nil :type range-deque))

(defun record-contours (hex left right divisor)
  (let* ((left-point (hex-vertex hex left))
	 (right-point (hex-vertex hex right))
	 (left-ele (round (point-elevation left-point)
			  divisor))
	 (right-ele (round (point-elevation right-point)
			   divisor))
	 (difference (- right-ele left-ele))
	 (contours
	   (make-contours
	    :left left-ele
	    :range difference
	    :water-left (point-water left-point)
	    :water-right (point-water right-point)
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
  (let ((point (hex-vertex hex dir)))
    (>= (point-water point) (point-elevation point))))
;(or (gethash (list hex dir) *vertex-underwater*)
 ;   0))

(defun sink (crd dir depth)
  (let ((hex (hex-at crd *world*)))
    (setf (gethash (list hex dir) *vertex-underwater*)
	  (+ (hex-vertex hex dir)
	     depth))))

'(defun flood-fill ()
  );; TODO: better rewrite verts to box everything

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

#|
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
|#

(defun hex-vertex (hex vert-direction)
  (declare ;(type hex hex)
   (type hex-vertex vert-direction))
  (when (not hex)
    (return-from hex-vertex))
  (ecase vert-direction
    #|
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
    (:NW (edge-middle (hex-NW-edge hex)))
|#
    (:CEN (hex-CEN hex))
    (:NNW (hex-NNW hex))
    (:N (hex-N hex))
    (:NNE (hex-NNE hex))
    (:NE (hex-NE hex))
    (:E (hex-E hex))
    (:SE (hex-SE hex))
    (:SSE (hex-SSE hex))
    (:S (hex-S hex))
    (:SSW (hex-SSW hex))
    (:SW (hex-SW hex))
    (:W (hex-W hex))
    (:NW (hex-NW hex))
    ))
#|
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
|#

(defun vertex-exists (crd dir world)
  (declare (crd crd)
	   (hex-vertex dir))
  (case dir
    ((:N :NE :SE :S :SW :NW)
     (hex-vertex (hex-at (crd-neighbour crd dir)
			 world)
		 (opposite dir)))
    (:NNE (or (hex-vertex (hex-at (crd-neighbour crd :N)
				  world)
			  :SSE)
	      (hex-vertex (hex-at (crd-neighbour crd :NE)
				  world)
			  :W)))
    (:E (or (hex-vertex (hex-at (crd-neighbour crd :NE)
				world)
			:SSW)
	    (hex-vertex (hex-at (crd-neighbour crd :SE)
				world)
			:NNW)))
    (:SSE (or (hex-vertex (hex-at (crd-neighbour crd :SE)
				  world)
			  :W)
	      (hex-vertex (hex-at (crd-neighbour crd :S)
				  world)
			  :NNE)))
    (:SSW (or (hex-vertex (hex-at (crd-neighbour crd :S)
				  world)
			  :NNW)
	      (hex-vertex (hex-at (crd-neighbour crd :SW)
				  world)
			  :E)))
    (:W (or (hex-vertex (hex-at (crd-neighbour crd :SW)
				world)
			:NNE)
	    (hex-vertex (hex-at (crd-neighbour crd :NW)
				world)
			:SSE)))
    (:NNW (or (hex-vertex (hex-at (crd-neighbour crd :N)
				  world)
			  :SSW)
	      (hex-vertex (hex-at (crd-neighbour crd :NW)
				  world)
			  :E)))))

(defun adjacent-vertex-directions (dir)
  "From absolute westmost to absolute eastmost"
  (declare (hex-vertex dir))
  (case dir
    (:N '(:NNW :N :NNE))
    (:NE '(:NNE :NE :E))
    (:SE '(:SSE :SE :E))
    (:S '(:SSW :S :SSE))
    (:SW '(:W :SW :SSW))
    (:NW '(:W :NW :NNW))))

(defun edge-verts (crd dir world)
  "Returns list of points or nils from west to east of edge DIR of hex at CRD"
  (declare (crd crd)
	   (direction dir))
  (mapcar #'(lambda (vert-dir)
	      (vertex-exists crd vert-dir world))
	  (adjacent-vertex-directions dir)))
