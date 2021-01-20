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
  (n (point 0 0) :type point))

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

(defstruct contours
  (left 0 :type (signed-byte 8))
  (range 0 :type (signed-byte 8))
  (water 0 :type (signed-byte 8))
  ;(water-left 0 :type (signed-byte 8))
  ;(water-right 0 :type (signed-byte 8))
  (deque nil :type range-deque))

(defun contours-water-left (c)
  (contours-water c))
(defun contours-water-right (c)
  (contours-water c))

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
	    :water (point-water left-point)
	    ;:water-right (point-water right-point)
	    :deque (make-range-deque))))
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

;; Each underwater vert should have an integer water surface altitude
(defun vertex-underwater (hex dir)
  (let ((point (hex-vertex hex dir)))
    (>= (point-water point) (point-elevation point))))

(defvar *point-connections*
  (make-hash-table :test 'eq :size 13))

(defmacro defpointcon (pointdir (&rest connections))
  `(setf (gethash ,pointdir *point-connections*) ',connections))

(defun point-connections (crd dir)
  (mapcar #'(lambda (pre-dir)
	      (etypecase pre-dir
		(keyword (list crd pre-dir))
		(list (list (crd-neighbour crd (first pre-dir))
			    (second pre-dir)))))
	  (gethash dir *point-connections*)))

;;          dir/dirs at this crd/go here, then this dir
(defpointcon :N (:NNW :CEN :NNE (:N :CEN)))
(defpointcon :NNE (:N :NE :cen (:N :SE)))
(defpointcon :NE (:NNE :CEN :E (:NE :CEN)))
(defpointcon :E (:NE :SE :cen (:NE :S)))
(defpointcon :SE (:E :CEN :SSE (:SE :CEN)))
(defpointcon :SSE (:SE :S :cen (:SE :SW)))
(defpointcon :S (:SSE :CEN :SSW (:S :CEN)))
(defpointcon :SSW (:S :SW :cen (:S :NW)))
(defpointcon :SW (:SSW :CEN :W (:SW :CEN)))
(defpointcon :W (:SW :NW :cen (:SW :N)))
(defpointcon :NW (:W :CEN :NNW (:NW :CEN)))
(defpointcon :NNW (:NW :N :cen (:NW :NE)))
(defpointcon :CEN (:N :NE :SE :S :SW :NW :nnw :nne :e :sse :ssw :w))

(defun flood-fill (water-level world crd dir)
  (declare (fixnum water-level)
	   (optimize (speed 3)
		     (compilation-speed 0)))
  (let ((current (vertex-exists crd dir world)))
    (when (and (< (point-water current) water-level)
	       (<= (point-elevation current) water-level))
      (setf (point-water current) water-level)
      (dolist (con
	       (remove-if
		#'(lambda (crd-dir)
		    (not (vertex-exists (first crd-dir)
					(second crd-dir)
					*world*)))
		(point-connections crd dir)))
	;; This WILL blow the stack:
	(apply #'flood-fill water-level world con))
      NIL)))

(defun sinktest ()
  (flood-fill 9 *world* (crd 49 13) :cen))

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

(defun hex-vertex (hex vert-direction)
  (declare ;(type hex hex)
   (type hex-vertex vert-direction))
  (when (not hex)
    (return-from hex-vertex))
  (ecase vert-direction
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
    (:NW (hex-NW hex))))

(defun vertex-exists (crd dir world)
  (declare (crd crd)
	   (hex-vertex dir))
  (or (hex-vertex (hex-at crd *world*) dir)
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
			      :E))))))

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
