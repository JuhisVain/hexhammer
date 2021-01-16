(in-package :hexhammer)

(defstruct world
  (map (make-hash-table :test 'equalp))
  )

(defconstant +primary-directions+
  (list :N :NW :SW :S :SE :NE))

(defconstant +vertex-directions+
  (append (list :NNE :NNW :W :SSW :SSE :E :CEN)
	  +primary-directions+))

(deftype direction () (cons 'member +primary-directions+))
(deftype hex-vertex () (cons 'member +vertex-directions+))
(deftype elevation () '(signed-byte 32))
;;(deftype crd () '(cons fixnum fixnum))

(declaim (inline crd))
(defstruct (crd (:conc-name nil)
		(:constructor crd (x y)))
  (x nil :type (or single-float fixnum))
  (y nil :type (or single-float fixnum)))

#|(setf (fdefinition 'crd) #'cons
      (fdefinition 'x) #'car
      (fdefinition 'y) #'cdr
      (fdefinition 'copy-crd) #'copy-list)

(defsetf x rplaca)
(defsetf y rplacd)
|#
#|
(defun crd-hex (crd world)
  (declare (type crd crd))
  (hex-at crd world))

(defun crd-edge (crd direction world)
  (declare (type crd crd)
	   (type direction direction))
  (hex-edge (hex-at crd world)
	    direction))

(defun crd-vertex (crd direction world)
  (declare (type crd crd)
	   (type hex-vertex direction))
  (hex-vertex (hex-at crd world) direction))
|#
(defun opposite (direction)
  (declare (type hex-vertex direction))
  (gethash direction
	   (sera:dictq :N :S
		       :NNW :SSE
		       :NW :SE
		       :W :E
		       :SW :NE
		       :SSW :NNE
		       :NNE :SSW
		       :NE :SW
		       :E :W
		       :SE :NW
		       :SSE :NNW
		       :S :N)))

(defun crd-neighbour (crd direction)
  (declare (type crd crd)
	   (type direction direction))
  (symbol-macrolet ((xodd+ (mod x 2))
		    (xodd- (1- xodd+)))
    
    (let ((x (x crd))
	  (y (y crd)))
      (the (values crd &optional)
	   (ecase direction
	     (:N (crd x (1+ y)))
	     (:NE (crd (1+ x) (+ y xodd+)))
	     (:SE (crd (1+ x) (+ y xodd-)))
	     (:S (crd x (1- y)))
	     (:SW (crd (1- x) (+ y xodd-)))
	     (:NW (crd (1- x) (+ y xodd+))))))))

(defun translate-data (x y direction width height data)
  (round
   (2faref data
	   (+ (* (xofs direction)
		 (/ 1.0 width))
	      (/ x width))
	   (+ (* (yofs direction)
		 (/ 1.0 height))
	      (/ y height)))
   2))

(defun filter-read-edge (x y direction width height data world)
  (mapcar #'(lambda (point dir)
	      (if point point
		  (point (translate-data x y dir width height data))))
	  (edge-verts (crd x y) direction world)
	  (adjacent-vertex-directions direction)))

(defun generate-map (width height filename world)
  (let ((source-data (read-pgm filename)))
    (dotimes (y height)
      (dotimes (x width)
	(setf
	 (hex-at (crd x y) world)
	 (let* ((hex (make-hex-by-edge ;; Need to reverse souths to force them clockwise
		      (point (translate-data x y :CEN width height source-data))
		      :ne-edge (filter-read-edge x y :NE width height source-data world)
		      :se-edge (reverse (filter-read-edge x y :SE width height source-data world))
		      :s-edge (reverse (filter-read-edge x y :S width height source-data world))
		      :sw-edge (reverse(filter-read-edge x y :SW width height source-data world))
		      :nw-edge (filter-read-edge x y :NW width height source-data world)
		      :n-edge (filter-read-edge x y :N width height source-data world)
		      #|
		      :n-edge (or (hex-edge (hex-at
		      (crd-neighbour
		      (crd x y) :N)
		      world)
		      :S)
		      (make-edge
		      :west (or ; this is a mess
		      (hex-vertex (hex-at
		      (crd-neighbour
		      (crd x y) :nw)
		      world)
		      :e)
		      nnw)
		      :middle (read-data :N)
		      :east nne))
		      :nw-edge (or (hex-edge (hex-at
		      (crd-neighbour
		      (crd x y) :NW)
		      world)
		      :SE)
		      (make-edge
		      :west (or
		      (hex-vertex (hex-at
		      (crd-neighbour
		      (crd x y) :sw)
		      world)
		      :nne)
		      w)
		      :middle (read-data :NW)
		      :east (or
		      (hex-vertex (hex-at
		      (crd-neighbour
		      (crd x y) :n)
		      world)
		      :ssw)
		      nnw)))
		      :sw-edge (or (hex-edge (hex-at
		      (crd-neighbour
		      (crd x y) :SW)
		      world)
		      :NE)
		      (make-edge
		      :west (or
		      (hex-vertex (hex-at
		      (crd-neighbour
		      (crd x y) :nw)
		      world)
		      :sse)
		      w)
		      :middle (read-data :SW)
		      :east (or
		      (hex-vertex (hex-at
		      (crd-neighbour
		      (crd x y) :s)
		      world)
		      :nnw)
		      ssw)))
		      :s-edge (or (hex-edge (hex-at
		      (crd-neighbour
		      (crd x y) :S)
		      world)
		      :N)
		      (make-edge
		      :west (or
		      (hex-vertex (hex-at
		      (crd-neighbour
		      (crd x y) :sw)
		      world)
		      :e)
		      ssw)
		      :middle (read-data :S)
		      :east (or
		      (hex-vertex (hex-at
		      (crd-neighbour
		      (crd x y) :se)
		      world)
		      :w)
		      sse)))
		      :se-edge (or (hex-edge (hex-at
		      (crd-neighbour
		      (crd x y) :SE)
		      world)
		      :NW)
		      (make-edge
		      :west (or
		      (hex-vertex (hex-at
		      (crd-neighbour
		      (crd x y) :s)
		      world)
		      :nne)
		      sse)
		      :middle (read-data :SE)
		      :east e))
		      :ne-edge (or (hex-edge (hex-at
		      (crd-neighbour
		      (crd x y) :NE)
		      world)
		      :SW)
		      (make-edge
		      :west nne
		      :middle (read-data :NE)
		      :east (or
		      (hex-vertex (hex-at
		      (crd-neighbour
		      (crd x y) :se)
		      world)
		      :nnw)
		      e)))
		      |#
		      )))
	   hex))))))

(defun set-crd (crd hex world)
  (setf (hex-at crd world) hex)
  (loop for direction in +primary-directions+
	for neighbour = (hex-at (crd-neighbour crd direction)
				 world)
	when neighbour
	  do (setf (hex-edge hex direction)
		   (hex-edge neighbour (opposite direction)))))
	



