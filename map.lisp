(in-package :hexhammer)

(defstruct world
  (map (make-hash-table :test 'equal))
  )

(defconstant +primary-directions+
  (list :N :NW :SW :S :SE :NE))

(defconstant +vertex-directions+
  (append (list :NNE :NNW :W :SSW :SSE :E :CEN)
	  +primary-directions+))

(deftype direction () (cons 'member +primary-directions+))
(deftype hex-vertex () (cons 'member +vertex-directions+))
(deftype elevation () '(signed-byte 32))
(deftype crd () '(cons fixnum fixnum))

(setf (fdefinition 'crd) #'cons
      (fdefinition 'x) #'car
      (fdefinition 'y) #'cdr)

(defun crd-hex (crd world)
  (declare (type crd crd))
  (gethash crd (world-map world)))

(defun crd-edge (crd direction world)
  (declare (type crd crd)
	   (type direction direction))
  (hex-edge (gethash crd (world-map world))
	    direction))

(defun crd-vertex (crd direction world)
  (declare (type crd crd)
	   (type hex-vertex direction))
  (hex-vertex (crd-hex crd world) direction))

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

(defun generate-map (width height world)
  (let* ((source-data (read-pgm "mhmap.pgm"))
	 (float-hex-width (float (/ width))) ; maybe 1+ ?
	 (float-hex-height (float (/ height))))
    (macrolet ((read-data (direction)
		 `(rem ; make elevation values 0 - 10
		   (round (2faref source-data
				  (+ (* (xofs ,direction)
					float-hex-width)
				     (/ x width))
				  (+ (* (yofs ,direction)
					float-hex-height)
				     (/ y height))))
		   10)))
      (dotimes (y height)
	(dotimes (x width)
	  (setf (gethash (crd x y) (world-map world))
		(make-hex
		 :elevation (read-data :CEN)
		 :n-edge (or (hex-edge (crd-hex
					  (crd-neighbour
					   (crd x y) :N)
					  world)
				       :S)
			     (make-edge
			      :west (read-data :NNW)
			      :middle (read-data :N)
			      :east (read-data :NNE)))
		 :nw-edge (or (hex-edge (crd-hex
					    (crd-neighbour
					     (crd x y) :NW)
					    world)
					:SE)
			      (make-edge
			       :west (read-data :W)
			       :middle (read-data :NW)
			       :east (read-data :NNW)))
		 :sw-edge (or (hex-edge (crd-hex
					    (crd-neighbour
					     (crd x y) :SW)
					    world)
					:NE)
			      (make-edge
			       :west (read-data :W)
			       :middle (read-data :SW)
			       :east (read-data :SSW)))
		 :s-edge (or (hex-edge (crd-hex
					  (crd-neighbour
					   (crd x y) :S)
					  world)
				       :N)
			     (make-edge
			      :west (read-data :SSW)
			      :middle (read-data :S)
			      :east (read-data :SSE)))
		 :se-edge (or (hex-edge (crd-hex
					    (crd-neighbour
					     (crd x y) :SE)
					    world)
					:NW)
			      (make-edge
			       :west (read-data :SSE)
			       :middle (read-data :SE)
			       :east (read-data :E)))
		 :ne-edge (or (hex-edge (crd-hex
					    (crd-neighbour
					     (crd x y) :NE)
					    world)
					:SW)
			      (make-edge
			       :west (read-data :NNE)
			       :middle (read-data :NE)
			       :east (read-data :E))))))))))

(defun set-crd (crd hex world)
  (setf (gethash crd (world-map world)) hex)
  (loop for direction in +primary-directions+
	for neighbour = (crd-hex (crd-neighbour crd direction)
				 world)
	when neighbour
	  do (setf (hex-edge hex direction)
		   (hex-edge neighbour (opposite direction)))))
	



