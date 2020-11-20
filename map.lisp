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

(defun set-crd (crd hex world)
  (setf (gethash crd (world-map world)) hex)
  (loop for direction in +primary-directions+
	for neighbour = (crd-hex (crd-neighbour crd direction)
				 world)
	when neighbour
	  do (setf (hex-edge hex direction)
		   (hex-edge neighbour (opposite direction)))))
	



