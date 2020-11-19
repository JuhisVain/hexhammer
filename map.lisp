(in-package :hexhammer)

(defstruct world
  (map (make-hash-table :test 'equal))
  )

;; Don't touch:
(defconstant +primary-directions+
  (list :N :NW :SW :S :SE :NE))

(defconstant +vertex-directions+
  (append (list :NNE :NNW :W :SSW :SSE :E :CEN)
	  +primary-directions+))

(deftype direction () (cons 'member +primary-directions+))
(deftype hex-vertex () (cons 'member +vertex-directions+))
(deftype elevation () '(signed-byte 32))
(deftype crd () '(cons (unsigned-byte 32) (unsigned-byte 32)))

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



(defun opposite (direction)
  (declare (type hex-vertex direction))
  (ecase direction ;;;TODO: (dictq
    (:N :S)
    ()))

(defun crd-neighbour (crd direction)
  (declare (type crd crd)
	   (type direction direction))
  (macrolet ((xodd+ () '(rem x 2))
	     (xodd- () '(1- (xodd+))))
    
    (let ((x (x crd))
	  (y (y crd)))

      (the (values crd &optional)
	   (ecase direction
	     (:N (crd x (1+ y)))
	     (:NE (crd (1+ x) (+ y (xodd+))))
	     (:SE (crd (1+ x) (+ y (xodd-))))
	     (:S (crd x (1- y)))
	     (:SW (crd (1- x) (+ y (xodd-))))
	     (:NW (crd (1- x) (+ y (xodd+)))))))))

#|
(defun set-crd (crd hex world)
  (setf (gethash crd (world-map world)) hex)
  (do ((direction (mapcan #'list ;;counter clockwise starting from NNE
			  +vertex-directions+
			  +primary-directions+)
		  (cdr direction))
       (neighbour +primary-directions+ (cdr neighbour)))
    (setf (hex-vertex hex direction)
	  ))

|#
