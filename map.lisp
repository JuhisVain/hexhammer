(in-package :hexhammer)

(defstruct world
  (map (make-hash-table :test 'equalp))
  (features (make-hash-table :test 'equalp))
  )

(defconstant +primary-directions+
  (list :N :NW :SW :S :SE :NE))

(defconstant +vertex-directions+
  (append (list :NNE :NNW :W :SSW :SSE :E :CEN)
	  +primary-directions+))

(deftype direction () (cons 'member +primary-directions+))
(deftype hex-vertex () (cons 'member +vertex-directions+))
(deftype elevation () '(signed-byte 32))

(declaim (inline crd))
(defstruct (crd (:conc-name nil)
		(:constructor crd (x y)))
  (x nil :type (or single-float fixnum))
  (y nil :type (or single-float fixnum)))

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

(defun vertex-alias (crd vertex alias-crd)
  "Returns the vertex name of CRD's VERTEX in ALIAS-CRD."
  (macrolet
      ((is-there-then ((there-0 then-0) (there-1 then-1))
	 `(or (and (equalp alias-crd (crd-neighbour crd ,there-0))
		   ,then-0)
	      (and (equalp alias-crd (crd-neighbour crd ,there-1))
		   ,then-1)
	      (error "Alias-crd ~a is not ~a or ~a from ~a vertex ~a~%"
		     alias-crd ,there-0 ,there-1 crd vertex))))

    (case vertex
      ((:N :NW :SW :S :SE :NE)
       (if (equalp (crd-neighbour crd vertex)
		   alias-crd)
	   (opposite vertex)
	   (error "Alias-crd ~a is not ~a from ~a~%"
		  alias-crd vertex crd)))
      (:NNE (is-there-then (:N :SSE) (:NE :W)))
      (:E (is-there-then (:NE :SSW) (:SE :NNW)))
      (:SSE (is-there-then (:SE :W) (:S :NNE)))
      (:SSW (is-there-then (:S :NNW) (:SW :E)))
      (:W (is-there-then (:SW :NNE) (:NW :SSE)))
      (:NNW (is-there-then (:NW :E) (:N :SSW)))
      (t (error "~a is not a valid vertex name!~%" vertex)))))

(defun translate-data (x y direction width height data &optional (divisor 1))
  (round
   (2faref data
	   (+ (* (xofs direction)
		 (/ 1.0 width))
	      (/ x width))
	   (+ (* (yofs direction)
		 (/ 1.0 height))
	      (/ y height)))
   divisor))

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
	 (make-hex-by-edge ;; Need to reverse souths to force them clockwise
	  (point (translate-data x y :CEN width height source-data))
	  :ne-edge (filter-read-edge x y :NE width height source-data world)
	  :se-edge (reverse (filter-read-edge x y :SE width height source-data world))
	  :s-edge (reverse (filter-read-edge x y :S width height source-data world))
	  :sw-edge (reverse(filter-read-edge x y :SW width height source-data world))
	  :nw-edge (filter-read-edge x y :NW width height source-data world)
	  :n-edge (filter-read-edge x y :N width height source-data world)))))))

(defun set-crd (crd hex world)
  (setf (hex-at crd world) hex)
  (loop for direction in +primary-directions+
	for neighbour = (hex-at (crd-neighbour crd direction)
				 world)
	when neighbour
	  do (setf (hex-edge hex direction)
		   (hex-edge neighbour (opposite direction)))))
	

(defparameter *test-world-stack* nil)

(defun pop-test-world ()
  (setf *world* (pop *test-world-stack*)))

(defun push-test-world (origin)
  (push *world* *test-world-stack*)
  (setf *world* nil)
  (typecase origin
    (hex
     (setf *world* (make-world))
     (setf (hex-at (crd 0 0) *world*) origin)))
  (when (null *world*)
    (pop-test-world)))
