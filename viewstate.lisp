(in-package :hexhammer)

(defclass view-state ()
  ((width :initform 1000
	  :initarg :width
	  :accessor width)
   (height :initform 800
	   :initarg :height
	   :accessor height)
   (centre-x :initform (* 4 75.0)
	     :initarg :centre-x
	     :accessor centre-x)
   (centre-y :initform (* 6 +sin60+ 75.0)
	     :initarg :centre-y
	     :accessor centre-y)
   (texture :initarg :texture
	    :accessor texture)
   (buffer :initarg :buffer
	   :accessor buffer)
   (hex-r :initform 75.0 ; ZOOM
	  :accessor hex-r)))

(defmethod (setf hex-r) :around (new-r view-state)
  (let ((old-r (hex-r view-state)))
    (call-next-method)
    (with-slots (centre-x centre-y hex-r) view-state
      (setf centre-x (* (/ centre-x old-r) hex-r)
	    centre-y (* (/ centre-y old-r) hex-r)))))

(defmethod (setf hex-r) :after (new-r view-state)
  (when (<= (hex-r view-state) 0)
    (setf (slot-value view-state 'hex-r) 1.0)))

(defun hex-x-at-pix (pixel view-state)
  "Returns floating point number as X coordinate of hex below this x PIXEL."
  (/ (+ pixel
	(- (centre-x view-state)
	   (/ (width view-state) 2)))
     (* 1.5 (hex-r view-state))))

(defun hex-y-at-pix (pixel view-state)
  "Returns floating point number as Y coordinate of hex below this y PIXEL
when X coordinate is divisible by 2. If X is odd, reduce 0.5."
  (+ -0.5
   (/ (- (height view-state)
	 (+ pixel
	    (- (/ (height view-state) 2)
	       (centre-y view-state))))
      (* +sin60+ 2 (hex-r view-state)))))

(defun hex-xy-vert-at-pix (pixel-x pixel-y view-state)
  "Returns (cons coordinate closest-vertex) of hex
when screen clicked at (pixel-x,pixel-y)."
  (let* ((x-crd (hex-x-at-pix pixel-x view-state))
	 (y-crd (+ (hex-y-at-pix pixel-y view-state)
		   (if (zerop (rem (floor x-crd) 2))
		       0 -0.5)))
	 (internal-x (mod x-crd 1)) ; 0.0 at W, 1.0 at NNE/SSE
	 (internal-y (mod y-crd 1)) ; 0.0 at S, 1.0 at N
	 ;;;(v1.x - v0.x)*(v2.y - v0.y) - (v2.x - v0.x)*(v1.y - v0.y)
	 ;;  => Will return positive if v2 to the left of line (v0-v1)
	 (left-of-nw-edge (- (* (float 1/3)
				(- internal-y 0.5))
			     (* internal-x
				0.5)))
	 (left-of-sw-edge (- (* (- (float 1/3))
				internal-y)
			     (* (- internal-x
				   (float 1/3))
				0.5)))
	 (preliminary-x (floor x-crd))
	 (x-div (floor (/ internal-x 1/6)))
	 (y-div (floor (/ internal-y 1/6))))
    (cond ((> left-of-nw-edge 0)
	   (cons
	    (crd (1- preliminary-x)
		 (+ (floor y-crd)
		    (if (evenp preliminary-x)
			0 1)))
	    (gethash (list x-div y-div)
		     (sera:dictq (0 3) :sse
				 (1 3) :sse
				 (0 4) :se
				 (1 4) :se
				 (0 5) :e
				 (1 5) :e))))
	  ((> left-of-sw-edge 0)
	   (cons
	    (crd (1- preliminary-x)
		 (+ (floor y-crd)
		    (if (evenp preliminary-x)
			-1 0)))
	    (gethash (list x-div y-div)
		     (sera:dictq (0 0) :e
				 (1 0) :e
				 (0 1) :ne
				 (1 1) :ne
				 (0 2) :nne
				 (1 2) :nne))))
	  (t
	   (cons (crd preliminary-x
		      (floor y-crd))
		 (gethash (list x-div y-div)
			  (sera:dictq (1 0) :ssw
				      (2 0) :ssw
				      (0 1) :sw
				      (1 1) :sw
				      (0 2) :w
				      (1 2) :w
				      (0 3) :w
				      (1 3) :w
				      (0 4) :nw
				      (1 4) :nw
				      (1 5) :nnw
				      (2 5) :nnw
				      (3 5) :n
				      (4 5) :n
				      (5 5) :nne
				      (3 0) :s
				      (4 0) :s
				      (5 0) :sse
				      (3 2) :cen
				      (3 3) :cen
				      (4 2) :cen
				      (4 3) :cen)))))))

(defun hex-xy-at-pix (pixel-x pixel-y view-state)
  "Returns coordinate of hex when screen clicked at (pixel-x,pixel-y)."
  (let* ((x-crd (hex-x-at-pix pixel-x view-state))
	 (y-crd (+ (hex-y-at-pix pixel-y view-state)
		   (if (zerop (rem (floor x-crd) 2))
		       0 -0.5)))
	 (internal-x (mod x-crd 1)) ; 0.0 at W, 1.0 at NNE/SSE
	 (internal-y (mod y-crd 1)) ; 0.0 at S, 1.0 at N
	 ;;;(v1.x - v0.x)*(v2.y - v0.y) - (v2.x - v0.x)*(v1.y - v0.y)
	 ;;  => Will return positive if v2 to the left of line (v0-v1)
	 (left-of-nw-edge (- (* (float 1/3)
				(- internal-y 0.5))
			     (* internal-x
				0.5)))
	 (left-of-sw-edge (- (* (- (float 1/3))
				internal-y)
			     (* (- internal-x
				   (float 1/3))
				0.5)))
	 (preliminary-x (floor x-crd)))
    (cond ((> left-of-nw-edge 0)
	   (crd (1- preliminary-x)
		(+ (floor y-crd)
		   (if (evenp preliminary-x)
		       0 1))))
	  ((> left-of-sw-edge 0)
	   (crd (1- preliminary-x)
		(+ (floor y-crd)
		   (if (evenp preliminary-x)
		       -1 0))))
	  (t
	   (crd preliminary-x
		(floor y-crd))))))

(defmacro do-visible ((x-var y-var view-state) &body body)
  "Iterates through currently visible hexses' coordinates."
  `(loop for ,x-var
	 from (1- (floor (hex-x-at-pix 0 ,view-state)))
	   to (ceiling (hex-x-at-pix (width ,view-state) ,view-state))
	 do (loop for ,y-var
		  from (1- (floor (hex-y-at-pix (height ,view-state) ,view-state)))
		    to (ceiling (hex-y-at-pix 0 ,view-state))
		  do ,@body)))
	 
