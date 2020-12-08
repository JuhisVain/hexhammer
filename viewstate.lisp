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


(defmacro do-visible ((x-var y-var view-state) &body body)
  "Iterates through currently visible hexses' coordinates."
  `(loop for ,x-var
	 from (1- (floor (hex-x-at-pix 0 ,view-state)))
	   to (ceiling (hex-x-at-pix (width ,view-state) ,view-state))
	 do (loop for ,y-var
		  from (1- (floor (hex-y-at-pix (height ,view-state) ,view-state)))
		    to (ceiling (hex-y-at-pix 0 ,view-state))
		  do ,@body)))
	 
