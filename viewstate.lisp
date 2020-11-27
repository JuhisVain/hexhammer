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
      (format t "setf hex-r around: ~a ~a ~a~%" centre-x centre-y hex-r)
      (setf centre-x (* (/ centre-x old-r) hex-r)
	    centre-y (* (/ centre-y old-r) hex-r)))))

(defmethod (setf hex-r) :after (new-r view-state)
  (when (<= (hex-r view-state) 0)
    (setf (slot-value view-state 'hex-r) 1.0)))
