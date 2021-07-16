(in-package :hexhammer)

;;; Offsets of x and y (low left origin) coordinates of hex (r = 0.5) vertices
(declaim (inline xofs yofs))
(defun xofs (direction)
  (ecase direction
    (:E 1.0)
    ((:SE :NE) 0.875)
    ((:SSE :NNE) 0.75)
    ((:S :N :CEN) 0.5)
    ((:SSW :NNW) 0.25)
    ((:SW :NW) 0.125)
    (:W 0.0)))

(defun yofs (direction)
  (ecase direction
    ((:E :W :CEN) (coerce (* 1/2 (sin (/ pi 3))) 'single-float))
    ((:NNE :NNW :N) (coerce (sin (/ pi 3)) 'single-float))
    ((:NE :NW) (coerce (* 3/4 (sin (/ pi 3))) 'single-float))
    ((:SE :SW) (coerce (* 1/4 (sin (/ pi 3))) 'single-float))
    ((:SSW :SSE :S) 0.0)))

(defun unit-hex-crd (dir)
  (case dir
    (:CEN (crd 0.0 0.0))
    (:E (crd 1.0 0.0))
    (:NE (crd 0.75 (* +sin60+ 0.5)))
    (:NNE (crd 0.5 +sin60+))
    (:N (crd 0.0 +sin60+))
    (:NNW (crd -0.5 +sin60+))
    (:NW (crd -0.75 (* +sin60+ 0.5)))
    (:W (crd -1.0 0.0))
    (:SW (crd -0.75 (* +sin60+ -0.5)))
    (:SSW (crd -0.5 (- +sin60+)))
    (:S (crd 0.0 (- +sin60+)))
    (:SSE (crd 0.5 (- +sin60+)))
    (:SE (crd 0.75 (* +sin60+ -0.5)))))

(defun translate-unit-crd (crd r)
  (crd (* (x crd) r)
       (* -1 (y crd) r)))

(defun contour-offset (contour-index contour-count edge-vector-pix)
  "Returns contour's position on edge in pixels clockwise."
  (declare (optimize speed)
	   (fixnum contour-index contour-count)
	   (single-float edge-vector-pix))
  (let* ((margin (/ edge-vector-pix 4))
	 (width (/ (- edge-vector-pix
		      (* 2 margin))
		   (+ 1.0 (the fixnum (abs contour-count))))))
    (+ margin (* (+ 1.0 contour-index)
		 width))))
