(in-package :hexhammer)

;;; Offsets of x and y (low left origin) coordinates of hex (r = 0.5) vertices
(defmacro xofs (direction)
  (case direction
    (:E 1.0)
    (:NE 0.875)
    (:NNE 0.75)
    (:CEN '(xofs :N))
    (:N 0.5)
    (:NNW 0.25)
    (:NW 0.125)
    (:W 0.0)
    (:SW '(xofs :NW))
    (:SSW '(xofs :NNW))
    (:S '(xofs :N))
    (:SSE '(xofs :NNE))
    (:SE '(xofs :NE))))

(defmacro yofs (direction)
  (case direction
    (:E '(yofs :CEN))
    (:NE '(yofs :NW))
    (:NNE '(yofs :N))
    (:CEN (coerce (* 1/2 (sin (/ pi 3))) 'single-float))
    (:N (coerce (sin (/ pi 3)) 'single-float))
    (:NNW '(yofs :N))
    (:NW (coerce (* 3/4 (sin (/ pi 3))) 'single-float))
    (:W '(yofs :CEN))
    (:SW (coerce (* 1/4 (sin (/ pi 3))) 'single-float))
    (:SSW '(yofs :S))
    (:S 0.0)
    (:SSE '(yofs :S))
    (:SE '(yofs :SW))))

(defun contour-offset (contour-index contour-count edge-x-vector-pix)
  "Pixels to the right of this edge's left vertex."
  (let ((width (/ edge-x-vector-pix (+ 1 (abs contour-count)))))
    (* (1+ contour-index) width)))
