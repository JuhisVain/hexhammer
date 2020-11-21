(in-package :hexhammer)

;;GOOD ENOUGH FOR TESTING
;; optimized and tested on a single tiny ancient pgm pic
;; pgm max value must be < 256
(defun read-pgm (file)

  (with-open-file (input file :element-type '(unsigned-byte 8))
    (flet ((skipper ()
	     (do ((byte (read-byte input) (read-byte input)))
		 ((= byte 10)))
	     t))
      (and (= (read-byte input) 80) ;P
	   (= (read-byte input) 53) ;5
	   (= (read-byte input) 10) ;line feed

	   (skipper))

      (let* ((width (parse-integer
		     (map 'string #'code-char
			  (loop for char = (read-byte input)
				collect char into width
				when (= char 32) ;not space
				  return width))))
	     (height (prog1 (parse-integer
			     (map 'string #'code-char
				  (loop for char = (read-byte input)
					collect char into width
					when (= char 10) ;not linefeed
					  return width)))
		       (skipper)))
	     (data (make-array (list width height)
			       :initial-element 0
			       :element-type '(unsigned-byte 8))))

	;; Generate 2d array where (0,0) is at low left
	(dotimes (y height)
	  (dotimes (x width)
	    (setf (aref data x (- height (1+ y))) (read-byte input))))
	data))))

;;copied from my plotter
(defun 2faref (2d-array x y)
  "Like aref for two dimensional array of numbers 2D-ARRAY,
but indexes X and Y should be floats between 0 and 1.

Returns some kind of weighted average of the 4 array elements forming
square within which indexes X and Y point to."
  (declare ((simple-array * 2) 2d-array) ; array might hold symbols
	   ((single-float 0.0 1.0) x y))
  (let* ((x-index (* x (1- (array-dimension 2d-array 0))))
	 (x-floor (floor x-index))
	 (x-ceili (ceiling x-index))
	 
	 (y-index (* y (1- (array-dimension 2d-array 1))))
	 (y-floor (floor y-index))
	 (y-ceili (ceiling y-index))
	 
	 (point-x (rem x-index 1))
	 (point-y (rem y-index 1))
	 (ip-x (- 1.0 point-x)) ; inverted point-x
	 (ip-y (- 1.0 point-y)) ; inverted point-y

	 (weight-ff (* ip-x 
		       ip-y)) 
	 (weight-cf (* point-x ; inverted inverted point-x
		       ip-y))
	 (weight-fc (* ip-x
		       point-y))
	 (weight-cc (* point-x
		       point-y)))

    (/ (+ (* (aref 2d-array x-floor y-floor)
	     weight-ff)
	  (* (aref 2d-array x-floor y-ceili)
	     weight-fc)
	  (* (aref 2d-array x-ceili y-floor)
	     weight-cf)
	  (* (aref 2d-array x-ceili y-ceili)
	     weight-cc))
       (+ weight-ff
	  weight-fc
	  weight-cf
	  weight-cc))))
