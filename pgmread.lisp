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
	     (data (make-array (* width height) :initial-element 0
						:element-type '(unsigned-byte 8))))
	
	(dotimes (i (* width height))
	  (read-sequence data input))
	data
	))))
