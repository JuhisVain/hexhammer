(in-package :hexhammer)

(defun test ()
  (sdl2:with-init (:everything)
    (sdl2:with-window
	(window :title "HH" :w 1000 :h 800 :flags '(:shown :resizable))
      (sdl2:with-renderer
	  (renderer window :index -1 :flags '(:accelerated :presentvsync))
      
	(let ((texture (sdl2:create-texture
			renderer :ARGB8888 :streaming 1000 800)))
	  (autowrap:with-alloc (buffer :unsigned-char (* 1000 800 4))
	    
	    
	    (sdl2:set-render-draw-color renderer 100 100 100 255)
	    (sdl2:render-clear renderer)
	    (sdl2:render-present renderer)

	    (let ((test-state
		    (make-instance 'view-state
				   :texture texture
				   :buffer buffer))
		  (test-world
		    (make-world)))
	      (defparameter *world* test-world)
	      (generate-map 100 50 test-world)

	      (dotimes (x 100)
		(dotimes (y 50)
		  (draw-hex-borders (crd x y) test-state)
		  (draw-contours (crd x y) test-world test-state)))



	      (sdl2:with-event-loop (:method :poll)

		(:mousebuttondown (:x x :y y :button button)
				  (format t "x=~a ; y=~a ; but: ~a~%" x y button)
				  (when (= button 3)
				    (setf (centre-x test-state) (+ (centre-x test-state)
								   (- x (/ (width test-state) 2)))
					  (centre-y test-state) (- (centre-y test-state)
								   (- y (/ (height test-state) 2))))
				    (format t "centre: (~a;~a)~%"
					    (centre-x test-state)
					    (centre-y test-state))
				    (clear-all test-state)
				    (dotimes (x 100)
				      (dotimes (y 50)
					(draw-hex-borders (crd x y) test-state)
					(draw-contours (crd x y) test-world test-state)))
				    
				    ))

		(:mousewheel (:y roll) ; 1 = away, -1 inwards, todo: test with non smooth wheel
			     (incf (hex-r test-state) (* 10 roll))

			     (clear-all test-state)
			     (dotimes (x 100)
			       (dotimes (y 50)
				 (draw-hex-borders (crd x y) test-state)
				 (draw-contours (crd x y) test-world test-state)))
			     
			     )
		
		(:idle ()
		       
		       
		       (sdl2:update-texture texture nil
					    buffer
					    (* 4 1000)) ; ARGB8888 size * texture width
		       (sdl2:render-clear renderer)
		       (sdl2:render-copy renderer texture)
		       (sdl2:render-present renderer)
		       )
		(:quit () t)
		)

	      (sdl2:destroy-texture texture)

	      )))))))

(defvar +sin60+ (sqrt (/ 3 4)))
(defvar +cos60+ 0.5)
(defvar +tan60+ (coerce (tan (/ pi 3)) 'single-float))

(defun clear-all (view-state)
  (let* ((cairo-surface
	   (cairo:create-image-surface-for-data
	    (buffer view-state) :argb32
	    (width view-state) (height view-state)
	    (* 4 (width view-state))))
	 (cairo-context (cairo:create-context cairo-surface)))
    (cairo:with-context (cairo-context)
      (cairo:set-source-rgb 0 0 0)
      (cairo:paint))
    (cairo:destroy cairo-context)
    (cairo:destroy cairo-surface)))

(defun draw-hex-borders (crd view-state)
  (let* ((cairo-surface
	   (cairo:create-image-surface-for-data
	    (buffer view-state) :argb32
	    (width view-state) (height view-state)
	    (* 4 (width view-state))))
	 (cairo-context (cairo:create-context cairo-surface)))

    (let* ((window-centre-x-pix (/ (width view-state) 2))
	   (window-centre-y-pix (/ (height view-state) 2))

	   (origin-x (- window-centre-x-pix (centre-x view-state)))
	   (origin-y (- window-centre-y-pix (centre-y view-state)))

	   (r (hex-r view-state))
	   (2r (* 2.0 r))
	   (half-down-y (* +sin60+ r))
	   (full-down-y (* half-down-y 2))
	   (half-r (/ r 2.0))
	   (three-halfs-r (* 1.5 r))
	   (top-left-x (+ origin-x (* (x crd) three-halfs-r)))
	   (top-left-y (+ (- window-centre-y-pix
			     (+ origin-y
				(* (y crd) full-down-y)))
			  window-centre-y-pix
			  (* -2 full-down-y)
			  (* (mod (1- (x crd)) 2) ;apply on even x
			     half-down-y))))
      
      (cairo:with-context (cairo-context)
	(if (= (x crd) (y crd) 0)
	    (cairo:set-source-rgb 0.6 0.6 0.6) ;; (0 . 0) gray
	    (cairo:set-source-rgb 0.6 0 0.6))
	(cairo:set-line-width 1)
	(cairo:move-to (+ top-left-x half-r) top-left-y) ;; NW
	(cairo:line-to top-left-x (+ top-left-y half-down-y)) ;; W
	(cairo:line-to (+ top-left-x half-r) (+ top-left-y full-down-y)) ;; SW
	(cairo:line-to (+ top-left-x three-halfs-r) (+ top-left-y full-down-y)) ;; SE
	(cairo:line-to (+ top-left-x 2r) (+ top-left-y half-down-y)) ;; E
	(cairo:line-to (+ top-left-x three-halfs-r) top-left-y) ;; NE
	(cairo:line-to (+ top-left-x half-r) top-left-y) ;; NW
	(cairo:stroke)))
    
    (cairo:destroy cairo-context)
    (cairo:destroy cairo-surface)))

(defun draw-contours (crd map view-state)
  (let* ((cairo-surface
	   (cairo:create-image-surface-for-data
	    (buffer view-state) :argb32
	    (width view-state) (height view-state)
	    (* 4 (width view-state))))
	 (cairo-context (cairo:create-context cairo-surface))
	 (hex (gethash crd (world-map map))))
    (unless hex (return-from draw-contours))
    (let* ((window-centre-x-pix (/ (width view-state) 2))
	   (window-centre-y-pix (/ (height view-state) 2))

	   (origin-x (- window-centre-x-pix (centre-x view-state)))
	   (origin-y (- window-centre-y-pix (centre-y view-state)))

	   (r (hex-r view-state))
	   (2r (* 2.0 r))
	   (half-down-y (* +sin60+ r))
	   (quarter-down-y (/ half-down-y 2))
	   (full-down-y (* half-down-y 2))
	   (half-r (/ r 2.0))
	   (quarter-r (/ r 4.0))
	   (three-halfs-r (* 1.5 r))
	   (top-left-x (+ origin-x (* (x crd) three-halfs-r)))
	   (top-left-y (+ (- window-centre-y-pix
			     (+ origin-y
				(* (y crd) full-down-y)))
			  window-centre-y-pix
			  (* -2 full-down-y)
			  (* (mod (1- (x crd)) 2) ;apply on even x
			     half-down-y))))

      (cairo:with-context (cairo-context)
	(cairo:set-source-rgb 0.5 0.5 0.5)
	(cairo:set-line-width 0.5)
	
	#|
	(cairo:move-to (+ top-left-x half-r) top-left-y) ;; NW
	(cairo:line-to top-left-x (+ top-left-y half-down-y)) ;; W
	(cairo:line-to (+ top-left-x half-r) (+ top-left-y full-down-y)) ;; SW
	(cairo:line-to (+ top-left-x three-halfs-r) (+ top-left-y full-down-y)) ;; SE
	(cairo:line-to (+ top-left-x 2r) (+ top-left-y half-down-y)) ;; E
	(cairo:line-to (+ top-left-x three-halfs-r) top-left-y) ;; NE
	(cairo:line-to (+ top-left-x half-r) top-left-y) ;; NW
	|#

	;;do NNW "square":
	(let ((top (record-contours hex :nnw :n 1))
	      (left (record-contours hex :nw :nnw 1))
	      (bottom (record-contours hex :nw :cen 1))
	      (right (record-contours hex :n :cen 1)))
	  (do-contours (elevation bottom)
	    (cond ((is-contour-of elevation left)
		   (let* ((x0 (+ top-left-x quarter-r
				 (contour-offset (contour-index elevation bottom)
					;index-from-left
						 (contours-range bottom)
						 (- r quarter-r))))
			  (y0 (+ top-left-y quarter-down-y
				 (contour-offset (contour-index elevation bottom)
						 ;index-from-left
						 (contours-range bottom)
						 quarter-down-y)))
			  (x1 (+ x0 (* +cos60+ (* 0.16 r))))
			  (y1 (- y0 (* +sin60+ (* 0.16 r))))

			  (recipient-index (contour-index elevation left))
			  (x3 (+ top-left-x quarter-r
				 (contour-offset recipient-index
						 (contours-range left)
						 quarter-r)))
			  (y3 (+ top-left-y quarter-down-y
				 (- (contour-offset recipient-index
						    (contours-range left)
						    quarter-down-y))))
			  (yowza (/ (/ (- (* +sin60+ r)
					  half-r)
				       2)
				    +sin60+))
			  (x2 (+ x3 yowza))
			  (y2 (+ y3 (* +tan60+ (/ yowza 2)))))

		     (cairo:move-to x0 y0)
		     (cairo:curve-to x1 y1 x2 y2 x3 y3)
		     ;(cairo:line-to x1 y1)(cairo:line-to x2 y2)(cairo:line-to x3 y3)
		     ))
		  ((is-contour-of elevation right)
		   (let* (;; same as with (is-contour-of elevation LEFT)
			  (x0 (+ top-left-x quarter-r
				 (contour-offset (contour-index elevation bottom)
						 ;index-from-left
						   (contours-range bottom)
						   (- r quarter-r))))
			  (y0 (+ top-left-y quarter-down-y
				 (contour-offset (contour-index elevation bottom)
						 ;index-from-left
						   (contours-range bottom)
						   quarter-down-y)))
			  (x1 (+ x0 (* +cos60+ (* 0.16 r))))
			  (y1 (- y0 (* +sin60+ (* 0.16 r))))
			  ;; end of sameness
			  (recipient-index (contour-index elevation right))
			  (x3 (+ top-left-x r))
			  (y3 (+ top-left-y
				 (contour-offset recipient-index
						 (contours-range right)
						 half-down-y)))
			  (x2 (- x3 (* 0.16 r)))
			  (y2 y3))

		     (cairo:move-to x0 y0)
		     ;(cairo:curve-to x1 y1 x2 y2 x3 y3)
		     (cairo:line-to x1 y1)(cairo:line-to x2 y2)(cairo:line-to x3 y3)
		     ))
		  
		  ((is-contour-of elevation top)
		   (let* (;; same as with (is-contour-of elevation LEFT & RIGHT)
			  (x0 (+ top-left-x quarter-r
				 (contour-offset
				  ;index-from-left
				  (contour-index elevation bottom)
				  (contours-range bottom)
				  (- r quarter-r))))
			  (y0 (+ top-left-y quarter-down-y
				 (contour-offset
				  ;index-from-left
				  (contour-index elevation bottom)
				  (contours-range bottom)
				  quarter-down-y)))
			  (x1 (+ x0 (* +cos60+ (* 0.16 r))))
			  (y1 (- y0 (* +sin60+ (* 0.16 r))))
			  ;; end of sameness
			  (recipient-index (contour-index elevation top))
			  (x3 (+ top-left-x half-r
				 (contour-offset recipient-index
						 (contours-range top)
						 half-r)))
			  (y3 top-left-y)
			  (x2 x3)
			  (y2 (+ top-left-y
				 (/ (/ (- (* +sin60+ r)
					  half-r)
				       2)
				    +sin60+))
			      ))

		     (when (equal crd '(2 . 2))
		       (format t "elevation: ~a // index: ~a recip: ~a~%"
			       elevation (contour-index elevation bottom) recipient-index))
		     
		     (cairo:move-to x0 y0)
		     (cairo:curve-to x1 y1 x2 y2 x3 y3)
		     )))) ;; end bottom

	  )
	
	(cairo:stroke)))

    (cairo:destroy cairo-context)
    (cairo:destroy cairo-surface)))
