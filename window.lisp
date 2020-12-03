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

(defparameter *soft* 0.9)
(defvar +cos30+ (coerce (cos (/ pi 6)) 'single-float))
(defvar +sin30+ (coerce (sin (/ pi 6)) 'single-float))
(defvar +sf-pi+ (coerce pi 'single-float))

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
	   (three-quarters-r (* 0.75 r))
	   (three-halfs-r (* 1.5 r))

	   (hex-centre-x (+ origin-x
			    
			    r
			    (* (x crd) three-halfs-r)))
	   (hex-centre-y (+ (- window-centre-y-pix
			       (+ origin-y
				  (* (y crd) full-down-y)))
			    (- half-down-y)
			    window-centre-y-pix
			    (* -1 full-down-y)
			    (* (mod (1- (x crd)) 2) ;apply on even x
			       half-down-y))))

      (cairo:with-context (cairo-context)
	(cairo:set-source-rgb 0.5 0.5 0.5)
	(cairo:set-line-width 0.5)
	(let ((top (record-contours hex :nnw :n 1))
	      (left (record-contours hex :nw :nnw 1))
	      (bottom (record-contours hex :nw :cen 1))
	      (right (record-contours hex :n :cen 1)))
	  (dolist (elevation (contours-list bottom))
	    (cond
	      ((is-contour-of elevation left)
	       (extract-contour elevation bottom)
	       (extract-contour elevation left)
	       (let* ((long-cathetus-offset
			(- half-down-y
			   (contour-offset (contour-index elevation bottom)
					   (contours-range bottom)
					   half-down-y)))
		      (short-cathetus-offset
			(contour-offset (contour-index elevation left)
					(contours-range left)
					half-r))
		      (xy0
			(crd long-cathetus-offset
			     0))
		      (xy1
			(crd long-cathetus-offset
			     (* short-cathetus-offset *soft*)))
		      (xy2
			(crd (- half-down-y
				(* long-cathetus-offset 0.5 *soft*))
			     short-cathetus-offset))
		      (xy3
			(crd half-down-y
			     short-cathetus-offset))
		      (angle (* 5/6 +sf-pi+))
		      (sin (sin angle))
		      (cos (cos angle)))

		 (rotate xy0 () sin cos hex-centre-x hex-centre-y)
		 (rotate xy1 () sin cos hex-centre-x hex-centre-y)
		 (rotate xy2 () sin cos hex-centre-x hex-centre-y)
		 (rotate xy3 () sin cos hex-centre-x hex-centre-y)

		 (cairo:move-to (x xy0) (y xy0))
		 (cairo:curve-to (x xy1) (y xy1)
				 (x xy2) (y xy2)
				 (x xy3) (y xy3))

		 '(progn
		   (cairo:line-to (x xy1) (y xy1))
		   (cairo:line-to (x xy2) (y xy2))
		   (cairo:line-to (x xy3) (y xy3)))
		 ))
	      ((is-contour-of elevation right)
	       (extract-contour elevation bottom)
	       (extract-contour elevation right)
	       (let* ((long-cathetus-offset
			(- half-down-y
			   (contour-offset (contour-index elevation bottom)
					   (contours-range bottom)
					   half-down-y)))
		      (right-cathetus-offset
			(- half-down-y
			   (contour-offset (contour-index elevation right)
					   (contours-range right)
					   half-down-y)))
		      (xy0
			(crd long-cathetus-offset
			     0))
		      (xy1
			(crd long-cathetus-offset
			     (* 0.36 long-cathetus-offset)))
		      
		      (angle-diff (/ +sf-pi+ -3))
		      (sin-diff (sin angle-diff))
		      (cos-diff (cos angle-diff))
		      
		      (xy2
			(rotate (crd right-cathetus-offset
				     (* -0.36 right-cathetus-offset))
				() sin-diff cos-diff))
		      (xy3
			(rotate (crd right-cathetus-offset
				     0)
				() sin-diff cos-diff))
		      
		      (angle (* 5/6 +sf-pi+))
		      (sin (sin angle))
		      (cos (cos angle)))

		 (rotate xy0 () sin cos hex-centre-x hex-centre-y)
		 (rotate xy1 () sin cos hex-centre-x hex-centre-y)
		 (rotate xy2 () sin cos hex-centre-x hex-centre-y)
		 (rotate xy3 () sin cos hex-centre-x hex-centre-y)

		 (cairo:move-to (x xy0) (y xy0))
		 (cairo:curve-to (x xy1) (y xy1)
		   (x xy2) (y xy2)
		   (x xy3) (y xy3))
		 '(progn
		   (cairo:line-to (x xy1) (y xy1))
		   (cairo:line-to (x xy2) (y xy2))
		   (cairo:line-to (x xy3) (y xy3)))
		 
		 
		 ))
	      ((is-contour-of elevation top)
	       (extract-contour elevation bottom)
	       (extract-contour elevation top)
	       (let* ((long-cathetus-offset
			(- half-down-y
			   (contour-offset (contour-index elevation bottom)
					   (contours-range bottom)
					   half-down-y)))
		      (top-cathetus-offset
			(contour-offset (contour-index elevation top)
					(contours-range top)
					half-r))
		      (xy0
			(crd long-cathetus-offset
			     0))
		      (xy1
			(crd long-cathetus-offset
			     (* 0.36 long-cathetus-offset)))

		      (angle-diff (/ +sf-pi+ -3))
		      (sin-diff (sin angle-diff))
		      (cos-diff (cos angle-diff))

		      (xy2
			(rotate (crd (- half-down-y
					(* long-cathetus-offset 0.5 *soft*))
				     (- top-cathetus-offset half-r))
				() sin-diff cos-diff))
		      (xy3
			(rotate (crd half-down-y
				     (- top-cathetus-offset half-r))
				() sin-diff cos-diff))
		      
		      (angle (* 5/6 +sf-pi+))
		      (sin (sin angle))
		      (cos (cos angle)))
		 
		 (rotate xy0 () sin cos hex-centre-x hex-centre-y)
		 (rotate xy1 () sin cos hex-centre-x hex-centre-y)
		 (rotate xy2 () sin cos hex-centre-x hex-centre-y)
		 (rotate xy3 () sin cos hex-centre-x hex-centre-y)

		 (cairo:move-to (x xy0) (y xy0))
		 (cairo:curve-to (x xy1) (y xy1)
		   (x xy2) (y xy2)
		   (x xy3) (y xy3))
		 '(progn
		   (cairo:line-to (x xy1) (y xy1))
		   (cairo:line-to (x xy2) (y xy2))
		   (cairo:line-to (x xy3) (y xy3)))
		   ))
	      ))
	  (dolist (elevation (contours-list right))
	    (cond
	      ((member elevation (contours-list left))
	       (extract-contour elevation right)
	       (extract-contour elevation left)
	       (let* ((right-cathetus-offset
			(- half-down-y
			   (contour-offset (contour-index elevation right)
					   (contours-range right)
					   half-down-y)))
		      (left-cathetus-offset
			(contour-offset (contour-index elevation left)
					(contours-range left)
					half-r))

		      (angle-diff (/ +sf-pi+ -3))
		      (sin-diff (sin angle-diff))
		      (cos-diff (cos angle-diff))
		      (xy0
			(rotate (crd right-cathetus-offset
				     0)
				() sin-diff cos-diff))
		      (xy1
			(rotate (crd right-cathetus-offset
				     (* -0.36 right-cathetus-offset))
				() sin-diff cos-diff))

		      (xy2
			(crd (- half-down-y
				(* right-cathetus-offset 0.5 *soft*))
			     left-cathetus-offset))
		      (xy3
			(crd half-down-y
			     left-cathetus-offset))
		      
		      (angle (* 5/6 +sf-pi+))
		      (sin (sin angle))
		      (cos (cos angle)))

	     	 (rotate xy0 () sin cos hex-centre-x hex-centre-y)
		 (rotate xy1 () sin cos hex-centre-x hex-centre-y)
		 (rotate xy2 () sin cos hex-centre-x hex-centre-y)
		 (rotate xy3 () sin cos hex-centre-x hex-centre-y)

		 (cairo:move-to (x xy0) (y xy0))
		 (cairo:curve-to (x xy1) (y xy1)
				 (x xy2) (y xy2)
				 (x xy3) (y xy3))

		 '(progn
		   (cairo:line-to (x xy1) (y xy1))
		   (cairo:line-to (x xy2) (y xy2))
		   (cairo:line-to (x xy3) (y xy3)))
		 
		 ))
	      
	      ))
	  (cairo:stroke))
	
	))
    (cairo:destroy cairo-context)
    (cairo:destroy cairo-surface)))

(defun rotate (crd angle
	       &optional
		 (sin (sin angle)) (cos (cos angle))
		 (x+ 0) (y+ 0))
  "Destructively rotates CRD around origin using up is down coordinates."
  (psetf (x crd) (+ (* cos (x crd))
		    (* sin (y crd))
		    x+)
	 (y crd) (+ (- (* cos (y crd))
		       (* sin (x crd)))
		    y+))
  crd)
