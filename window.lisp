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
	      (generate-map 100 100
			    ;;"mhmap.pgm" ;; mini mars
			    "getty.pgm" ;; huge gettysburg
			    test-world)

	      
	      (do-visible (x y test-state)
		(when (gethash (crd x y) (world-map test-world))
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
				    (time
				     (do-visible (x y test-state)
				       (when (gethash (crd x y) (world-map test-world))
					 (draw-hex-borders (crd x y) test-state)
					 (draw-contours (crd x y) test-world test-state))))
				    )
				  (when (= button 1)
				    (format t "That's hex ~a~%"
					    (hex-xy-at-pix x y test-state)))
				  )

		(:mousewheel (:y roll) ; 1 = away, -1 inwards, todo: test with non smooth wheel
			     (incf (hex-r test-state) (* 10 roll))

			     (clear-all test-state)
			     (do-visible (x y test-state)
			       (when (gethash (crd x y) (world-map test-world))
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

(defconstant +sin60+ (sqrt (/ 3 4)))
(defconstant +cos60+ 0.5)
(defconstant +tan60+ (coerce (tan (/ pi 3)) 'single-float))

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
(declaim (single-float *soft*))
(defconstant +cos30+ (coerce (cos (/ pi 6)) 'single-float))
(defconstant +sin30+ (coerce (sin (/ pi 6)) 'single-float))
(defconstant +sf-pi+ (coerce pi 'single-float))

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
	   (half-down-y (* +sin60+ r))
	   (full-down-y (* half-down-y 2))
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

      (let ((top (record-contours hex :n :nnw 1))
	    (left (record-contours hex :nnw :nw 1))
	    (bottom (record-contours hex :nw :cen 1))
	    (right (record-contours hex :cen :n 1))
	    (angle (* 5/6 +sf-pi+)))
	(draw-kite-contours top left bottom right
			    angle hex-centre-x hex-centre-y
			    (hex-r view-state) cairo-context))

      (let ((top (record-contours hex :ne :nne 1))
	    (left (record-contours hex :nne :n 1))
	    (bottom (record-contours hex :n :cen 1))
	    (right (record-contours hex :cen :ne 1))
	    (angle (* 3/6 +sf-pi+)))
	(draw-kite-contours top left bottom right
			    angle hex-centre-x hex-centre-y
			    (hex-r view-state) cairo-context))

      (let ((top (record-contours hex :se :e 1))
	    (left (record-contours hex :e :ne 1))
	    (bottom (record-contours hex :ne :cen 1))
	    (right (record-contours hex :cen :se 1))
	    (angle (* 1/6 +sf-pi+)))
	(draw-kite-contours top left bottom right
			    angle hex-centre-x hex-centre-y
			    (hex-r view-state) cairo-context))

      (let ((top (record-contours hex :s :sse 1))
	    (left (record-contours hex :sse :se 1))
	    (bottom (record-contours hex :se :cen 1))
	    (right (record-contours hex :cen :s 1))
	    (angle (* 11/6 +sf-pi+)))
	(draw-kite-contours top left bottom right
			    angle hex-centre-x hex-centre-y
			    (hex-r view-state) cairo-context))

      (let ((top (record-contours hex :sw :ssw 1))
	    (left (record-contours hex :ssw :s 1))
	    (bottom (record-contours hex :s :cen 1))
	    (right (record-contours hex :cen :sw 1))
	    (angle (* 9/6 +sf-pi+)))
	(draw-kite-contours top left bottom right
			    angle hex-centre-x hex-centre-y
			    (hex-r view-state) cairo-context))
      
      (let ((top (record-contours hex :nw :w 1))
	    (left (record-contours hex :w :sw 1))
	    (bottom (record-contours hex :sw :cen 1))
	    (right (record-contours hex :cen :nw 1))
	    (angle (* 7/6 +sf-pi+)))
	(draw-kite-contours top left bottom right
			    angle hex-centre-x hex-centre-y
			    (hex-r view-state) cairo-context))
      )
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

(defun OBSOLETEdraw-kite-contours (top left bottom right
			   angle hex-centre-x hex-centre-y
			   hex-radius cairo-context)
  (let* (;; Angle to rotate top & right contours:
	 (angle-diff ;(+ angle
			(/ +sf-pi+ -3));)
	 (sin-diff (sin angle-diff))
	 (cos-diff (cos angle-diff))
	 
	 (sin (sin angle))
	 (cos (cos angle))

	 (half-down-y (* +sin60+ hex-radius))
	 (half-r (/ hex-radius 2.0)))

    (cairo:with-context (cairo-context)
      (cairo:set-source-rgb 0.5 0.5 0.5)
      (cairo:set-line-width 0.5)
      
      (probe-contours (left bottom) elevation
	;;CONFIRMED USEFUL
	;;(format t "1. probe-contours (left bottom) IS NOT USELESS~%")
	(let* ((bottom-offset
		 (- half-down-y
		    (contour-offset (contour-index elevation bottom)
				    (contours-range bottom)
				    half-down-y)))
	       (left-offset
		 (- half-r 
		    (contour-offset (contour-index elevation left)
				    (contours-range left)
				    half-r)))
	       (xy0
		 (crd bottom-offset
		      0))
	       (xy1
		 (crd bottom-offset
		      (* left-offset *soft*)))
	       (xy2
		 (crd (+ bottom-offset
			 (* *soft* (- half-down-y
				      bottom-offset)))
		      left-offset))
	       (xy3
		 (crd half-down-y
		      left-offset)))

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
      
      (probe-contours (top bottom) elevation
	;;CONFIRMED USEFUL
	;;(format t "2. probe-contours (top bottom) IS NOT USELESS~%")
	(let* ((bottom-offset
		 (- half-down-y
		    (contour-offset (contour-index elevation bottom)
				    (contours-range bottom)
				    half-down-y)))
	       (top-offset
		 (contour-offset (contour-index elevation top)
				 (contours-range top)
				 (- half-r)))
	       (xy0
		 (crd bottom-offset
		      0))
	       (xy1
		 (crd bottom-offset
		      (* 0.36 bottom-offset)))
	       (xy2
		 (rotate (crd (+ bottom-offset
				 (* *soft*
				    0.5
				    (- half-down-y
				       bottom-offset)))
			      top-offset)
			 () sin-diff cos-diff))
	       (xy3
		 (rotate (crd half-down-y
			      top-offset)
			 () sin-diff cos-diff)))

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
	  )
	)
      
      (probe-contours (right bottom) elevation
	;;CONFIRMED USEFUL
	;;(format t "3. probe-contours (right bottom) IS NOT USELESS~%")
	(let* ((bottom-offset
		 (- half-down-y
		    (contour-offset (contour-index elevation bottom)
				    (contours-range bottom)
				    half-down-y)))
	       (right-offset
		 (contour-offset (contour-index elevation right)
				 (contours-range right)
				 half-down-y))

	       (xy0 (crd bottom-offset
			 0))
	       (xy1 (crd bottom-offset
			 (* 0.36 bottom-offset)))
	       (xy2 (rotate (crd right-offset
				 (* -0.36 right-offset))
			    () sin-diff cos-diff))
	       (xy3 (rotate (crd right-offset
				 0)
			    () sin-diff cos-diff)))

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
	    (cairo:line-to (x xy3) (y xy3)))))
      
      (probe-contours (top left) elevation
	;;CONFIRMED USEFUL
	;;(format t "4. probe-contours (top left) IS NOT USELESS~%")
	(let* ((left-offset
		 (- half-r 
		    (contour-offset (contour-index elevation left)
				    (contours-range left)
				    half-r)))
	       (top-offset
		 (contour-offset (contour-index elevation top)
				 (contours-range top)
				 (- half-r)))

	       (xy0 (crd half-down-y
			 left-offset))
	       (xy1 (crd (- half-down-y
			    (* 0.67 ;?
			       (- half-r
				  left-offset)))
			 left-offset))
	       
	       (xy2 (rotate (crd (+ half-down-y
				    (* 0.67 top-offset))
				 top-offset)
			    () sin-diff cos-diff))
	       (xy3 (rotate (crd half-down-y
				 top-offset)
			    () sin-diff cos-diff)))
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

      (probe-contours (right left) elevation
	;;CONFIRMED USEFUL
	;;(format t "5. probe-contours (right left) IS NOT USELESS~%")
	(let* ((left-offset
		 (- half-r 
		    (contour-offset (contour-index elevation left)
				    (contours-range left)
				    half-r)))
	       (right-offset
		 (contour-offset (contour-index elevation right)
				 (contours-range right)
				 half-down-y))

	       (xy0
		 (crd half-down-y
		      left-offset))
	       (xy1
		 (crd (- half-down-y
			 (* *soft*
			    0.5
			    right-offset))
		      left-offset))

	       (xy2
		 (rotate (crd right-offset
			      (* -0.36 right-offset))
			 () sin-diff cos-diff))
	       (xy3
		 (rotate (crd right-offset
			      0)
			 () sin-diff cos-diff)))

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

      
      (probe-contours (bottom left) elevation
	;;useless?
	(format t "6. probe-contours (bottom left) IS NOT USELESS~%")
	#|(let* ((bottom-offset
	(- half-down-y
	(contour-offset (contour-index elevation bottom)
	(contours-range bottom)
	half-down-y)))
	(left-offset
	(- half-r 
	(contour-offset (contour-index elevation left)
	(contours-range left)
	half-r)))
	(xy0
	(crd bottom-offset
	0))
	(xy1
	(crd bottom-offset
	(* left-offset *soft*)))
	(xy2
	(crd (+ bottom-offset
	(* *soft* (- half-down-y
	bottom-offset)))
	left-offset))
	(xy3
	(crd half-down-y
	left-offset))
	(angle (* 5/6 +sf-pi+))
	(sin (sin angle))
	(cos (cos angle)))

	;; If this ever prints these procedures will be redeemed
	(format t "Probe-contours (bottom left) IS NOT USELESS!~%")

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
	)|#
	)
      
      (probe-contours (right top) elevation
	;;CONFIRMED USEFUL
	;;(format t "7. probe-contours (right top) IS NOT USELESS~%")
	(let* ((top-offset
		 (contour-offset (contour-index elevation top)
				 (contours-range top)
				 (- half-r)))
	       (right-offset
		 (contour-offset (contour-index elevation right)
				 (contours-range right)
				 half-down-y))

	       (xy0 (rotate (crd half-down-y
				 top-offset)
			    () sin-diff cos-diff))
	       (xy1 (rotate (crd
			     (+ right-offset
				(* *soft*
				   (- half-down-y
				      right-offset)))
			     top-offset)
			    () sin-diff cos-diff))
	       (xy2 (rotate (crd right-offset
				 (* *soft* top-offset))
			    () sin-diff cos-diff))
	       (xy3 (rotate (crd right-offset
				 0)
			    () sin-diff cos-diff)))
	  
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
	  
	  )
	)
      (probe-contours (bottom top) elevation
	;;CONFIRMED USEFUL
	;;(format t "8. probe-contours (bottom top) IS NOT USELESS~%")
	(let* ((bottom-offset
		 (- half-down-y
		    (contour-offset (contour-index elevation bottom)
				    (contours-range bottom)
				    half-down-y)))
	       (top-offset
		 (contour-offset (contour-index elevation top)
				 (contours-range top)
				 (- half-r)))
	       (xy0
		 (crd bottom-offset
		      0))
	       (xy1
		 (crd bottom-offset
		      (* 0.36 bottom-offset)))
	       (xy2
		 (rotate (crd (+ bottom-offset
				 (* *soft*
				    0.5
				    (- half-down-y
				       bottom-offset)))
			      top-offset)
			 () sin-diff cos-diff))
	       (xy3
		 (rotate (crd half-down-y
			      top-offset)
			 () sin-diff cos-diff)))

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
      
      (probe-contours (left top) elevation
	;; This one tries the exact same stuff as (top left)
	    ;;; If we can't connect on the inside corner of the neighbours
	    ;;; there's surely no way to connect on the outside either
	(format t "9. probe-contours (left top) IS NOT USELESS~%"))

      (probe-contours (bottom right) elevation
	;;CONFIRMED USEFUL
	;;(format t "10. probe-contours (bottom right) IS NOT USELESS~%")
	;; same code as (right bottom)
	(let* ((bottom-offset
		 (- half-down-y
		    (contour-offset (contour-index elevation bottom)
				    (contours-range bottom)
				    half-down-y)))
	       (right-offset
		 (contour-offset (contour-index elevation right)
				 (contours-range right)
				 half-down-y))

	       (xy0 (crd bottom-offset
			 0))
	       (xy1 (crd bottom-offset
			 (* 0.36 bottom-offset)))
	       (xy2 (rotate (crd right-offset
				 (* -0.36 right-offset))
			    () sin-diff cos-diff))
	       (xy3 (rotate (crd right-offset
				 0)
			    () sin-diff cos-diff)))

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
	    (cairo:line-to (x xy3) (y xy3))))
	
	)

      (probe-contours (left right) elevation
	;;CONFIRMED USEFUL
	;;(format t "11. probe-contours (left right) IS NOT USELESS~%")
	;; same code as (right left)
	(let* ((left-offset
		 (- half-r 
		    (contour-offset (contour-index elevation left)
				    (contours-range left)
				    half-r)))
	       (right-offset
		 (contour-offset (contour-index elevation right)
				 (contours-range right)
				 half-down-y))

	       (xy0
		 (crd half-down-y
		      left-offset))
	       (xy1
		 (crd (- half-down-y
			 (* *soft*
			    0.5
			    right-offset))
		      left-offset))

	       (xy2
		 (rotate (crd right-offset
			      (* -0.36 right-offset))
			 () sin-diff cos-diff))
	       (xy3
		 (rotate (crd right-offset
			      0)
			 () sin-diff cos-diff)))

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
	  
	  )
	)
      (probe-contours (top right) elevation
	;; same as (right top)
	(format t "12. probe-contours (top right) IS NOT USELESS~%"))

      ;; more test:
      (probe-contours (right top) elevation
	(format t "13. probe-contours (right top) IS NOT USELESS~%"))

      
      (probe-contours (bottom top) elevation
	;;CONFIRMED USEFUL
	;;(format t "14. probe-contours (bottom top) IS NOT USELESS~%")
	(let* ((bottom-offset
		 (- half-down-y
		    (contour-offset (contour-index elevation bottom)
				    (contours-range bottom)
				    half-down-y)))
	       (top-offset
		 (contour-offset (contour-index elevation top)
				 (contours-range top)
				 (- half-r)))
	       (xy0
		 (crd bottom-offset
		      0))
	       (xy1
		 (crd bottom-offset
		      (* 0.36 bottom-offset)))
	       (xy2
		 (rotate (crd (+ bottom-offset
				 (* *soft*
				    0.5
				    (- half-down-y
				       bottom-offset)))
			      top-offset)
			 () sin-diff cos-diff))
	       (xy3
		 (rotate (crd half-down-y
			      top-offset)
			 () sin-diff cos-diff)))

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
	  )
	)
      
      (probe-contours (left top) elevation
	(format t "15. probe-contours (left top) IS NOT USELESS~%"))

      (probe-contours (top left) elevation
	(format t "16. probe-contours (top left) IS NOT USELESS~%"))
      (probe-contours (right left) elevation
	(format t "17. probe-contours (rightleft) IS NOT USELESS~%"))
      (probe-contours (bottom left) elevation
	(format t "18. probe-contours (bottom left) IS NOT USELESS~%"))

      (probe-contours (left bottom) elevation
	(format t "19. probe-contours (left bottom) IS NOT USELESS~%"))
      (probe-contours (top bottom) elevation
	(format t "20. probe-contours (top bottom) IS NOT USELESS~%"))
      (probe-contours (right bottom) elevation
	(format t "21. probe-contours (right bottom) IS NOT USELESS~%"))

      (and (linkage-leftmost (contours-deque bottom))
	   (linkage-leftmost (contours-deque left))
	   (linkage-leftmost (contours-deque top))
	   (linkage-leftmost (contours-deque right))
	   (format t "STILL NOT EMPTY!!!~%"))

      ;;stroke at the end of each probe loop allows different line attributes:
      ;;(cairo:stroke)
      )))





(defun draw-kite-contours (top left bottom right
			   angle hex-centre-x hex-centre-y
			   hex-radius cairo-context)
  (declare (optimize speed)
	   (contours top left bottom right)
	   (single-float angle hex-centre-x hex-centre-y hex-radius)
	   (cairo:context cairo-context)
	   (dynamic-extent top left bottom right angle
			   hex-centre-x hex-centre-y hex-radius))
  (let* (;; Angle to rotate top & right contours:
	 (angle-d (+ angle
		     (/ +sf-pi+ -3)))
	 (sin-d (sin angle-d))
	 (cos-d (cos angle-d))
	 
	 (sin (sin angle))
	 (cos (cos angle))

	 (half-down-y (* +sin60+ hex-radius))
	 (half-r (/ hex-radius 2.0)))

    (macrolet ((offset-bottom ()
		 `(- half-down-y
		     (the single-float
			  (contour-offset (contour-index elevation bottom)
					  (contours-range bottom)
					  half-down-y))))
	       (offset-left ()
		 `(- half-r
		     (the single-float
			  (contour-offset (contour-index elevation left)
					  (contours-range left)
					  half-r))))
	       (offset-right ()
		 `(the single-float
		       (contour-offset (contour-index elevation right)
				       (contours-range right)
				       half-down-y)))
	       (offset-top ()
		 `(the single-float
		       (contour-offset (contour-index elevation top)
				       (contours-range top)
				       (- half-r))))
	       (rotation (bot-lefts top-rights)
		 `(progn
		    ,@(loop
			for symbol in bot-lefts
			collect `(rotate ,symbol ()
					 sin cos
					 hex-centre-x hex-centre-y))
		    ,@(loop
			for symbol in top-rights
			collect `(rotate ,symbol ()
					 sin-d cos-d
					 hex-centre-x hex-centre-y))))
	       (move-curve ()
		 '(progn
		   (cairo:move-to (x xy0) (y xy0))
		   (cairo:curve-to (x xy1) (y xy1)
		    (x xy2) (y xy2)
		    (x xy3) (y xy3)))))
      
      (cairo:with-context (cairo-context)
	(cairo:set-source-rgb 0.5 0.5 0.5)
	(cairo:set-line-width 0.5)
	
	(probe-contours (left bottom) elevation
	  (let* ((bottom-offset (offset-bottom))
		 (left-offset (offset-left))
		 (xy0 (crd bottom-offset 0.0))
		 (xy1 (crd bottom-offset
			   (* left-offset *soft*)))
		 (xy2 (crd (+ bottom-offset
			      (* *soft* (- half-down-y
					   bottom-offset)))
			   left-offset))
		 (xy3 (crd half-down-y left-offset)))
	    (rotation (xy0 xy1 xy2 xy3) ())
	    (move-curve)))
	
	(probe-contours (top bottom) elevation
	  (let* ((bottom-offset (offset-bottom))
		 (top-offset (offset-top))
		 (xy0 (crd bottom-offset 0))
		 (xy1 (crd bottom-offset
			   (* 0.36 bottom-offset)))
		 (xy2 (crd (+ bottom-offset
			      (* *soft*
				 0.5
				 (- half-down-y bottom-offset)))
			   top-offset))
		 (xy3 (crd half-down-y top-offset)))
	    (rotation (xy0 xy1) (xy2 xy3))
	    (move-curve)))
	
	(probe-contours (right bottom) elevation
	  (let* ((bottom-offset (offset-bottom))
		 (right-offset (offset-right))
		 (xy0 (crd bottom-offset 0))
		 (xy1 (crd bottom-offset
			   (* 0.36 bottom-offset)))
		 (xy2 (crd right-offset
			   (* -0.36 right-offset)))
		 (xy3 (crd right-offset 0)))
	    (rotation (xy0 xy1) (xy2 xy3))
	    (move-curve)))
	
	(probe-contours (top left) elevation
	  (let* ((left-offset (offset-left))
		 (top-offset (offset-top))
		 (xy0 (crd half-down-y left-offset))
		 (xy1 (crd (- half-down-y
			      (* 0.67 ;?
				 (- half-r
				    left-offset)))
			   left-offset))
		 (xy2 (crd (+ half-down-y
			      (* 0.67 top-offset))
			   top-offset))
		 (xy3 (crd half-down-y top-offset)))
	    (rotation (xy0 xy1) (xy2 xy3))
	    (move-curve)))

	(probe-contours (right left) elevation
	  (let* ((left-offset (offset-left))
		 (right-offset (offset-right))
		 (xy0 (crd half-down-y left-offset))
		 (xy1 (crd (- half-down-y
			      (* *soft*
				 0.5
				 right-offset))
			   left-offset))
		 (xy2 (crd right-offset
			   (* -0.36 right-offset)))
		 (xy3 (crd right-offset 0)))
	    (rotation (xy0 xy1) (xy2 xy3))
	    (move-curve)))

	(probe-contours (right top) elevation
	  (let* ((top-offset (offset-top))
		 (right-offset (offset-right))
		 (xy0 (crd half-down-y top-offset))
		 (xy1 (crd
		       (+ right-offset
			  (* *soft*
			     (- half-down-y
				right-offset)))
		       top-offset))
		 (xy2 (crd right-offset
			   (* *soft* top-offset)))
		 (xy3 (crd right-offset 0)))
	    (rotation () (xy0 xy1 xy2 xy3))
	    (move-curve)))
	
	(probe-contours (bottom top) elevation
	  (let* ((bottom-offset (offset-bottom))
		 (top-offset (offset-top))
		 (xy0 (crd bottom-offset 0))
		 (xy1 (crd bottom-offset
			   (* 0.36 bottom-offset)))
		 (xy2 (crd (+ bottom-offset
			      (* *soft*
				 0.5
				 (- half-down-y
				    bottom-offset)))
			   top-offset))
		 (xy3 (crd half-down-y top-offset)))
	    (rotation (xy0 xy1) (xy2 xy3))
	    (move-curve)))
	
	(probe-contours (bottom right) elevation
	  ;; same code as (right bottom)
	  (let* ((bottom-offset (offset-bottom))
		 (right-offset (offset-right))
		 (xy0 (crd bottom-offset 0))
		 (xy1 (crd bottom-offset
			   (* 0.36 bottom-offset)))
		 (xy2 (crd right-offset
			   (* -0.36 right-offset)))
		 (xy3 (crd right-offset 0)))
	    (rotation (xy0 xy1) (xy2 xy3))
	    (move-curve)))
	
	(probe-contours (left right) elevation
	  ;; same code as (right left)
	  (let* ((left-offset (offset-left))
		 (right-offset (offset-right))
		 (xy0 (crd half-down-y left-offset))
		 (xy1 (crd (- half-down-y
			      (* *soft*
				 0.5
				 right-offset))
			   left-offset))
		 (xy2 (crd right-offset
			   (* -0.36 right-offset)))
		 (xy3 (crd right-offset 0)))
	    (rotation (xy0 xy1) (xy2 xy3))
	    (move-curve)))

	(probe-contours (bottom top) elevation
	  (let* ((bottom-offset (offset-bottom))
		 (top-offset (offset-top))
		 (xy0
		   (crd bottom-offset
			0))
		 (xy1
		   (crd bottom-offset
			(* 0.36 bottom-offset)))
		 (xy2 (crd (+ bottom-offset
			      (* *soft*
				 0.5
				 (- half-down-y
				    bottom-offset)))
			   top-offset))
		 (xy3 (crd half-down-y
			   top-offset)))
	    (rotation (xy0 xy1) (xy2 xy3))
	    (move-curve)))
	
	))))
