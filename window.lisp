(in-package :hexhammer)

(defun render (world view-state)
  (do-visible (x y view-state)
    (when (hex-at (crd x y) world)
      (draw-terrain (crd x y) world view-state)
      (draw-gouraud-shading (crd x y) world view-state)
      (draw-contours (crd x y) world view-state)
      (draw-hex-borders (crd x y) view-state)))
  
  (do-visible (x y view-state)
    (draw-rivers (crd x y) world view-state)))

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
		    (apply #'make-instance
			   'view-state
			   :texture texture
			   :buffer buffer
			   *previous-view-state*))
		  (test-world
		    (progn
		      (defvar *world* (make-world))
		      (when (null *world*)
			(setf *world* (make-world)))
		      (when (zerop (hash-table-count (world-map *world*)))
			(generate-map 100 100
				      ;;"mhmap.pgm" ;; mini mars
				      "getty.pgm" ;; huge gettysburg
				      *world*))
		      *world*)))
	      
	      (do-visible (x y test-state)
		(when (hex-at (crd x y) test-world)
		  (draw-contours (crd x y) test-world test-state)
		  (draw-hex-borders (crd x y) test-state)))



	      (sdl2:with-event-loop (:method :poll)

		(:mousebuttondown (:x x :y y :button button)
				  (format t "x=~a ; y=~a ; but: ~a~%" x y button)
				  (cond
				    ((= button 3)
				     (setf (centre-x test-state) (+ (centre-x test-state)
								    (- x (/ (width test-state) 2)))
					   (centre-y test-state) (- (centre-y test-state)
								    (- y (/ (height test-state) 2))))
				     (format t "centre: (~a;~a)~%"
					     (centre-x test-state)
					    (centre-y test-state))
				     (clear-all test-state)
				     (time
				      (render test-world test-state)))
				    ((= button 1)
				     (format t "That's hex ~a, clicked with ~a~%"
					     (hex-xy-vert-at-pix x y test-state)
					     button)
				     (destructuring-bind (crd . vert)
					 (hex-xy-vert-at-pix x y test-state)
				       (cond ((null vert)
					      nil) ;; abort
					     ((sdl2:keyboard-state-p 225) ;; left shift
					      (when vert
						(elevate-vert crd vert 1 test-world)
						(clear-all test-state)
						(render test-world test-state)))
					     ((sdl2:keyboard-state-p 224) ;; left ctrl
					      (when vert
						(depress-vert crd vert 1 test-world)
						(clear-all test-state)
						(render test-world test-state)))
					     ((sdl2:keyboard-state-p 4) ;; A key
					      (increase-water-level crd vert test-world)
					      (clear-all test-state)
					      (render test-world test-state)
					      )
					     ((sdl2:keyboard-state-p 29) ;; Z key
					      (increase-water-level crd vert test-world -1)
					      (clear-all test-state)
					      (render test-world test-state)
					      )
					     ((sdl2:keyboard-state-p 22) ;; S key
					      ;; add one depth of water
					      (incf
					       (terrain-depth
						(change-terrain (hex-vertex (hex-at crd test-world)
									    vert)
								:base 'lake)))
					      (clear-all test-state)
					      (render test-world test-state)
					      )
					     ((sdl2:keyboard-state-p 27) ;; X key
					      ;; reduce one depth of water
					      (let* ((point (hex-vertex (hex-at crd test-world)
									vert))
						     (ter (point-terrain point)))
						(cond ((> (terrain-depth ter) 1)
						       (decf (terrain-depth ter)))
						      ((= (terrain-depth ter) 1)
						       (decf (terrain-depth ter))
						       (change-terrain point :base 'cultivated))))
					      (clear-all test-state)
					      (render test-world test-state)
					      ))))
				    (t
				     (format t "~a~%" button))
				    ))
		
		(:mousewheel (:y roll) ; 1 = away, -1 inwards, todo: test with non smooth wheel
			     (incf (hex-r test-state) (* 10 roll))

			     (clear-all test-state)
			     (do-visible (x y test-state)
			       (when (hex-at (crd x y) test-world)
				 (draw-contours (crd x y) test-world test-state)
				 (draw-hex-borders (crd x y) test-state))))

		(:keydown (:keysym keysym)
			  ;;(format t "~a~%" (sdl2:scancode-value keysym))
			  (let ((scancode (sdl2:scancode-value keysym)))
			    (format t "That button is keysym: ~a, scancode ~a~%"
				    keysym scancode)
			    (when (sdl2:scancode= scancode :scancode-q)
			      ;; Rotate light source around map
			      (defvar *vector-wall* (crd 0.2 1))
			      (setf *light-vector*
				    (surface-normal (crd 0 0) 0
						    (nrotate *vector-wall* (* 0.1 +sf-pi+)) 0
						    *vector-wall* 1))
			      (clear-all test-state)

			      (render test-world test-state)
			      
			      (sdl2:update-texture texture nil
						   buffer
						   (* 4 1000)) ; ARGB8888 size * texture width
			      (sdl2:render-clear renderer)
			      (sdl2:render-copy renderer texture)
			      (sdl2:render-present renderer)
			      )))
		
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

(defun clear-all (view-state)
  (let* ((cairo-surface
	   (cairo:create-image-surface-for-data
	    (buffer view-state) :argb32
	    (width view-state) (height view-state)
	    (* 4 (width view-state))))
	 (cairo-context (cairo:create-context cairo-surface)))
    (cairo:with-context (cairo-context)
      ;; Default background color testing
      (cairo:set-source-rgb 0.08 0.5 0.02)
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
	    (cairo:set-source-rgb 0.1 0.1 0.1))
	(cairo:set-line-width 0.25)
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

(defun rotate (crd angle
	       &optional
		 (sin (sin angle)) (cos (cos angle))
		 (x+ 0) (y+ 0))
  "Rotates CRD around origin using up is down coordinates."
  (crd (+ (* cos (x crd))
	  (* sin (y crd))
	  x+)
       (+ (- (* cos (y crd))
	     (* sin (x crd)))
	  y+)))

(defun nrotate (crd angle
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

(defun ntranslate (crd x+ y+)
  (incf (x crd) x+)
  (incf (y crd) y+)
  crd)

(defun translate (crd x+ y+)
  (crd (+ x+ (x crd))
       (+ y+ (y crd))))
