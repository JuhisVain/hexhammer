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
					 ;;(draw-shading (crd x y) test-world test-state)
					 (draw-gouraud-shading (crd x y) test-world test-state)
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

(defparameter *soft* 0.9)
(declaim (single-float *soft*))
(defconstant +cos30+ (coerce (cos (/ pi 6)) 'single-float))
(defconstant +sin30+ (coerce (sin (/ pi 6)) 'single-float))
(defconstant +sf-pi+ (coerce pi 'single-float))

;;should be used with normie coordinate space:
;; X is east, Y is north, Z is up
(defun surface-normal (point-0 ele-0 point-1 ele-1 point-2 ele-2)
  (let ((vector-01 (list (- (x point-1) (x point-0))
			 (- (y point-1) (y point-0))
			 (- ele-1 ele-0)))
	(vector-02 (list (- (x point-2) (x point-0))
			 (- (y point-2) (y point-0))
			 (- ele-2 ele-0))))
    (list (- (* (second vector-01)
		(third vector-02))
	     (* (third vector-01)
		(second vector-02)))
	  (- (* (third vector-01)
		(first vector-02))
	     (* (first vector-01)
		(third vector-02)))
	  (- (* (first vector-01)
		(second vector-02))
	     (* (second vector-01)
		(first vector-02))))))

(defun form-normal (dir-a dir-b hex)
  (surface-normal (unit-hex-crd :cen) (hex-vertex hex :cen)
		  (unit-hex-crd dir-a) (hex-vertex hex dir-a)
		  (unit-hex-crd dir-b) (hex-vertex hex dir-b)))

(defparameter *light-vector* (surface-normal (crd 0 0) 0 (crd 0.2 1) 0 (crd 0.2 1) 1))

(defun vector-angle (vector-1 vector-2)
  (acos (/ (+ (* (first vector-1) (first vector-2))
	      (* (second vector-1) (second vector-2))
	      (* (third vector-1) (third vector-2)))
	   (* (sqrt (+ (expt (first vector-1) 2)
		       (expt (second vector-1) 2)
		       (expt (third vector-1) 2)))
	      (sqrt (+ (expt (first vector-2) 2)
		       (expt (second vector-2) 2)
		       (expt (third vector-2) 2)))))))

(defun vertex-normal (normals)
  (let ((count (length normals)))
    (loop for (x y z) in normals
	  sum x into x-sum
	  sum y into y-sum
	  sum z into z-sum
	  finally (return (list (/ x-sum count)
				(/ y-sum count)
				(/ z-sum count))))))

(defmacro dir-list (start count)
  (let* ((dir-list (list :n :nnw :nw :w :sw :ssw :s :sse :se :e :ne :nne))
	 (start-list (member start dir-list)))
    (when (null start-list) (error "There is no direction ~a!~%" start))
    (rplacd (last dir-list) dir-list)
    `(list ,@(loop repeat count for dir in start-list collect dir))))

(defun vertex-triangle-normals (crd dir world)
  (macrolet ((list-tris (dira dirb count hex)
	       (let ((a (gensym "dira"))
		     (b (gensym "dirb")))
		 `(when ,hex
		    (mapcar #'(lambda (,a ,b)
				(form-normal ,a ,b ,hex))
			    (dir-list ,dira ,count)
			    (dir-list ,dirb ,count))))))
    (let ((hex (hex-at crd world)))
      (case dir
	(:CEN (list-tris :s :sse 12 hex))
	(:N (let ((n-hex (hex-at (crd-neighbour crd :n) world)))
	      (append (list-tris :nne :n 2 hex)
		      (list-tris :ssw :s 2 n-hex))))
	(:NNW (let ((n-hex (hex-at (crd-neighbour crd :n) world))
		    (nw-hex (hex-at (crd-neighbour crd :nw) world)))
		(append (list-tris :n :nnw 2 hex)
			(list-tris :sw :ssw 2 n-hex)
			(list-tris :se :e 2 nw-hex))))
	(:NW (let ((nw-hex (hex-at (crd-neighbour crd :nw) world)))
	       (append (list-tris :nnw :nw 2 hex)
		       (list-tris :sse :se 2 nw-hex))))
	(:W (let ((nw-hex (hex-at (crd-neighbour crd :nw) world))
		  (sw-hex (hex-at (crd-neighbour crd :sw) world)))
	      (append (list-tris :nw :w 2 hex)
		      (list-tris :s :sse 2 nw-hex)
		      (list-tris :ne :nne 2 sw-hex))))
	(:SW (let ((sw-hex (hex-at (crd-neighbour crd :sw) world)))
	       (append (list-tris :w :sw 2 hex)
		       (list-tris :e :ne 2 sw-hex))))
	(:SSW (let ((sw-hex (hex-at (crd-neighbour crd :sw) world))
		    (s-hex (hex-at (crd-neighbour crd :s) world)))
		(append (list-tris :sw :ssw 2 hex)
			(list-tris :se :e 2 sw-hex)
			(list-tris :n :nnw 2 s-hex))))
	(:S (let ((s-hex (hex-at (crd-neighbour crd :s) world)))
	      (append (list-tris :ssw :s 2 hex)
		      (list-tris :nne :n 2 s-hex))))
	(:SSE (let ((s-hex (hex-at (crd-neighbour crd :s) world))
		    (se-hex (hex-at (crd-neighbour crd :se) world)))
		(append (list-tris :s :sse 2 hex)
			(list-tris :ne :nne 2 s-hex)
			(list-tris :nw :w 2 se-hex))))
	(:SE (let ((se-hex (hex-at (crd-neighbour crd :se) world)))
	       (append (list-tris :sse :se 2 hex)
		       (list-tris :nnw :nw 2 se-hex))))
	(:E (let ((se-hex (hex-at (crd-neighbour crd :se) world))
		  (ne-hex (hex-at (crd-neighbour crd :ne) world)))
	      (append (list-tris :se :e 2 hex)
		      (list-tris :n :nnw 2 se-hex)
		      (list-tris :sw :ssw 2 ne-hex))))
	(:NE (let ((ne-hex (hex-at (crd-neighbour crd :ne) world)))
	       (append (list-tris :e :ne 2 hex)
		       (list-tris :w :sw 2 ne-hex))))
	(:NNE (let ((ne-hex (hex-at (crd-neighbour crd :ne) world))
		    (n-hex (hex-at (crd-neighbour crd :n) world)))
		(append (list-tris :ne :nne 2 hex)
			(list-tris :nw :w 2 ne-hex)
			(list-tris :s :sse 2 n-hex))))))))

(defun light-value (vector)
  (/ (vector-angle *light-vector* vector)
     +sf-pi+))

(defun vertex-light-value (crd dir world &optional (scaler 1.0))
  (* scaler
     (light-value
      (vertex-normal
       (vertex-triangle-normals crd dir world)))))

(defun draw-gouraud-shading (crd world view-state)
  (let ((hex (gethash crd (world-map world))))
    (unless hex (return-from draw-gouraud-shading))
    (let* ((cen (vertex-light-value crd :cen world))
	   (n (vertex-light-value crd :n world))
	   (nnw (vertex-light-value crd :nnw world))
	   (nw (vertex-light-value crd :nw world))
	   (w (vertex-light-value crd :w world))
	   (sw (vertex-light-value crd :sw world))
	   (ssw (vertex-light-value crd :ssw world))
	   (s (vertex-light-value crd :s world))
	   (sse (vertex-light-value crd :sse world))
	   (se (vertex-light-value crd :se world))
	   (e (vertex-light-value crd :e world))
	   (ne (vertex-light-value crd :ne world))
	   (nne (vertex-light-value crd :nne world))

	   (cairo-surface
	     (cairo:create-image-surface-for-data
	      (buffer view-state) :argb32
	      (width view-state) (height view-state)
	      (* 4 (width view-state))))
	   (cairo-context (cairo:create-context cairo-surface))

	   (window-centre-x-pix (/ (width view-state) 2))
	   (window-centre-y-pix (/ (height view-state) 2))

	   (origin-x (- window-centre-x-pix (centre-x view-state)))
	   (origin-y (- window-centre-y-pix (centre-y view-state)))

	   (r (hex-r view-state))
	   (half-down-y (* +sin60+ r))
	   (quarter-down-y (/ half-down-y 2))
	   (full-down-y (* half-down-y 2))
	   (half-r (* 0.5 r))
	   (three-halfs-r (* 1.5 r))
	   (quarter-r (* 0.25 r))
	   (three-quarters-r (* 0.75 r))
	   (seven-eights-r (* 0.875 r))

	   (hex-centre-x (+ origin-x
			    r
			    (* (x crd) three-halfs-r)))
	   (hex-centre-y (+ (- window-centre-y-pix
			       (+ origin-y
				  (* (y crd) full-down-y)))
			    (- half-down-y)
			    window-centre-y-pix
			    (* -1 full-down-y)
			    (* (mod (1- (x crd)) 2)
			       half-down-y)))
	   )

      (macrolet ((triangle-bounds ((dira dirb) &body body)
		   ;;Right triangle where (origin)-(dirb) edge is hypothenuse
		   (let ((a-crd (unit-hex-crd dira))
			 (b-crd (unit-hex-crd dirb)))
		     `(progn
			(cairo:new-path)
			(cairo:move-to hex-centre-x hex-centre-y)
			(cairo:line-to (+ hex-centre-x (* ,(car a-crd) r))
				       (+ hex-centre-y (* -1 ,(cdr a-crd) r)))
			(cairo:line-to (+ hex-centre-x (* ,(car b-crd) r))
				       (+ hex-centre-y (* -1 ,(cdr b-crd) r)))
			(cairo:close-path)
			,@body
			;(cairo:fill-path)
			))))

	(cairo:with-context (cairo-context)
	  (cairo:set-line-width 0.0)
	  (cairo:set-antialias :none)
	  
	  (triangle-bounds
	   (:s :sse)
	   (draw-gouraud-counter-tri
	    cen s sse (* +sf-pi+ 3/6) ; #'rotate rotates clockwise 
	    r hex-centre-x hex-centre-y cairo-context))

	  (triangle-bounds
	   (:se :e)
	   (draw-gouraud-counter-tri
	    cen se e (* +sf-pi+ 1/6)
	    r hex-centre-x hex-centre-y cairo-context))

	  (triangle-bounds
	   (:ne :nne)
	   (draw-gouraud-counter-tri
	    cen ne nne (* +sf-pi+ -1/6)
	    r hex-centre-x hex-centre-y cairo-context))

	  (triangle-bounds
	   (:n :nnw)
	   (draw-gouraud-counter-tri
	    cen n nnw (* +sf-pi+ -3/6)
	    r hex-centre-x hex-centre-y cairo-context))

	  (triangle-bounds
	   (:nw :w)
	   (draw-gouraud-counter-tri
	    cen nw w (* +sf-pi+ -5/6)
	    r hex-centre-x hex-centre-y cairo-context))

	  (triangle-bounds
	   (:sw :ssw)
	   (draw-gouraud-counter-tri
	    cen sw ssw (* +sf-pi+ -7/6)
	    r hex-centre-x hex-centre-y cairo-context))
	  
	  )))))

(defun draw-gouraud-counter-tri (o-value a-value b-value rotation
				 r hex-centre-x hex-centre-y context)
  (let* ((0a (- a-value o-value)) ; edges' value differences
	 (ab (- b-value a-value))
	 (b0 (- o-value b-value))
	 (abs-0a (abs 0a))
	 (abs-ab (abs ab))
	 (abs-b0 (abs b0)))
    (cond ((= abs-0a abs-ab abs-b0)
	   (cairo:set-source-rgb o-value o-value o-value context)
	   (cairo:fill-path context))

	  ((= a-value b-value)
	   (let* ((o-xy (crd 0 0))
		  (target-xy (rotate (crd +sin60+ 0) rotation)) ; A point
		  (gradient (cairo:create-linear-pattern
			     (+ hex-centre-x (* r (x o-xy)))
			     (+ hex-centre-y (* r -1 (y o-xy)))
			     (+ hex-centre-x (* r (x target-xy)))
			     (+ hex-centre-y (* r -1 (y target-xy))))))
	     (cairo:pattern-add-color-stop-rgb
	      gradient 0.0 o-value o-value o-value)
	     (cairo:pattern-add-color-stop-rgb
	      gradient 1.0 a-value a-value a-value)
	     (cairo:set-source gradient context)
	     (cairo:fill-path context)
	     (cairo:destroy gradient)))

	  #|((= o-value a-value) ;; handled by (> abs-0a (min abs-ab abs-b0)) ???
	   (let* ((o-xy (crd 0 0))
		  (target-xy (rotate (crd 0 0.5) rotation))
		  (gradient (cairo:create-linear-pattern
			     (+ hex-centre-x (* r (x o-xy)))
			     (+ hex-centre-y (* r -1 (y o-xy)))
			     (+ hex-centre-x (* r (x target-xy)))
			     (+ hex-centre-y (* r -1 (y target-xy))))))
	     (cairo:pattern-add-color-stop-rgb
	      gradient 0.0 o-value o-value o-value)
	     (cairo:pattern-add-color-stop-rgb
	      gradient 1.0 b-value b-value b-value)
	     (cairo:set-source gradient context)
	     (cairo:fill-path context)
	     (cairo:destroy gradient)))|#

	  ;;((= o-value b-value)) Handled by (> abs-ab (min abs-0a abs-b0))
	  ;; Why does it handle it? I don't know
	  
	  ((or (> abs-0a (min abs-ab abs-b0))
	       (> abs-ab (min abs-0a abs-b0)));; Slope does not intersect AB, but results OK
	   ;; if edge from origin to A is greatest
	   ;; then draw line from mid-value corner to mid-value's offset
	   ;; on edge origin-A and compute it's function ax+by+c=0
	   ;; move the line function to pass through minimum vertex
	   ;; - in this case b=-1 and c=0
	   ;; Find the point on this line closest to maximum vertex
	   (let* ((x0 +sin60+)
		  (y0 0)
		  (k-mb (/ (- 0.5 0)
			   (- +sin60+
			      (* (/ (- b-value o-value)
				    (- a-value o-value))
				 +sin60+))))

		  (target-x (/ +sin60+
			       (1+ (* k-mb k-mb))))

		  (target-y (* target-x k-mb))
		  
		  (target-xy (rotate (crd target-x target-y)
				     rotation))
		  (a-xy (rotate (crd x0 y0)
				rotation))
		  (gradient (cairo:create-linear-pattern
			     (+ hex-centre-x (* r (x a-xy)))
			     (+ hex-centre-y (* r -1 (y a-xy)))
			     (+ hex-centre-x (* r (x target-xy)))
			     (+ hex-centre-y (* r -1 (y target-xy))))))
	     
	     (cairo:pattern-add-color-stop-rgb
	      gradient 0.0 a-value a-value a-value)
	     (cairo:pattern-add-color-stop-rgb
	      gradient 1.0 o-value o-value o-value)
	     (cairo:set-source gradient context)
	     (cairo:fill-path context)
#|
	     (when (= rotation (/ +sf-pi+ 2))
	       (format t "centre-x:~a centre-y:~a  r:~a~%--from (~a ; ~a)~%----to (~a;~a)~%"
		       hex-centre-x hex-centre-y r
		       (+ hex-centre-x (* r (x a-xy)))
		       (+ hex-centre-y (* r 1 (y a-xy)))
		       (+ hex-centre-x (* r (x target-xy)))
		       (+ hex-centre-y (* r 1 (y target-xy))))
	     |#
	     ;(cairo:set-source-rgb 0.1 0.1 0.1)
	     (cairo:new-path context)
	     ;(cairo:stroke context)
					;(cairo:set-antialias :default)
	     (cairo:set-source-rgb 1 0 0)
	     (cairo:set-line-width 1 context)
	     (cairo:move-to (+ hex-centre-x (* r (x a-xy)))
			    (+ hex-centre-y (* r -1 (y a-xy)))
			    context)
	     (cairo:line-to (+ hex-centre-x (* r (x target-xy)))
			    (+ hex-centre-y (* r -1 (y target-xy)))
			    context)
	     (cairo:line-to (+ hex-centre-x )
			    (+ hex-centre-y )
			    context)
	     (cairo:stroke context)
					;)
	     
	     
	     (cairo:destroy gradient)))
	  ((or (> abs-b0 (max abs-0a abs-ab))
	       (and (> abs-0a (max abs-ab abs-b0))
		    (progn (format t "I am not useless if I print~%")
			   t)))
	   ;;If point A is the mid point, can't start gradient from there.
	   (let* ((x0 0)
		  (y0 0)
		  (b-o (- b-value o-value))
		  (a-o (- a-value o-value))
		  (aobo-rat (/ a-o b-o))
		  (k-ma (/ (* aobo-rat -0.5)
			   (- +sin60+
			      (* +sin60+ aobo-rat))))
		  (c-ma (/ (* 0.5 aobo-rat)
			   (- 1 aobo-rat)))
		  (target-x (/ (* (- k-ma) (+ c-ma 0.5))
			       (+ (* k-ma k-ma) 1)))
		  (target-y (/ (- target-x) k-ma))
		  (target-xy (rotate (crd target-x target-y)
				     rotation))
		  (gradient (cairo:create-linear-pattern
			     (+ hex-centre-x x0)
			     (+ hex-centre-y y0)
			     (+ hex-centre-x (* r (x target-xy)))
			     (+ hex-centre-y (* r -1 (y target-xy))))))
	     
	     (cairo:pattern-add-color-stop-rgb
	      gradient 0.0 o-value o-value o-value)
	     (cairo:pattern-add-color-stop-rgb
	      gradient 1.0 b-value b-value b-value)
	     (cairo:set-source gradient context)
	     (cairo:fill-path context)

					;(cairo:set-source-rgb 0.1 0.1 0.1)
	     (cairo:new-path context)
					;(cairo:stroke context)
					;(cairo:set-antialias :default)
	     (cairo:set-source-rgb 1 0 0)
	     (cairo:set-line-width 1 context)
	     (cairo:move-to (+ hex-centre-x )
			    (+ hex-centre-y )
			    context)
	     (cairo:line-to (+ hex-centre-x (* r (x target-xy)))
			    (+ hex-centre-y (* r -1 (y target-xy)))
			    context)
	     (let ((b-crd (rotate (crd +sin60+ 0.5)
				  rotation)))
	       (cairo:line-to (+ hex-centre-x (* r (x b-crd)))
			      (+ hex-centre-y (* r -1 (y b-crd)))
			      context))
	     (cairo:stroke context)
					;)
	     
	     
	     (cairo:destroy gradient)
	     ))
	  
	  )))

(defun draw-shading (crd map view-state)
  (let ((hex (gethash crd (world-map map))))
    (unless hex (return-from draw-shading))

    (macrolet ((flat-half-kite (dira ; A vector of abstract surface
				dirb ; B vector of abstract surface
				)
		 (let* ((lightness
			  (gensym (concatenate
				   'string "light-cen-"
				   (string dira) "-" (string dirb))))
			(a-crd (unit-hex-crd dira))
			(b-crd (unit-hex-crd dirb)))
		   `(let ((,lightness
			    (/ (vector-angle
				*light-vector*
				(surface-normal ',(unit-hex-crd :CEN) (hex-vertex hex :cen)
					        ',a-crd (hex-vertex hex ,dira)
					        ',b-crd (hex-vertex hex ,dirb)))
			       +sf-pi+)))
		      ;; Needs height/hex-r scaling:
		      (cairo:set-source-rgb ,lightness ,lightness ,lightness)
		      (cairo:move-to hex-centre-x hex-centre-y)
		      (cairo:line-to (+ hex-centre-x (* ,(car a-crd) r))
				     (+ hex-centre-y (* -1 ,(cdr a-crd) r)))
		      (cairo:line-to (+ hex-centre-x (* ,(car b-crd) r))
				     (+ hex-centre-y (* -1 ,(cdr b-crd) r)))
		      (cairo:close-path)
		      ;;;; with antialias :none lines don't need to be stroked
		      ;;(cairo:set-line-width 0.5)
		      ;;(cairo:stroke-preserve)
		      (cairo:set-line-width 0.0)
		      (cairo:fill-path)))))
      
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
	       (half-down-y (* +sin60+ r))
	       (quarter-down-y (/ half-down-y 2))
	       (full-down-y (* half-down-y 2))
	       (half-r (* 0.5 r))
	       (three-halfs-r (* 1.5 r))
	       (quarter-r (* 0.25 r))
	       (three-quarters-r (* 0.75 r))
	       (seven-eights-r (* 0.875 r))

	       (hex-centre-x (+ origin-x
				r
				(* (x crd) three-halfs-r)))
	       (hex-centre-y (+ (- window-centre-y-pix
				   (+ origin-y
				      (* (y crd) full-down-y)))
				(- half-down-y)
				window-centre-y-pix
				(* -1 full-down-y)
				(* (mod (1- (x crd)) 2)
				   half-down-y))))

	  (cairo:with-context (cairo-context)
	    (cairo:set-line-width 0.0)
	    (cairo:set-antialias :none)

	    (flat-half-kite :e :ne)
	    (flat-half-kite :ne :nne)
	    (flat-half-kite :nne :n)
	    (flat-half-kite :n :nnw)
	    (flat-half-kite :nnw :nw)
	    (flat-half-kite :nw :w)
	    (flat-half-kite :w :sw)
	    (flat-half-kite :sw :ssw)
	    (flat-half-kite :ssw :s)
	    (flat-half-kite :s :sse)
	    (flat-half-kite :sse :se)
	    (flat-half-kite :se :e)))
	
	;; Wrap up
	(cairo:destroy cairo-context)
	(cairo:destroy cairo-surface)))))

(defun draw-contours (crd map view-state)
  (let ((hex (gethash crd (world-map map))))
    (unless hex (return-from draw-contours))
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
      (cairo:destroy cairo-surface))))

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
