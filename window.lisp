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
	    #|
	    (let* ((cairo-surface
		     (cairo:create-image-surface-for-data
		      buffer :argb32 1000 800 (* 4 1000)))
		   (cairo-context (cairo:create-context cairo-surface)))
	      (cairo:with-context (cairo-context)
		(cairo:set-source-rgb 0 0 0)
		(cairo:paint)
		(cairo:set-source-rgb 0 1 0)
		(cairo:set-line-width 3)
		(cairo:move-to 0 0)
		(cairo:line-to 1000 800)
		;(cairo:stroke)
		(cairo:set-line-width 2)
		(cairo:line-to 500 200)
		;(cairo:stroke)
		(cairo:set-line-width 1)
		(cairo:line-to 0 0)
		;(cairo:stroke)
		(cairo:set-source-rgb 1 0 0)
		(cairo:set-line-width 1)
		(cairo:arc 0 0 30 0 (* 2 pi))
		(cairo:stroke)
		)
	      (cairo:destroy cairo-context)
	      (cairo:destroy cairo-surface))|#

	    (let ((test-state
		    (make-instance 'view-state
				   :texture texture
				   :buffer buffer)))

	      (dotimes (x 5)
		(dotimes (y 5)
		  (draw-hex (crd x y) test-state)))



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
				    (dotimes (x 5)
				      (dotimes (y 5)
					(draw-hex (crd x y) test-state)))
				    
				    ))

		(:mousewheel (:y roll) ; 1 = away, -1 inwards, todo: test with non smooth wheel
			     (incf (hex-r test-state) roll) ;; todo: focus on centre or mouse

			     (clear-all test-state)
				    (dotimes (x 5)
				      (dotimes (y 5)
					(draw-hex (crd x y) test-state)))
			     
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

(defun draw-hex (crd view-state)
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
