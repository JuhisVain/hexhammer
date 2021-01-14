(in-package :hexhammer)

(defparameter *soft* 0.9)
(declaim (single-float *soft*))

(defun draw-contours (crd map view-state)
  (let ((hex (hex-at crd map)))
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
			collect `(nrotate ,symbol ()
					  sin cos
					  hex-centre-x hex-centre-y))
		    ,@(loop
			for symbol in top-rights
			collect `(nrotate ,symbol ()
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
	    (move-curve)))))))
