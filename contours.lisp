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

(declaim (inline offset-bottom offset-left offset-right offset-top)
	 (ftype (function (elevation contours single-float) single-float)
		offset-bottom offset-left offset-right offset-top))

(defun offset-bottom (elevation contours hex-r)
  (let ((half-down-y (* +sin60+ hex-r)))
    (- half-down-y
       (contour-offset (contour-index elevation contours)
		       (contours-range contours)
		       half-down-y))))

(defun offset-left (elevation contours hex-r)
  (let ((half-r (/ hex-r 2.0)))
    (- half-r
       (contour-offset (contour-index elevation contours)
		       (contours-range contours)
		       half-r))))

(defun offset-right (elevation contours hex-r)
  (let ((half-down-y (* +sin60+ hex-r)))
    (contour-offset (contour-index elevation contours)
		    (contours-range contours)
		    half-down-y)))

(defun offset-top (elevation contours hex-r)
  (let ((minus-half-r (/ hex-r -2.0)))
    (contour-offset (contour-index elevation contours)
		    (contours-range contours)
		    minus-half-r)))

(defun draw-kite-contours (top left bottom right
			   angle hex-centre-x hex-centre-y
			   hex-radius cairo-context)
  (declare ;(optimize speed)
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
	 (half-r (/ hex-radius 2.0))

	 (l-b-corner
	   (nrotate (crd half-down-y 0.0) () sin cos
		    hex-centre-x hex-centre-y))
	 (b-r-corner
	   (nrotate (crd 0.0 0.0) () sin cos
		    hex-centre-x hex-centre-y))
	 (t-l-corner
	   (nrotate (crd half-down-y half-r) () sin cos
		    hex-centre-x hex-centre-y))
	 (r-t-corner
	   (nrotate (crd half-down-y 0.0) () sin-d cos-d
		    hex-centre-x hex-centre-y)))

    (macrolet ((rotation (bot-lefts top-rights)
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
		    (x xy3) (y xy3))))

	       (fill-kite-water ((left right) (other1 other2)
				 right-high-corners
				 left-high-corners
				 &body body)
		 `(block water-filler
		    ,(cond
		       ;; Take care of the isthmus/straits special case:
		       ((and (eq left 'left)
			     (eq right 'bottom))
			`(when (and (< (contours-max ,left) (1+ (contours-water ,left)))
				    (< (contours-max ,right) (1+ (contours-water ,right))))
			   (cairo:move-to (x t-l-corner)
					  (y t-l-corner))
			   (cairo:line-to (x l-b-corner)
					  (y l-b-corner))
			   (cairo:line-to (x b-r-corner)
					  (y b-r-corner))
			   (when (and ;(< (contours-max ,other1) (1+ (contours-water ,other1)))
				      (< (contours-max ,other2) (1+ (contours-water ,other2))))
			     (cairo:line-to (x r-t-corner)
					    (y r-t-corner)))
			   (cairo:set-source-rgb 0.2 0.3 1.0)
			   (cairo:close-path)
			   (cairo:fill-path)
			   (return-from water-filler t)))
		       ;; Other side of previous
		       ((and (eq left 'right)
			     (eq right 'top))
			`(when (and (< (contours-max ,left) (1+ (contours-water ,left)))
				    (< (contours-max ,right) (1+ (contours-water ,right))))
			   (cairo:move-to (x b-r-corner)
					  (y b-r-corner))
			   (cairo:line-to (x r-t-corner)
					  (y r-t-corner))
			   (cairo:line-to (x t-l-corner)
					  (y t-l-corner))
			   
			   (when (and ;(< (contours-max ,other1) (1+ (contours-water ,other1)))
				      (< (contours-max ,other2) (1+ (contours-water ,other2))))
			     (cairo:line-to (x l-b-corner)
					    (y l-b-corner)))
			   (cairo:set-source-rgb 0.2 0.3 1.0)
			   (cairo:close-path)
			   (cairo:fill-path)
			   (return-from water-filler t)))
		       ;; Kite totally submerged		       
		       (t `(when (and (< (contours-max ,left) (1+ (contours-water ,left)))
				      ;(< (contours-max ,right) (1+ (contours-water ,right)))
				      ;(< (contours-max ,other1) (1+ (contours-water ,other1)))
				      (< (contours-max ,other2) (1+ (contours-water ,other2))))
			     (cairo:move-to (x b-r-corner)
					    (y b-r-corner))
			     (cairo:line-to (x r-t-corner)
					    (y r-t-corner))
			     (cairo:line-to (x t-l-corner)
					    (y t-l-corner))
			     (cairo:line-to (x l-b-corner)
					    (y l-b-corner))
			     (cairo:set-source-rgb 0.2 0.3 1.0)
			     (cairo:close-path)
			     (cairo:fill-path)
			     (return-from water-filler t))))

		    
		    (let ((right-surf (surface-level ,right))
			  (left-surf (surface-level ,left)))
		      (when (and right-surf (eql right-surf left-surf))
			(cairo:set-source-rgb 0.2 0.3 1.0)
			,@body
			(cond ((plusp (contours-range ,right))
			       ,@(mapcar
				  #'(lambda (corner)
				      `(cairo:line-to (x ,corner)
						      (y ,corner)))
				  right-high-corners))
			      (t
			       ,@(mapcar
				  #'(lambda (corner)
				      `(cairo:line-to (x ,corner)
						      (y ,corner)))
				  left-high-corners)))
			(cairo:close-path)
			(cairo:fill-path)
			t))))
	       )
      
      (cairo:with-context (cairo-context)
	(cairo:set-source-rgb 0.5 0.5 0.5)
	(cairo:set-line-width 0.5)
	
	(prog1 nil
	  (dolist (contours (list top left bottom right))
	    (set-all-contours contours))

	  (or
	   (fill-kite-water (left bottom) (top right)
			    (l-b-corner)
			    (t-l-corner b-r-corner)
			    (let* ((surface (1+ (surface-level bottom)))
				   (bottom-offset (offset-bottom surface bottom hex-radius))
				   (left-offset (offset-left surface left hex-radius))
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

	   (fill-kite-water (top bottom) (left right)
			    (t-l-corner l-b-corner)
			    (r-t-corner b-r-corner)
			    (let* ((surface (1+ (surface-level bottom)))
				   (bottom-offset (offset-bottom surface bottom hex-radius))
				   (top-offset (offset-top surface top hex-radius))
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

	   (fill-kite-water (right bottom) (top left)
			    (r-t-corner t-l-corner l-b-corner)
			    (b-r-corner)
			    (let* ((surface (1+ (surface-level bottom)))
				   (bottom-offset (offset-bottom surface bottom hex-radius))
				   (right-offset (offset-right surface right hex-radius))
				   (xy0 (crd bottom-offset 0))
				   (xy1 (crd bottom-offset
					     (* 0.36 bottom-offset)))
				   (xy2 (crd right-offset
					     (* -0.36 right-offset)))
				   (xy3 (crd right-offset 0)))
			      (rotation (xy0 xy1) (xy2 xy3))
			      (move-curve)))
	  
	   (fill-kite-water (top left) (right bottom)
			    (t-l-corner)
			    (r-t-corner b-r-corner l-b-corner)
			    (let* ((surface (1+ (surface-level left)))
				   (left-offset (offset-left surface left hex-radius))
				   (top-offset (offset-top surface top hex-radius))
				   (xy0 (crd half-down-y left-offset))
				   (xy1 (crd (- half-down-y
						(* 0.67
						   (- half-r
						      left-offset)))
					     left-offset))
				   (xy2 (crd (+ half-down-y
						(* 0.67 top-offset))
					     top-offset))
				   (xy3 (crd half-down-y top-offset)))
			      (rotation (xy0 xy1) (xy2 xy3))
			      (move-curve)))

	   (fill-kite-water (right left) (top bottom)
			    (r-t-corner t-l-corner)
			    (b-r-corner l-b-corner)
			    (let* ((surface (1+ (surface-level left)))
				   (left-offset (offset-left surface left hex-radius))
				   (right-offset (offset-right surface right hex-radius))
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
			      (move-curve))))

	  ;; NOTE: This last one must be outside the OR daisy chain
	  (fill-kite-water (right top) (left bottom)
			   (r-t-corner)
			   (b-r-corner t-l-corner)
			   (let* ((surface (1+ (surface-level top)))
				  (top-offset (offset-top surface top hex-radius))
				  (right-offset (offset-right surface right hex-radius))
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

	  
	  
	  (probe-contours (left bottom) elevation
	    (let* ((bottom-offset (offset-bottom elevation bottom hex-radius))
		   (left-offset (offset-left elevation left hex-radius))
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
	    (let* ((bottom-offset (offset-bottom elevation bottom hex-radius))
		   (top-offset (offset-top elevation top hex-radius))
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
	    (let* ((bottom-offset (offset-bottom elevation bottom hex-radius))
		   (right-offset (offset-right elevation right hex-radius))
		   (xy0 (crd bottom-offset 0))
		   (xy1 (crd bottom-offset
			     (* 0.36 bottom-offset)))
		   (xy2 (crd right-offset
			     (* -0.36 right-offset)))
		   (xy3 (crd right-offset 0)))
	      (rotation (xy0 xy1) (xy2 xy3))
	      (move-curve)))

	  (probe-contours (top left) elevation
	    (let* ((left-offset (offset-left elevation left hex-radius))
		   (top-offset (offset-top elevation top hex-radius))
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
	    (let* ((left-offset (offset-left elevation left hex-radius))
		   (right-offset (offset-right elevation right hex-radius))
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
	    (let* ((top-offset (offset-top elevation top hex-radius))
		   (right-offset (offset-right elevation right hex-radius))
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

	  ;;(bottom top)
	  ;;(t-l-corner l-b-corner b-r-corner)
	  ;;(r-t-corner)
	  (probe-contours (bottom top) elevation
	    (let* ((bottom-offset (offset-bottom elevation bottom hex-radius))
		   (top-offset (offset-top elevation top hex-radius))
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

	  ;;(bottom right)
	  ;;(b-r-corner)
	  ;;(r-t-corner t-l-corner l-b-corner)
	  (probe-contours (bottom right) elevation
	    ;; same code as (right bottom)
	    (let* ((bottom-offset (offset-bottom elevation bottom hex-radius))
		   (right-offset (offset-right elevation right hex-radius))
		   (xy0 (crd bottom-offset 0))
		   (xy1 (crd bottom-offset
			     (* 0.36 bottom-offset)))
		   (xy2 (crd right-offset
			     (* -0.36 right-offset)))
		   (xy3 (crd right-offset 0)))
	      (rotation (xy0 xy1) (xy2 xy3))
	      (move-curve)))

	  ;;(left right)
	  ;;(b-r-corner l-b-corner)
	  ;;(r-t-corner t-l-corner)
	  (probe-contours (left right) elevation
	    ;; same code as (right left)
	    (let* ((left-offset (offset-left elevation left hex-radius))
		   (right-offset (offset-right elevation right hex-radius))
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
	    (let* ((bottom-offset (offset-bottom elevation bottom hex-radius))
		   (top-offset (offset-top elevation top hex-radius))
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
	  )
	
	
	))))
