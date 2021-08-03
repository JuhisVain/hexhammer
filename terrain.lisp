(in-package :hexhammer)

(defun record-terrain-border (hex left right)
  (list (point-terrain (hex-vertex hex left))
	(point-terrain (hex-vertex hex right))))

;;; TODO: repurpose contour routines for this.
(defun draw-terrain (crd map view-state)
  (let ((hex (hex-at crd map)))
    (unless hex (return-from draw-terrain))
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
	     ;;(contour-step (contour-step view-state))
	     
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

	(let ((top (record-terrain-border hex :n :nnw))
	      (left (record-terrain-border hex :nnw :nw))
	      (bottom (record-terrain-border hex :nw :cen))
	      (right (record-terrain-border hex :cen :n))
	      (angle (* 5/6 +sf-pi+)))
	  (draw-terrain-borders top left bottom right
				angle hex-centre-x hex-centre-y
				(hex-r view-state) cairo-context))
	;;rest
	)
      (cairo:destroy cairo-context)
      (cairo:destroy cairo-surface))))

'(defun draw-terrain-borders (top left bottom right
			     angle hex-centre-x hex-centre-y
			     hex-radius cairo-context)
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
	       )

      (cairo:with-context (cairo-context)
	(cairo:set-source-rgb 0.5 0.5 0.5)
	(cairo:set-line-width 0.5)
	(cairo:set-antialias :none)

	(prog1 nil
	  (dolist (borders (list top left bottom right))
	    (let* ((left (car borders))
		   (right (cadr borders))
		   (left-base (list (car left) (car left)))
		   (right-base (list (car right) (car right)))
		   )
	      (if (equal left-base right-base)
		  ))


	    ))
      
      ))))
