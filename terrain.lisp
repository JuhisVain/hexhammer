(in-package :hexhammer)

(defun point-base-terrain (point)
  (car (point-terrain point)))

(defun record-terrain-border (hex left right)
  "Returns a !!!border!!! between two terrain types.
Forest at left and forest at right produces no border.
Forest at left and swamp at right produces (FOREST . SWAMP) border."
  (let ((left-point (point-base-terrain (hex-vertex hex left)))
	(right-point (point-base-terrain (hex-vertex hex right))))
    (when (not (equal left-point right-point)) ;; base terrain type WIP
      (cons left-point right-point))))

(defun point-edge-terrain (hex left right)
  "List terrains at vertices LEFT & RIGHT."
  (list (car (point-base-terrain (hex-vertex hex left)))
	(car (point-base-terrain (hex-vertex hex right)))))

(defun terrain-borderp (point-edge-terrain)
  "Returns T when terrain border found."
  (not (equal (car point-edge-terrain)
	      (cadr point-edge-terrain))))

;; Contours don't fit this problem
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

	(let ((top (point-edge-terrain hex :n :nnw))
	      (left (point-edge-terrain hex :nnw :nw))
	      (bottom (point-edge-terrain hex :nw :cen))
	      (right (point-edge-terrain hex :cen :n))
	      (angle (* 5/6 +sf-pi+)))
	  (draw-kite-terrain top left bottom right
			     angle hex-centre-x hex-centre-y
			     (hex-r view-state) cairo-context))
	;;rest
	)
      (cairo:destroy cairo-context)
      (cairo:destroy cairo-surface))))

;; Insert some test borders in there:
(defun test-terrain ()

  ;; Reset screw-ups:
  (dolist (v +vertex-directions+)
    (dolist (crd (list (crd 1 1)
		       (crd 3 2)
		       (crd 1 3)))
      (setf (point-terrain (hex-vertex (hex-at crd *world*) v))
	    (list (cons 'cultivated 'dry)))))
  
  (dolist (v (list :n :nw :sw :s :se :ne))
    (setf (point-terrain (hex-vertex (hex-at (crd 1 1) *world*) v))
	  (list (cons 'forest 'dry))))

  (dolist (v (list :nw :nnw :ne :e :s :ssw))
    (setf (point-terrain (hex-vertex (hex-at (crd 3 2) *world*) v))
	  (list (cons 'forest 'dry))))

  (dolist (v (list :nw :nnw :n :ne :e :se :s :ssw :sw))
    (setf (point-terrain (hex-vertex (hex-at (crd 1 3) *world*) v))
	  (list (cons 'forest 'dry)))))

(defun draw-kite-terrain (top left bottom right
			  angle hex-centre-x hex-centre-y
			  hex-radius cairo-context)
  (let* ((angle-d (+ angle
		     (/ +sf-pi+ -3)))
	 (sin-d (sin angle-d))
	 (cos-d (cos angle-d))
	 
	 (sin (sin angle))
	 (cos (cos angle))

	 (half-down-y (* +sin60+ hex-radius))
	 (half-kite-long (/ half-down-y 2.0)) ; half of a long edge of kite
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
	       
	       (path-through (&rest crds)
		 `(progn (cairo:move-to (x ,(car crds)) (y ,(car crds)))
			 ,@(loop for crd in (cdr crds)
				 collect `(cairo:line-to (x ,crd) (y ,crd)))
			 (cairo:close-path))))
      
      (cairo:with-context (cairo-context)
	(cairo:set-source-rgb 0.5 0.5 0.5)
	(cairo:set-line-width 0.5)
	(cairo:set-antialias :none)

	(when (and (equal (car bottom) (car right))
		   (equal (car bottom) (car top))
		   (equal (car bottom) (car left)))
	  ;;; Whole kite is of same terrain type
	  ;; TODO: draw fill
	  (return-from draw-kite-terrain))
        
	(when (terrain-borderp bottom)
	  
	  (let ((probe (car bottom)))
	    (cond ((and (terrain-borderp left)
			(eq probe (cadr left)))
		   ;; bottom-left corner
		   (let ((terrain-type (car bottom))
			 (xy0 (crd half-kite-long 0))
			 (xy1 (crd half-kite-long (/ half-r 2)))
			 (xyn (crd half-down-y (/ half-r 2))))
		     (rotation (xy0 xy1 xyn) ())

		     (path-through xy0 xy1 xyn l-b-corner)
		     
		     (case terrain-type
		       (forest (cairo:set-source-rgb 0.0 1.0 0.0))
		       (cultivated (cairo:set-source-rgb 1.0 0.0 0.0)))
		     
		     (cairo:fill-path)))
		  
		  ((and (terrain-borderp top)
			(eq probe (cadr top)))
		   ;; left side
		   (let ((terrain-type (car bottom))
			 (xy0 (crd half-kite-long 0))
			 (xy1 (crd half-kite-long (/ half-r 2)))
			 (xyn (crd half-down-y (/ half-r -2))))
		     (rotation (xy0 xy1) (xyn))

		     (path-through xy0 xy1 xyn t-l-corner l-b-corner)
		     
		     (case terrain-type
		       (forest (cairo:set-source-rgb 0.0 1.0 0.0))
		       (cultivated (cairo:set-source-rgb 1.0 0.0 0.0)))
		     
		     (cairo:fill-path)
		     ))
		  
		  ((and (terrain-borderp right)
			(eq probe (cadr right)))
		   ;; all except bottom right corner
		   (format t "left-side and top-right~%")

		   (let ((terrain-type (car bottom))
			 (xy0 (crd half-kite-long 0))
			 (xy1 (crd half-kite-long (/ half-r 2)))
			 (xy2 (crd half-kite-long 0)))
		     (rotation (xy0 xy1) (xy2))

		     (path-through xy0 xy1 xy2 r-t-corner t-l-corner l-b-corner)
		     
		     (case terrain-type
		       (forest (cairo:set-source-rgb 0.0 1.0 0.0))
		       (cultivated (cairo:set-source-rgb 1.0 0.0 0.0)))
		     
		     (cairo:fill-path))
		   
		   ))))

	;; rest
	
	))))
