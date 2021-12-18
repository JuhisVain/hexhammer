(in-package :hexhammer)

(defstruct (terrain
	    (:constructor
		make-terrain (&optional (base 'cultivated) (state 'dry) mod)))
  (base)
  (state)
  (mod))

(defun point-terrain-base (point)
  (terrain-base (point-terrain point)))
(defun point-terrain-state (point)
  (terrain-state (point-terrain point)))
(defun point-terrain-mod (point)
  (terrain-mod (point-terrain point)))

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

	(let ((top (hex-vertex hex :nnw))
	      (left (hex-vertex hex :nw))
	      (bottom (hex-vertex hex :cen))
	      (right (hex-vertex hex :n))
	      (angle (* 5/6 +sf-pi+)))
	  (draw-kite-terrain top left bottom right
			     angle hex-centre-x hex-centre-y
			     (hex-r view-state) cairo-context))

	(let ((top (hex-vertex hex :nne))
	      (left (hex-vertex hex :n))
	      (bottom (hex-vertex hex :cen))
	      (right (hex-vertex hex :ne))
	      (angle (* 3/6 +sf-pi+)))
	  (draw-kite-terrain top left bottom right
			     angle hex-centre-x hex-centre-y
			     (hex-r view-state) cairo-context))

	(let ((top (hex-vertex hex :e))
	      (left (hex-vertex hex :ne))
	      (bottom (hex-vertex hex :cen))
	      (right (hex-vertex hex :se))
	      (angle (* 1/6 +sf-pi+)))
	  (draw-kite-terrain top left bottom right
			     angle hex-centre-x hex-centre-y
			     (hex-r view-state) cairo-context))

	(let ((top (hex-vertex hex :sse))
	      (left (hex-vertex hex :se))
	      (bottom (hex-vertex hex :cen))
	      (right (hex-vertex hex :s))
	      (angle (* -1/6 +sf-pi+)))
	  (draw-kite-terrain top left bottom right
			     angle hex-centre-x hex-centre-y
			     (hex-r view-state) cairo-context))

	(let ((top (hex-vertex hex :ssw))
	      (left (hex-vertex hex :s))
	      (bottom (hex-vertex hex :cen))
	      (right (hex-vertex hex :sw))
	      (angle (* -3/6 +sf-pi+)))
	  (draw-kite-terrain top left bottom right
			     angle hex-centre-x hex-centre-y
			     (hex-r view-state) cairo-context))

	(let ((top (hex-vertex hex :w))
	      (left (hex-vertex hex :sw))
	      (bottom (hex-vertex hex :cen))
	      (right (hex-vertex hex :nw))
	      (angle (* -5/6 +sf-pi+)))
	  (draw-kite-terrain top left bottom right
			     angle hex-centre-x hex-centre-y
			     (hex-r view-state) cairo-context))
	
	)
      (cairo:destroy cairo-context)
      (cairo:destroy cairo-surface))))

(defmacro render-terrain-path-form (terrain-type with-lines without-lines)
  (let ((lined
	  `((cairo:move-to (x ,(car with-lines)) (y ,(car with-lines)))
	    ,@(loop for crd in (cdr with-lines)
		    collect `(cairo:line-to (x ,crd) (y ,crd)))))
	(lineless
	  `(,@(when (null with-lines)
	       `((cairo:move-to (x ,(car without-lines)) (y ,(car without-lines)))))
	    ,@(loop for crd in (if with-lines
				   without-lines
				   (cdr without-lines))
		    collect `(cairo:line-to (x ,crd) (y ,crd))))))
    
    `(let ((type ,terrain-type))
       ,@(when with-lines
	   lined)
       ,@(when without-lines
	   lineless)
       (set-terrain-fill type)
       (cairo:close-path)
       (cairo:fill-path)
       ,@(when with-lines
	   `(,@lined
	     (set-terrain-line type)
	     (cairo:stroke)
	     (cairo:set-antialias :none))))))

(defmacro rtp (terrain-type &rest control-points)
  `(case ,terrain-type
     ((FOREST CULTIVATED)
      (render-terrain-path-form ,terrain-type
				()
				,control-points))
     ((LAKE)
      (render-terrain-path-form ,terrain-type
				,(subseq control-points 0 3)
				,(subseq control-points 3)))))


(defun set-terrain-fill (terrain-type)
  (case terrain-type
    (forest (cairo:set-source-rgb 0.83 0.87 0.80))
    (swamp (cairo:set-source-rgb 0.3 0.7 0.80))
    (cultivated (cairo:set-source-rgb 0.96 0.95 0.94))
    (lake (cairo:set-source-rgb 0.67 0.86 0.95))))

(defun set-terrain-line (terrain-type)
  (cairo:set-antialias :default)
  (cairo:set-line-width 3.0)
  (case terrain-type
    (forest (cairo:set-source-rgb 1.0 0.0 1.0))
    (cultivated (cairo:set-source-rgb 1.0 0.0 1.0))
    (lake (cairo:set-source-rgb 0.2 0.58 0.69))))

(defun render-terrain-path (terrain-type)
  (case terrain-type
    (forest (cairo:set-source-rgb 0.83 0.87 0.80))
    (cultivated (cairo:set-source-rgb 0.96 0.95 0.94)))
  (cairo:fill-path))

'(defun test-rounded ()
  (setf (point-terrain (hex-vertex (hex-at (crd 2 2) *world*) :cen))
	(list (cons 'lake 'dry)))
  (dolist (v (list :n :nne :ne :se :s))
    (setf (point-terrain (hex-vertex (hex-at (crd 2 2) *world*) v))
	  (list (cons 'forest 'dry)))))

(defun terrain-naturalp (terrain)
  (or (eq terrain 'forest)
      (eq terrain 'swamp)))

(defun terrain-artificialp (terrain)
  (eq terrain 'cultivated))

(defun terrain-waterp (terrain)
  (eq terrain 'lake))

(defmacro lines (&rest lines)
  `(progn ,@(loop for crd in lines
		 collect `(cairo:line-to (x ,crd) (y ,crd)))))

(defmacro new-lines (start &rest lines)
  `(progn (cairo:move-to (x ,start) (y ,start))
	  (lines ,@lines)))

(defmacro curves (&rest crds)
  `(progn ,@(loop for (a b c) on crds by #'cdddr
		  if (and a b c)
		    collect `(cairo:curve-to (x ,a) (y ,a)
					     (x ,b) (y ,b)
					     (x ,c) (y ,c))
		  else do (error "Not enough arguments (~a ~a ~a)" a b c))))

(defmacro new-curves (start &rest crds)
  `(progn (cairo:move-to (x ,start) (y ,start))
	  (curves ,@crds)))

(defun terrain-depth (terrain)
  (or (cdr (assoc :depth (terrain-mod terrain)))
      0))

(defun (setf terrain-depth) (new-depth terrain)
  (cond ((not (plusp new-depth))
	 (setf (terrain-mod terrain) (delete :depth (terrain-mod terrain) :key #'car)))
	(t
	 (let ((depth-cons (assoc :depth (terrain-mod terrain))))
	   (if depth-cons
	       (setf (cdr depth-cons) new-depth)
	       (push (cons :depth new-depth) (terrain-mod terrain)))))))

(defun change-terrain(point &key
			      (base nil supplied-base)
			      (state nil supplied-state)
			      (mod nil supplied-mod))
  (unless point
    (return-from change-terrain))
  (let ((terrain (point-terrain point)))
    (when supplied-base
      (setf (terrain-base terrain) base)
      #|(when (terrain-waterp base)
	(unless (assoc :depth mod)
	  (incf (terrain-depth terrain))))|#
      )
    (when supplied-state
      (setf (terrain-state terrain) state))
    (when supplied-mod
      (setf (terrain-mod terrain) mod))
    terrain))

(defun more-test ()

  (dolist (v (list :nnw :nne :e))
    (setf (terrain-base (point-terrain (hex-vertex (hex-at (crd 3 2) *world*) v)))
	  'cultivated))
  (dolist (v (list :nw :n))
    (setf (terrain-base (point-terrain (hex-vertex (hex-at (crd 3 2) *world*) v)))
	  'forest))
  (dolist (v (list :cen ))
    (setf (terrain-base (point-terrain (hex-vertex (hex-at (crd 3 2) *world*) v)))
	  'swamp))

  (dolist (v (list :n :ne :se :s :sw :nw))
    (setf (terrain-base (point-terrain (hex-vertex (hex-at (crd 1 1) *world*) v)))
	  'lake))
  (dolist (v (list :cen :e :ssw :w))
    (setf (terrain-base (point-terrain (hex-vertex (hex-at (crd 1 1) *world*) v)))
	  'swamp))
  (dolist (v (list :nnw :nne :sse))
    (setf (terrain-base (point-terrain (hex-vertex (hex-at (crd 1 1) *world*) v)))
	  'forest))
  
  (setf (terrain-base (point-terrain (hex-vertex (hex-at (crd 1 3) *world*) :cen)))
	'lake)
  (dolist (v (list :nw :ne :nne :nnw :nnw :sw))
    (setf (terrain-base (point-terrain (hex-vertex (hex-at (crd 1 3) *world*) v)))
	  'forest))
  (dolist (v (list :n :e :w))
    (setf (terrain-base (point-terrain (hex-vertex (hex-at (crd 1 3) *world*) v)))
	  'swamp))

  (dolist (v (list :n))
    (setf (terrain-base (point-terrain (hex-vertex (hex-at (crd 3 0) *world*) v)))
	  'lake))
  (dolist (v (list :nne :cen))
    (setf (terrain-base (point-terrain (hex-vertex (hex-at (crd 3 0) *world*) v)))
	  'forest))
  (dolist (v (list :ne))
    (setf (terrain-base (point-terrain (hex-vertex (hex-at (crd 3 0) *world*) v)))
	  'swamp))

  (dolist (v (list :n))
    (setf (terrain-base (point-terrain (hex-vertex (hex-at (crd 5 0) *world*) v)))
	  'lake))
  (dolist (v (list :nne))
    (setf (terrain-base (point-terrain (hex-vertex (hex-at (crd 5 0) *world*) v)))
	  'forest))
  (dolist (v (list :cen :ne))
    (setf (terrain-base (point-terrain (hex-vertex (hex-at (crd 5 0) *world*) v)))
	  'swamp))

  (dolist (v (list :n :se :sw))
    (setf (terrain-base (point-terrain (hex-vertex (hex-at (crd 5 2) *world*) v)))
	  'lake))
  (dolist (v (list :cen :nw :e))
    (setf (terrain-base (point-terrain (hex-vertex (hex-at (crd 5 2) *world*) v)))
	  'forest))
  (dolist (v (list :nnw :ne :s :ssw))
    (setf (terrain-base (point-terrain (hex-vertex (hex-at (crd 5 2) *world*) v)))
	  'swamp)))

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
	 (quarter-r (/ half-r 2.0))
	 
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
		    hex-centre-x hex-centre-y))

	 ;; rotation later:
	 (kite-mid (crd half-kite-long (/ half-r 2)))
	 (b-mid (crd half-kite-long 0))
	 (l-mid (crd half-down-y quarter-r))
	 (t-mid (crd half-down-y (- quarter-r)))
	 (r-mid (crd half-kite-long 0))

	 (softness 0.6) ; for left & right corner curves

	 ;; from mid-short move perpendicular to opposing long:
	 (mid-short-at-long (/ quarter-r +cos30+))
	 (b-msal (crd mid-short-at-long 0))
	 (r-msal (crd mid-short-at-long 0))

	 ;; from mid-kite straight line to long edges:
	 (mid-kite-at-long (/ half-r +cos30+))
	 (b-mkal (crd mid-kite-at-long 0))
	 (r-mkal (crd mid-kite-at-long 0))

	 ;; First control point of curve from t-l-corner to a long-mid
	 ;; Might also be center point of straight from short-long corner to opposing
	 (top-to-l/r-cpoint (crd (* +cos30+ (* 0.75 hex-radius))
				 (* +sin30+ (* 0.75 hex-radius)))))
    
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
			 (cairo:close-path)))
	       (kite-perimeter (terrain)
		 `(progn
		    (set-terrain-fill ,terrain)
		    (new-lines t-l-corner l-b-corner b-r-corner r-t-corner)
		    (cairo:close-path)
		    (cairo:fill-path)))
	       (top-kite-perimeter (terrain)
		 `(progn
		    (set-terrain-fill ,terrain)
		    (new-lines l-b-corner r-t-corner t-l-corner)
		    (cairo:close-path)
		    (cairo:fill-path)))
	       (bottom-kite-perimeter (terrain)
		 `(progn
		    (set-terrain-fill ,terrain)
		    (new-lines l-b-corner r-t-corner b-r-corner)
		    (cairo:close-path)
		    (cairo:fill-path)))
	       (left-kite-perimeter (terrain)
		 `(progn
		    (set-terrain-fill ,terrain)
		    (new-lines t-l-corner l-b-corner b-r-corner)
		    (cairo:close-path)
		    (cairo:fill-path)))
	       (right-kite-perimeter (terrain)
		 `(progn
		    (set-terrain-fill ,terrain)
		    (new-lines t-l-corner b-r-corner r-t-corner)
		    (cairo:close-path)
		    (cairo:fill-path)))
	       (artificial-left (terrain)
		 `(progn
		    (set-terrain-fill ,terrain)
		    (new-lines l-mid l-b-corner b-mid kite-mid)
		    (cairo:close-path)
		    (cairo:fill-path)))
	       (artificial-right (terrain)
		 `(progn
		    (set-terrain-fill ,terrain)
		    (new-lines t-mid kite-mid r-mid r-t-corner)
		    (cairo:close-path)
		    (cairo:fill-path)))
	       (artificial-top (terrain)
		 `(progn
		    (set-terrain-fill ,terrain)
		    (new-lines l-mid kite-mid t-mid t-l-corner)
		    (cairo:close-path)
		    (cairo:fill-path)))
	       (artificial-bottom (terrain)
		 `(progn
		    (set-terrain-fill ,terrain)
		    (new-lines b-mid kite-mid r-mid b-r-corner)
		    (cairo:close-path)
		    (cairo:fill-path)))
	       (artificial-top-right (terrain)
		 `(progn
		    (set-terrain-fill ,terrain)
		    (new-lines l-mid kite-mid r-mid r-t-corner t-l-corner)
		    (cairo:close-path)
		    (cairo:fill-path)))
	       (artificial-top-left (terrain)
		 `(progn
		    (set-terrain-fill ,terrain)
		    (new-lines b-mid kite-mid t-mid t-l-corner l-b-corner)
		    (cairo:close-path)
		    (cairo:fill-path)))
	       (artificial-bottom-right (terrain)
		 `(progn
		    (set-terrain-fill ,terrain)
		    (new-lines b-mid kite-mid t-mid r-t-corner b-r-corner)
		    (cairo:close-path)
		    (cairo:fill-path)))
	       (left-artificial-tmid-bottom (terrain)
		 `(progn
		    (set-terrain-fill ,terrain)
		    (new-lines t-mid kite-mid b-r-corner r-t-corner)
		    (cairo:close-path)
		    (cairo:fill-path)))
	       (right-artificial-lmid-bottom (terrain) ; r-mirror of above
		 `(progn
		    (set-terrain-fill ,terrain)
		    (new-lines l-mid kite-mid b-r-corner l-b-corner)
		    (cairo:close-path)
		    (cairo:fill-path)))
	       (right-artificial-rmid-top (terrain)
		 `(progn
		    (set-terrain-fill ,terrain)
		    (new-lines r-mid kite-mid t-l-corner r-t-corner)
		    (cairo:close-path)
		    (cairo:fill-path)))
	       (left-artificial-bmid-top (terrain) ; r-mirror of above
		 `(progn
		    (set-terrain-fill ,terrain)
		    (new-lines b-mid kite-mid t-l-corner l-b-corner)
		    (cairo:close-path)
		    (cairo:fill-path)))
	       (right-artificial-bmid-top (terrain) ; inverse of above
		 `(progn
		    (set-terrain-fill ,terrain)
		    (new-lines b-mid kite-mid t-l-corner r-t-corner b-r-corner)
		    (cairo:close-path)
		    (cairo:fill-path)))
	       (left-artificial-left-tmid (terrain)
		 `(progn
		    (set-terrain-fill ,terrain)
		    (new-lines l-b-corner kite-mid t-mid t-l-corner)
		    (cairo:close-path)
		    (cairo:fill-path)))
	       (right-artificial-left-tmid (terrain) ; inverse of above
		 `(progn
		    (set-terrain-fill ,terrain)
		    (new-lines l-b-corner kite-mid t-mid r-t-corner b-r-corner l-b-corner)
		    (cairo:close-path)
		    (cairo:fill-path)))
	       (right-artificial-right-lmid (terrain)
		 `(progn
		    (set-terrain-fill ,terrain)
		    (new-lines r-t-corner kite-mid l-mid t-l-corner)
		    (cairo:close-path)
		    (cairo:fill-path)))
	       (natural-bottom (terrain &optional (point-0 nil offsetp) point-2)
		 (cond ((not offsetp)
			`(progn
			   (set-terrain-fill ,terrain)
			   (new-lines b-r-corner b-mid)
			   (let ((xy1 (crd half-kite-long
					   (* 0.36 half-kite-long)))
				 (xy2 (crd half-kite-long
					   (* -0.36 half-kite-long))))
			     (rotation (xy1) (xy2))
			     (curves xy1 xy2 r-mid))
			   (cairo:close-path)
			   (cairo:fill-path)))
		       (t
			`(progn
			   (set-terrain-fill (terrain-base (point-terrain ,terrain)))
			   (let* ((depth (point-water-elevation bottom))
				  (bottom-offset
				    (water-offset-bottom depth :END
							 (point-elevation ,point-0)
							 (point-elevation ,terrain)
							 hex-radius))
				  (right-offset
				    (water-offset-right depth :START
							(point-elevation ,terrain)
							(point-elevation ,point-2)
							hex-radius))
				  (xy0 (crd bottom-offset 0))
				  (xy1 (crd bottom-offset
					    (* 0.36 bottom-offset)))
				  (xy2 (crd right-offset
					    (* -0.36 right-offset)))
				  (xy3 (crd right-offset 0)))
			     #|(format t "~a ~a ~a ~a~%Water offset right : ~a~%"
				     depth
				     (point-elevation ,terrain)
				     (point-elevation ,point-2)
				     hex-radius
				     right-offset)|#
			     (rotation (xy0 xy1) (xy2 xy3))
			     (new-curves xy0 xy1 xy2 xy3)
			     (lines b-r-corner)
			     (cairo:close-path)
			     (cairo:fill-path))))))
	       (natural-top (terrain &optional (point-0 nil offsetp) point-2)
		 (cond ((not offsetp)
			`(progn
			   (set-terrain-fill ,terrain)
			   (let ((xy1 (crd (- half-down-y
					      (* 0.67 quarter-r))
					   quarter-r))
				 (xy2 (crd (- half-down-y
					      (* 0.67 quarter-r))
					   (- quarter-r))))
			     (rotation (xy1) (xy2))
			     (new-curves l-mid xy1 xy2 t-mid)
			     (lines t-l-corner)
			     (cairo:close-path)
			     (cairo:fill-path))))
		       (t
			`(progn
			   (set-terrain-fill (terrain-base (point-terrain ,terrain)))
			   (let* ((depth (point-water-elevation ,terrain))
				  (left-offset
				    (water-offset-left depth :start
						       (point-elevation ,terrain)
						       (point-elevation ,point-2)
						       hex-radius))
				  (top-offset
				    (water-offset-top depth :end
						      (point-elevation right);,point-0)
						      (point-elevation top);,terrain)
						      hex-radius))
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
			     (new-curves xy0 xy1 xy2 xy3)
			     (lines t-l-corner)
			     (cairo:close-path)
			     (cairo:fill-path))))))
	       (natural-right (terrain &optional (point-0 nil offsetp) point-2)
		 (cond ((not offsetp)
			`(progn
			   (set-terrain-fill ,terrain)
			   (let ((xy1 (crd half-kite-long
					   (- (* softness quarter-r))))
				 (xy2 (crd (- half-down-y
					      (* half-kite-long
						 softness))
					   (- quarter-r))))
			     (rotation () (xy1 xy2))
			     (new-curves r-mid xy1 xy2 t-mid)
			     (lines r-t-corner)
			     (cairo:close-path)
			     (cairo:fill-path))))
		       (t
			`(progn
			   (set-terrain-fill (terrain-base (point-terrain ,terrain)))
			   (let* ((depth (point-water-elevation right))
				  (right-offset
				    (water-offset-right depth :END
							(point-elevation ,point-0)
							(point-elevation ,terrain)
							hex-radius))
				  (top-offset
				    (water-offset-top depth :start
						      (point-elevation ,terrain)
						      (point-elevation ,point-2)
						      hex-radius))
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
			     (new-curves xy0 xy1 xy2 xy3)
			     (lines r-t-corner)
			     (cairo:close-path)
			     (cairo:fill-path))))))
	       (natural-left (terrain &optional (point-0 nil offsetp) point-2)
		 (cond ((not offsetp)
			`(progn
			   (set-terrain-fill ,terrain)
			   (let ((xy1 (crd half-kite-long
					   (* softness quarter-r)))
				 (xy2 (crd (- half-down-y
					      (* half-kite-long
						 softness))
					   quarter-r)))
			     (rotation (xy1 xy2) ())
			     (new-curves b-mid xy1 xy2 l-mid)
			     (lines l-b-corner)
			     (cairo:close-path)
			     (cairo:fill-path))))
		       (t
			`(progn
			   (set-terrain-fill (terrain-base (point-terrain ,terrain)))
			   (let* ((depth ;;(+ (depth (point-terrain ,terrain))
					    ;;(point-elevation ,terrain))
					 (point-water-elevation left))
				  (left-offset
				    (water-offset-left depth :END
						       (point-elevation ,point-0)
						       (point-elevation ,terrain)
						       hex-radius))
				  (bottom-offset
				    (water-offset-bottom depth :START
							 (point-elevation ,terrain)
							 (point-elevation ,point-2)
							 hex-radius))
				  (xy0 (crd bottom-offset 0.0))
				  (xy1 (crd bottom-offset
					    (* left-offset *soft*)))
				  (xy2 (crd (+ bottom-offset
					       (* *soft* (- half-down-y
							    bottom-offset)))
					    left-offset))
				  (xy3 (crd half-down-y left-offset)))
			     (rotation (xy0 xy1 xy2 xy3) ())
			     (new-curves xy0 xy1 xy2 xy3)
			     (lines l-b-corner)
			     (cairo:close-path)
			     (cairo:fill-path))))))
	       (natural-top-bottom (terrain &optional offsetp)
		 (cond ((not offsetp)
			`(progn
			   (set-terrain-fill ,terrain)
			   (let ((rxy1 (crd half-kite-long
					    (- (* softness quarter-r))))
				 (rxy2 (crd (- half-down-y
					       (* half-kite-long
						  softness))
					    (- quarter-r)))
				 (lxy1 (crd half-kite-long
					    (* softness quarter-r)))
				 (lxy2 (crd (- half-down-y
					       (* half-kite-long
						  softness))
					    quarter-r)))
			     (rotation (lxy1 lxy2) (rxy1 rxy2))
			     (new-curves r-mid rxy1 rxy2 t-mid)
			     (lines t-l-corner l-mid)
			     (curves lxy1 lxy2 b-mid)
			     (lines b-r-corner)
			     (cairo:close-path)
			     (cairo:fill-path))))
		       (t
			`(progn
			   (set-terrain-fill (terrain-base (point-terrain ,terrain)))
			   (let* ((depth-bot (point-water-elevation bottom))
				  (depth-top (point-water-elevation top))
				  
				  (bottom-offset
				    (water-offset-bottom depth-bot :END
							 (point-elevation left)
							 (point-elevation bottom)
							 hex-radius))
				  (left-offset
				    (water-offset-left depth-top :START
						       (point-elevation top)
						       (point-elevation left)
						       hex-radius))
				  (xy0 (crd bottom-offset 0.0))
				  (xy1 (crd bottom-offset
					    (* left-offset *soft*)))
				  (xy2 (crd (+ bottom-offset
					       (* *soft* (- half-down-y
							    bottom-offset)))
					    left-offset))
				  (xy3 (crd half-down-y left-offset))

				  (top-offset
				    (water-offset-top depth-top :END
						      (point-elevation right)
						      (point-elevation top)
						      hex-radius))
				  (right-offset
				    (water-offset-right depth-bot :START
							(point-elevation bottom)
							(point-elevation right)
							hex-radius))
				  (xy4 (crd half-down-y top-offset))
				  (xy5 (crd
					(+ right-offset
					   (* *soft*
					      (- half-down-y
						 right-offset)))
					top-offset))
				  (xy6 (crd right-offset
					    (* *soft* top-offset)))
				  (xy7 (crd right-offset 0))
				  )
			     (rotation (xy0 xy1 xy2 xy3) (xy4 xy5 xy6 xy7))
			     (new-curves xy0 xy1 xy2 xy3)
			     (lines t-l-corner xy4)
			     (curves xy5 xy6 xy7)
			     (lines b-r-corner)
			     (cairo:close-path)
			     (cairo:fill-path))))))
	       (natural-top-right (terrain &optional offsetp)
		 (cond ((not offsetp)
			`(progn
			   (set-terrain-fill ,terrain)
			   (let ((xy1 (crd half-kite-long
					   (- (* softness quarter-r))))
				 (xy2 (crd (- half-down-y
					      (* half-kite-long
						 softness))
					   quarter-r)))
			     (rotation (xy2) (xy1))
			     (new-curves r-mid xy1 xy2 l-mid)
			     (lines t-l-corner r-t-corner)
			     (cairo:close-path)
			     (cairo:fill-path))))
		       (t
			`(progn
			   (set-terrain-fill (terrain-base (point-terrain ,terrain)))
			   (let* ((depth-top (point-water-elevation top))
				  (depth-right (point-water-elevation right))
				  (left-offset
				    (water-offset-left depth-top :start
						       (point-elevation top)
						       (point-elevation left)
						       hex-radius))
				  (right-offset
				    (water-offset-right depth-right :END
							 (point-elevation bottom)
							 (point-elevation right)
							 hex-radius))
				  (xy0 (crd right-offset 0))
				  (xy1 (crd right-offset
					    (* -0.36 right-offset)))
				  (xy2 (crd (- half-down-y
					       (* *soft*
						  0.5
						  right-offset))
					    left-offset))
				  (xy3 (crd half-down-y left-offset)))
			     (rotation (xy2 xy3) (xy0 xy1))
			     (new-curves xy0 xy1 xy2 xy3)
			     (lines t-l-corner r-t-corner)
			     (cairo:close-path)
			     (cairo:fill-path))))))
	       (natural-bottom-left (terrain &optional offsetp) ; inverse of above
		 (cond ((not offsetp)
			`(progn
			   (set-terrain-fill ,terrain)
			   (let ((xy1 (crd half-kite-long
					   (- (* softness quarter-r))))
				 (xy2 (crd (- half-down-y
					      (* half-kite-long
						 softness))
					   quarter-r)))
			     (rotation (xy2) (xy1))
			     (new-curves r-mid xy1 xy2 l-mid)
			     (lines l-b-corner b-r-corner)
			     (cairo:close-path)
			     (cairo:fill-path))))
		       (t
			`(progn
			   (set-terrain-fill (terrain-base (point-terrain ,terrain)))
			   (let* ((depth-left (point-water-elevation left))
				  (depth-bot (point-water-elevation bottom))
				  (left-offset
				    (water-offset-left depth-left :end
						       (point-elevation top)
						       (point-elevation left)
						       hex-radius))
				  (right-offset
				    (water-offset-right depth-bot :START
							 (point-elevation bottom)
							 (point-elevation right)
							 hex-radius))
				  (xy0 (crd right-offset 0))
				  (xy1 (crd right-offset
					    (* -0.36 right-offset)))
				  (xy2 (crd (- half-down-y
					       (* *soft*
						  0.5
						  right-offset))
					    left-offset))
				  (xy3 (crd half-down-y left-offset)))
			     (rotation (xy2 xy3) (xy0 xy1))
			     (new-curves xy0 xy1 xy2 xy3)
			     (lines l-b-corner b-r-corner)
			     (cairo:close-path)
			     (cairo:fill-path))))))
	       (natural-top-left (terrain &optional (waterp))
		 (cond ((not waterp)
			`(progn
			   (set-terrain-fill ,terrain)
			   (let ((xy1 (crd half-kite-long
					   (* softness quarter-r)))
				 (xy2 (crd (- half-down-y
					      (* half-kite-long
						 softness))
					   (- quarter-r))))
			     (rotation (xy1) (xy2))
			     (new-curves b-mid xy1 xy2 t-mid)
			     (lines t-l-corner l-b-corner)
			     (cairo:close-path)
			     (cairo:fill-path))))
		       (t
			`(progn
			   (set-terrain-fill (terrain-base (point-terrain ,terrain)))
			   (let* ((depth0 (point-water-elevation left))
				  (depth2 (point-water-elevation top))
				  (bottom-offset
				    (water-offset-bottom depth0 :START
							 (point-elevation left)
							 (point-elevation bottom)
							 hex-radius))
				  (top-offset
				    (water-offset-top depth2 :END
						      (point-elevation right)
						      (point-elevation top)
						      hex-radius))
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
			     (new-curves xy0 xy1 xy2 xy3)
			     (lines t-l-corner l-b-corner)
			     (cairo:close-path)
			     (cairo:fill-path))))))
	       ;; inverse of above:
	       (natural-bottom-right (terrain &optional (waterp))
		 (cond ((not waterp)
			`(progn
			   (set-terrain-fill ,terrain)
			   (let ((xy1 (crd half-kite-long
					   (* softness quarter-r)))
				 (xy2 (crd (- half-down-y
					      (* half-kite-long
						 softness))
					   (- quarter-r))))
			     (rotation (xy1) (xy2))
			     (new-curves b-mid xy1 xy2 t-mid)
			     (lines r-t-corner b-r-corner)
			     (cairo:close-path)
			     (cairo:fill-path))))
		       (t
			`(progn
			   (set-terrain-fill (terrain-base (point-terrain ,terrain)))
			   (let* ((depth0 (point-water-elevation bottom))
				  (depth2 (point-water-elevation right))
				  (bottom-offset
				    (water-offset-bottom depth0 :END
							 (point-elevation left)
							 (point-elevation bottom)
							 hex-radius))
				  (top-offset
				    (water-offset-top depth2 :START
						      (point-elevation right)
						      (point-elevation top)
						      hex-radius))
				  (xy0 (crd bottom-offset 0))
				  (xy1 (crd bottom-offset
					    (* 0.36 bottom-offset)))
				  (xy2 (crd (- half-down-y
					       (* *soft*
						  0.5
						  bottom-offset))
					    top-offset))
				  (xy3 (crd half-down-y top-offset)))
			     (rotation (xy0 xy1) (xy2 xy3))
			     (new-curves xy0 xy1 xy2 xy3)
			     (lines r-t-corner b-r-corner)
			     (cairo:close-path)
			     (cairo:fill-path))))))
	       ;; inverse of natural-right:
	       (natural-top-bottom-left
		   (terrain &optional (point-0 nil offsetp) point-2 r-t-point)
		 (cond ((not offsetp)
			`(progn
			   (set-terrain-fill ,terrain)
			   (let ((xy1 (crd half-kite-long
					   (- (* softness quarter-r))))
				 (xy2 (crd (- half-down-y
					      (* half-kite-long
						 softness))
					   (- quarter-r))))
			     (rotation () (xy1 xy2))
			     (new-curves r-mid xy1 xy2 t-mid)
			     (lines t-l-corner l-b-corner b-r-corner)
			     (cairo:close-path)
			     (cairo:fill-path))))
		       (t
			`(progn
			   (set-terrain-fill (terrain-base (point-terrain ,terrain)))
			   (let* ((depth0 (point-water-elevation bottom))
				  (depth2 (point-water-elevation top))
				  (right-offset
				    (water-offset-right depth0 :START
							(point-elevation bottom)
							(point-elevation ,r-t-point)
							hex-radius))
				  (top-offset
				    (water-offset-top depth2 :end
						      (point-elevation ,r-t-point)
						      (point-elevation top)
						      hex-radius))
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
			     (new-curves xy0 xy1 xy2 xy3)
			     (lines b-r-corner l-b-corner t-l-corner)
			     (cairo:close-path)
			     (cairo:fill-path))))))
	       ;; inverse of natural-bottom:
	       (natural-top-right-left (terrain &optional (point-0 nil offsetp) point-2 b-r-point)
		 (cond ((not offsetp)
			`(progn
			   (set-terrain-fill ,terrain)
			   (let ((xy1 (crd half-kite-long
					   (* 0.36 half-kite-long)))
				 (xy2 (crd half-kite-long
					   (* -0.36 half-kite-long))))
			     (rotation (xy1) (xy2))
			     (new-curves b-mid xy1 xy2 r-mid)
			     (lines r-t-corner t-l-corner l-b-corner)
			     (cairo:close-path)
			     (cairo:fill-path))))
		       (t
			`(progn
			   (set-terrain-fill (terrain-base (point-terrain ,terrain)))
			   (let* ((depth0 (point-water-elevation left))
				  (depth2 (point-water-elevation right))
				  (bottom-offset
				    (water-offset-bottom depth0 :START
							 (point-elevation ,point-0)
							 (point-elevation ,b-r-point)
							 hex-radius))
				  (right-offset
				    (water-offset-right depth2 :END
							(point-elevation ,b-r-point)
							(point-elevation ,point-2)
							hex-radius))
				  (xy0 (crd bottom-offset 0))
				  (xy1 (crd bottom-offset
					    (* 0.36 bottom-offset)))
				  (xy2 (crd right-offset
					    (* -0.36 right-offset)))
				  (xy3 (crd right-offset 0)))
			     (rotation (xy0 xy1) (xy2 xy3))
			     (new-curves xy0 xy1 xy2 xy3)
			     (lines r-t-corner t-l-corner l-b-corner)
			     (cairo:close-path)
			     (cairo:fill-path))))))
	       ;; inverse of natural-left:
	       (natural-top-bottom-right
		   (terrain &optional (point-0 nil offsetp) point-2 l-b-point)
		 (cond ((not offsetp)
			`(progn
			   (set-terrain-fill ,terrain)
			   (let ((xy1 (crd half-kite-long
					   (* softness quarter-r)))
				 (xy2 (crd (- half-down-y
					      (* half-kite-long
						 softness))
					   quarter-r)))
			     (rotation (xy1 xy2) ())
			     (new-curves b-mid xy1 xy2 l-mid)
			     (lines t-l-corner r-t-corner b-r-corner)
			     (cairo:close-path)
			     (cairo:fill-path))))
		       (t
			`(progn
			   (set-terrain-fill (terrain-base (point-terrain ,terrain)))
			   (let* ((depth0 (point-water-elevation ,point-0))
				  (depth2 (point-water-elevation ,point-2))
				  (left-offset
				    (water-offset-left depth0 :START
						       (point-elevation ,point-0)
						       (point-elevation ,l-b-point)
						       hex-radius))
				  (bottom-offset
				    (water-offset-bottom depth2 :END
							 (point-elevation ,l-b-point)
							 (point-elevation ,point-2)
							 hex-radius))
				  (xy0 (crd bottom-offset 0.0))
				  (xy1 (crd bottom-offset
					    (* left-offset *soft*)))
				  (xy2 (crd (+ bottom-offset
					       (* *soft* (- half-down-y
							    bottom-offset)))
					    left-offset))
				  (xy3 (crd half-down-y left-offset)))
			     (rotation (xy0 xy1 xy2 xy3) ())
			     (new-curves xy0 xy1 xy2 xy3)
			     (lines t-l-corner r-t-corner b-r-corner)
			     (cairo:close-path)
			     (cairo:fill-path))))))
	       ; ; inverse of natural-top:
	       (natural-bottom-right-left
		   (terrain &optional (point-0 nil offsetp) point-2 t-l-point)
		 (cond ((not offsetp)
			`(progn
			   (set-terrain-fill ,terrain)
			   (let ((xy1 (crd (- half-down-y
					      (* 0.67 quarter-r))
					   quarter-r))
				 (xy2 (crd (- half-down-y
					      (* 0.67 quarter-r))
					   (- quarter-r))))
			     (rotation (xy1) (xy2))
			     (new-curves l-mid xy1 xy2 t-mid)
			     (lines r-t-corner b-r-corner l-b-corner)
			     (cairo:close-path)
			     (cairo:fill-path))))
		       (t
			`(progn
			   (set-terrain-fill (terrain-base (point-terrain ,terrain)))
			   (let* ((depth0 (point-water-elevation ,point-0))
				  (depth2 (point-water-elevation ,point-2))
				  (left-offset
				    (water-offset-left depth2 :end
						       (point-elevation ,t-l-point)
						       (point-elevation ,point-2)
						       hex-radius))
				  (top-offset
				    (water-offset-top depth0 :start
						      (point-elevation ,point-0)
						      (point-elevation ,t-l-point)
						      hex-radius))
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
			     (new-curves xy0 xy1 xy2 xy3)
			     (lines r-t-corner b-r-corner l-b-corner)
			     (cairo:close-path)
			     (cairo:fill-path))))))
	       (right-curve-rmid-top (terrain)
		 `(progn
		    (set-terrain-fill ,terrain)
		    (let ((xy1 (crd half-kite-long (- quarter-r))))
		      (rotation () (xy1))
		      (new-curves r-mid xy1 top-to-l/r-cpoint t-l-corner)
		      (lines r-t-corner)
		      (cairo:close-path)
		      (cairo:fill-path))))
	       (left-curve-bmid-top (terrain)
		 `(progn
		    (set-terrain-fill ,terrain)
		    (let ((xy1 (crd half-kite-long quarter-r)))
		      (rotation (xy1) ())
		      (new-curves b-mid xy1 top-to-l/r-cpoint t-l-corner)
		      (lines l-b-corner)
		      (cairo:close-path)
		      (cairo:fill-path))))
	       (right-curve-bottom-tmid (terrain)
		 `(progn
		    (set-terrain-fill ,terrain)
		    (let ((xy1 (crd (* +cos30+ half-r)
				    (* +sin30+ half-r)))
			  (xy2 (crd (+ half-kite-long quarter-r)
				    (- quarter-r))))
		      (rotation (xy1) (xy2))
		      (new-curves b-r-corner xy1 xy2 t-mid)
		      (lines r-t-corner)
		      (cairo:close-path)
		      (cairo:fill-path))))
	       (left-curve-bottom-lmid (terrain)
		 `(progn
		    (set-terrain-fill ,terrain)
		    (let ((xy1 (crd (* +cos30+ half-r)
				    (* +sin30+ half-r)))
			  (xy2 (crd (+ half-kite-long quarter-r)
				    quarter-r)))
		      (rotation (xy1 xy2) ())
		      (new-curves b-r-corner xy1 xy2 l-mid)
		      (lines l-b-corner)
		      (cairo:close-path)
		      (cairo:fill-path))))
	       (kite-left-bottom (terrain)
		 `(progn
		    (set-terrain-fill ,terrain)
		    (new-lines r-mid l-b-corner b-r-corner)
		    (cairo:close-path)
		    (cairo:fill-path)))
	       (kite-right-top (terrain) ; inverse of above
		 `(progn
		    (set-terrain-fill ,terrain)
		    (new-lines r-mid l-b-corner t-l-corner r-t-corner)
		    (cairo:close-path)
		    (cairo:fill-path)))
	       (kite-right-bottom (terrain)
		 `(progn
		    (set-terrain-fill ,terrain)
		    (new-lines b-mid r-t-corner b-r-corner)
		    (cairo:close-path)
		    (cairo:fill-path)))
	       (kite-left-top (terrain) ; inverse of above
		 `(progn
		    (set-terrain-fill ,terrain)
		    (new-lines b-mid r-t-corner t-l-corner l-b-corner)
		    (cairo:close-path)
		    (cairo:fill-path)))
	       (left-curve-left-tmid (terrain)
		 `(progn
		    (set-terrain-fill ,terrain)
		    (let ((xy1 (crd half-kite-long
				    quarter-r))
			  (xy2 (crd (* half-kite-long 1.5)
				    (- quarter-r))))
		      (rotation (xy1) (xy2))
		      (new-curves l-b-corner xy1 xy2 t-mid)
		      (lines t-l-corner)
		      (cairo:close-path)
		      (cairo:fill-path))))
	       (right-curve-right-lmid (terrain)
		 `(progn
		    (set-terrain-fill ,terrain)
		    (let ((xy1 (crd half-kite-long
				    (- quarter-r)))
			  (xy2 (crd (* half-kite-long 1.5)
				    quarter-r)))
		      (rotation (xy2) (xy1))
		      (new-curves r-t-corner xy1 xy2 l-mid)
		      (lines t-l-corner)
		      (cairo:close-path)
		      (cairo:fill-path)))))
      
      (rotation (kite-mid b-mid l-mid b-msal b-mkal top-to-l/r-cpoint)
		(t-mid r-mid r-msal r-mkal))
      
      (cairo:with-context (cairo-context)
	(cairo:set-source-rgb 1.0 0.0 1.0)
	(cairo:set-line-width 0.5)
	(cairo:set-antialias :none)

	(flet ((render-natural-terrain (top left bottom right)
		 
		   
		   (let ((selector (+ (if (terrain-naturalp top) 8 0)
				      (if (terrain-naturalp left) 4 0)
				      (if (terrain-naturalp bottom) 2 0)
				      (if (terrain-naturalp right) 1 0))))

		     #|
		     (defun check (caselist)
		       (sort (loop for x in caselist
			     when (listp x)
			       if (listp (car x))
				 append (car x)
			     else collect (car x)) #'<))
		     |#
		     ;;(format t "Selector ~a~%" selector)
		     (ecase selector
		       (0 ; No natural land terrain
			nil)
		       ((1 2 4 8) ; No natural land terrain boundaries
					;(format t "WOW ~a~%" selector)
			(kite-perimeter (case selector
					    (1 right)
					    (2 bottom)
					    (4 left)
					    (8 top))))
		       (3 ; bottom & right
			(cond ((eq bottom right)
			       (kite-perimeter bottom))
			      (t
			       (set-terrain-fill right)
			       (cairo:move-to (x r-mid) (y r-mid))
			       (cairo:line-to (x l-b-corner) (y l-b-corner))
			       (cairo:line-to (x t-l-corner) (y t-l-corner))
			       (cairo:line-to (x r-t-corner) (y r-t-corner))
			       (cairo:close-path)
			       (cairo:fill-path)
			       
			       (set-terrain-fill bottom)
			       (cairo:move-to (x r-mid) (y r-mid))
			       (cairo:line-to (x l-b-corner) (y l-b-corner))
			       (cairo:line-to (x b-r-corner) (y b-r-corner))
			       (cairo:close-path)
			       (cairo:fill-path))))
		       (6 ; bottom & left
			(cond ((eq bottom left)
			       (kite-perimeter bottom))
			      (t
			       (set-terrain-fill left)
			       (cairo:move-to (x b-mid) (y b-mid))
			       (cairo:line-to (x r-t-corner) (y r-t-corner))
			       (cairo:line-to (x t-l-corner) (y t-l-corner))
			       (cairo:line-to (x l-b-corner) (y l-b-corner))
			       (cairo:close-path)
			       (cairo:fill-path)
			       
			       (set-terrain-fill bottom)
			       (cairo:move-to (x b-mid) (y b-mid))
			       (cairo:line-to (x r-t-corner) (y r-t-corner))
			       (cairo:line-to (x b-r-corner) (y b-r-corner))
			       (cairo:close-path)
			       (cairo:fill-path))))
		       (12 ; top & left
			(cond ((eq top left)
			       (kite-perimeter top))
			      (t
			       (set-terrain-fill left)
			       (new-lines l-mid r-msal b-r-corner l-b-corner)
			       (cairo:close-path)
			       (cairo:fill-path)

			       (set-terrain-fill top)
			       (new-lines l-mid r-msal r-t-corner t-l-corner)
			       (cairo:close-path)
			       (cairo:fill-path))))
		       (9 ; top & right
			(cond ((eq top right)
			       (kite-perimeter top))
			      (t
			       (set-terrain-fill right)
			       (new-lines t-mid b-msal b-r-corner r-t-corner)
			       (cairo:close-path)
			       (cairo:fill-path)

			       (set-terrain-fill top)
			       (new-lines t-mid b-msal l-b-corner t-l-corner)
			       (cairo:close-path)
			       (cairo:fill-path))))
		       (10 ; top & bottom
			(cond ((eq top bottom)
			       (kite-perimeter top))
			      (t
			       (set-terrain-fill bottom)
			       (new-lines r-mkal b-mkal b-r-corner)
			       (cairo:close-path)
			       (cairo:fill-path)

			       (set-terrain-fill top)
			       (new-lines r-mkal b-mkal l-b-corner t-l-corner r-t-corner)
			       (cairo:close-path)
			       (cairo:fill-path)
			       )))
		       (7 ; all but top
			(cond ((and (eq left right) ; all same
				    (eq left bottom))
			       (kite-perimeter left))
			      ((terrain-artificialp top)
			       (cond ((eq left bottom)
				      (kite-perimeter left)
				      
				      (set-terrain-fill right)
				      (new-lines r-mid kite-mid t-mid r-t-corner)
				      (cairo:close-path)
				      (cairo:fill-path))
				     ((eq right bottom)
				      (kite-perimeter right)

				      (set-terrain-fill left)
				      (new-lines l-mid kite-mid b-mid l-b-corner)
				      (cairo:close-path)
				      (cairo:fill-path))
				     ((eq left right)
				      (kite-perimeter right)
				      (natural-bottom bottom))
				     (t ; all different
				      (left-kite-perimeter left)
				      (right-kite-perimeter right)
				      (natural-bottom bottom))))
			      (t ; (terrain-waterp top)
			       (cond ((eq left bottom)
				      (kite-perimeter left)
				      (right-curve-rmid-top right))
				     ((eq right bottom)
				      (kite-perimeter right)
				      (left-curve-bmid-top left))
				     ((eq left right)
				      (kite-perimeter right)
				      (natural-bottom bottom))
				     (t ; all different
				      (left-kite-perimeter left)
				      (right-kite-perimeter right)
				      (natural-bottom bottom))))))
		       (13 ; all but bottom
			(cond ((and (eq left right) ; all same
				    (eq left top))
			       (kite-perimeter left))
			      ((terrain-artificialp bottom)
			       (cond ((eq left top)
				      (kite-perimeter left)
				      (artificial-right right))
				     ((eq top right)
				      (kite-perimeter top)
				      (artificial-left left))
				     ((eq left right)
				      (kite-perimeter right)
				      (artificial-top top))
				     (t ; all different
				      (left-kite-perimeter left)
				      (right-kite-perimeter right)
				      (natural-top top))))
			      (t ; (terrain-waterp bottom)
			       (cond ((eq left top)
				      (kite-perimeter left)
				      (right-curve-bottom-tmid right))
				     ((eq top right)
				      (kite-perimeter top)
				      (left-curve-bottom-lmid left))
				     ((eq left right)
				      (kite-perimeter left)
				      (natural-top top))
				     (t ; all different
				      (left-kite-perimeter left)
				      (right-kite-perimeter right)
				      (natural-top top))))))
		       (5 ; left & right
			(cond ((eq left right)
			       (kite-perimeter left))
			      (t
			       (left-kite-perimeter left)
			       (right-kite-perimeter right))))
		       (11 ; all but left
			(cond ((and (eq top right) ; all same
				    (eq top bottom))
			       (kite-perimeter top))
			      ((terrain-artificialp left)
			       (cond ((eq top right)
				      (kite-perimeter right)
				      (artificial-bottom bottom))
				     ((eq top bottom)
				      (kite-perimeter top)
				      (natural-right right))
				     ((eq right bottom)
				      (kite-perimeter right)
				      (artificial-top top))
				     (t ; all different
				      (artificial-top top)
				      (artificial-bottom bottom)
				      (artificial-right right))))
			      (t ; (terrain-waterp left)
			       (cond ((eq top right)
				      (kite-perimeter top)
				      (kite-left-bottom bottom))
				     ((eq top bottom)
				      (kite-perimeter top)
				      (natural-right right))
				     ((eq right bottom)
				      (kite-perimeter bottom)
				      (left-curve-left-tmid top))
				     (t ; all different
				       ;; NOTE : could also draw between long mids
				      (top-kite-perimeter top)
				      (bottom-kite-perimeter bottom)
				      (natural-right right))))))
		       (14 ; all but right
			(cond ((and (eq top left)
				    (eq top bottom))
			       (kite-perimeter top))
			      ((terrain-artificialp right)
			       (cond ((eq top left)
				      (kite-perimeter left)
				      (artificial-bottom bottom))
				     ((eq top bottom)
				      (kite-perimeter top)
				      (natural-left left))
				     ((eq left bottom)
				      (kite-perimeter left)
				      (artificial-top top))
				     (t ; all different
				      (artificial-top top)
				      (artificial-bottom bottom)
				      (artificial-left left))))
			      (t ; (terrain-waterp right)
			       (cond ((eq top left)
				      (kite-perimeter top)
				      (kite-right-bottom bottom))
				     ((eq top bottom)
				      (kite-perimeter top)
				      (natural-left left))
				     ((eq left bottom)
				      (kite-perimeter bottom)
				      (right-curve-right-lmid top))
				     (t ; all different
				       ;; NOTE : could also draw between long mids
				      (top-kite-perimeter top)
				      (bottom-kite-perimeter bottom)
				      (natural-left left))))
			      ))
		       (15 ; 100% natural
			(cond ((eq* top left right bottom) ;0000
			       (kite-perimeter top))
			      
			      ((eq* top right bottom) ;1110
			       (kite-perimeter top)
			       (natural-left left))
			      ((eq* top right left) ;1101
			       (kite-perimeter top)
			       (natural-bottom bottom))
			      ((eq* top bottom left) ;1011
			       (kite-perimeter top)
			       (natural-right right))
			      ((eq* right bottom left) ;0111
			       (kite-perimeter right)
			       (natural-top top))

			      ((and (eq top right) ;0011
				    (eq bottom left))
			       (kite-perimeter bottom)
			       (natural-top-right top))
			      ((and (eq top left) ;0110
				    (eq right bottom))
			       (kite-perimeter bottom)
			       (natural-top-left top))
			      ((and (eq top bottom) ;1010
				    (eq right left))
			       ;;; NOTE: This stuff may have an effect on moving hex to hex!
			       (kite-perimeter left)
			       (natural-top top)
			       (natural-bottom bottom)
			       #|
			       ;; Could also do:
			       (kite-perimeter top)
			       (natural-right right)
			       (natural-left left)
			       |#
			       )
			      ((eq top right) ;0012
			       (kite-perimeter bottom)
			       (natural-top-left left)
			       (natural-top-right top))
			      ((eq top bottom) ;0102
			       ;;; NOTE: Gameplay effects! see above
			       (left-kite-perimeter left)
			       (right-kite-perimeter right)
			       (natural-top top)
			       (natural-bottom bottom))
			      ((eq top left) ;0120
			       (kite-perimeter bottom)
			       (natural-top-right right)
			       (natural-top-left left))

			      ((eq right bottom) ;1002
			       (kite-perimeter left)
			       (natural-top-right top)
			       (natural-bottom-right right))
			      ((eq right left) ;1020
			       ;;; NOTE: Gameplay effects! see above
			       (kite-perimeter right)
			       (natural-top top)
			       (natural-bottom bottom))

			      ((eq bottom left) ;1200
			       (kite-perimeter right)
			       (natural-top-left top)
			       (natural-bottom-left left))
			      
			      (t ; all different
			       #| also known as:
			       (not (or (eq top right)
				  	(eq top bottom)
					(eq top left)
					(eq right bottom)
					(eq right left)
					(eq bottom left)))|#
			       (artificial-top top)
			       (artificial-right right)
			       (artificial-bottom bottom)
			       (artificial-left left)))))))
	       (render-artificial-terrain (top left bottom right)

		 (let ((selector (+ (if (terrain-artificialp top) 8 0)
				    (if (terrain-artificialp left) 4 0)
				    (if (terrain-artificialp bottom) 2 0)
				    (if (terrain-artificialp right) 1 0)))
		       (water (+ (if (terrain-waterp top) 8 0)
				 (if (terrain-waterp left) 4 0)
				 (if (terrain-waterp bottom) 2 0)
				 (if (terrain-waterp right) 1 0))))
		   (case selector
		     (0 nil)
		     (1 ; right
		      (ecase water
			(14 (kite-perimeter right))
			((0 4 6 10) (artificial-right right))
			(2 (left-artificial-tmid-bottom right))
			(8 (right-artificial-rmid-top right))
			(12 (artificial-top-right right))
			((1 3 5 7 9 11 13 15) nil)))
		     (2 ; bottom
		      (ecase water
			(13 (kite-perimeter bottom))
			((0 8) (artificial-bottom bottom))
			((1 9) (kite-right-bottom bottom))
			((4 12) (kite-left-bottom bottom))
			(5 (bottom-kite-perimeter bottom))
			((2 3 6 7 10 11 14 15) nil)))
		     (4 ; left
		      (ecase water
			(11 (kite-perimeter left))
			((0 1 3 10) (artificial-left left))
			(2 (right-artificial-lmid-bottom left))
			(8 (left-artificial-bmid-top left))
			(9 (artificial-top-left left))
			((4 5 6 7 12 13 14 15) nil)))
		     (8 ; top
		      (ecase water
			(7 (kite-perimeter top))
			((0 2 3 6) (artificial-top top))
			(1 (right-artificial-right-lmid top))
			(4 (left-artificial-left-tmid top))
			(5 (top-kite-perimeter top))
			((8 9 10 11 12 13 14 15) nil)))
		     (3 ; right & bottom
		      (ecase water
			(0
			 (artificial-bottom bottom)
			 (artificial-right right))
			(4
			 (artificial-right right)
			 (kite-left-bottom bottom))
			(8
			 (artificial-bottom bottom)
			 (right-artificial-rmid-top right))
			(12
			 (kite-left-bottom bottom)
			 (kite-right-top right))
			((1 2 3 5 6 7 9 10 11 13 14 15) nil)))
		     (5 ; left & right
		      (ecase water
			(0
			 (artificial-left left)
			 (artificial-right right))
			(10
			 (left-kite-perimeter left)
			 (right-kite-perimeter right))
			(2
			 (right-artificial-lmid-bottom left)
			 (left-artificial-tmid-bottom right))
			(8
			 (left-artificial-bmid-top left)
			 (right-artificial-rmid-top right))
			((1 4 5 6 7 9 11 12 13 14 15) nil)))
		     (6 ; left & bottom
		      (ecase water
			(0
			 (artificial-left left)
			 (artificial-bottom bottom))
			(1
			 (artificial-left left)
			 (kite-right-bottom bottom))
			(8
			 (left-artificial-bmid-top left)
			 (artificial-bottom bottom))
			(9
			 (kite-right-bottom bottom)
			 (kite-left-top left))
			((2 3 4 5 6 7 10 11 12 13 14 15) nil)))
		     (7 ; all but top
		      (ecase water
			(0
			 (artificial-right right)
			 (artificial-left left)
			 (artificial-bottom bottom))
			(8
			 (artificial-bottom bottom)
			 (left-artificial-bmid-top left)
			 (right-artificial-rmid-top right))
			((1 2 3 4 5 6 7 9 10 11 12 13 14 15) nil)))
		     (9 ; top & right
		      (ecase water
			(0
			 (artificial-top top)
			 (artificial-right right))
			(6 (kite-perimeter top))
			(2
			 (artificial-top top)
			 (left-artificial-tmid-bottom right))
			(4
			 (artificial-right right)
			 (left-artificial-left-tmid top))
			((1 3 5 7 8 9 10 11 12 13 14 15) nil)))
		     (10 ; top & bottom
		      (ecase water
			(0
			 (artificial-top top)
			 (artificial-bottom bottom))
			(1
			 (right-artificial-right-lmid top)
			 (kite-right-bottom bottom))
			(4
			 (left-artificial-left-tmid top)
			 (kite-left-bottom bottom))
			(5
			 (top-kite-perimeter top)
			 (bottom-kite-perimeter bottom))
			((2 3 6 7 8 9 10 11 12 13 14 15) nil)))
		     (11 ; all but left
		      (ecase water
			(0
			 (artificial-right right)
			 (artificial-top top)
			 (artificial-bottom bottom))
			(4
			 (artificial-right right)
			 (left-artificial-left-tmid top)
			 (kite-left-bottom bottom))
			((1 2 3 5 6 7 8 9 10 11 12 13 14 15) nil)))
		     (12 ; top & left
		      (ecase water
			(3
			 (right-kite-perimeter top)
			 (left-kite-perimeter left))
			(0
			 (artificial-top top)
			 (artificial-left left))
			(1
			 (artificial-left left)
			 (right-artificial-right-lmid top))
			(2
			 (artificial-top top)
			 (right-artificial-lmid-bottom left))
			((4 5 6 7 8 9 10 11 12 13 14 15) nil)))
		     (13 ; all but bottom
		      (ecase water
			(0
			 (artificial-right right)
			 (artificial-top top)
			 (artificial-left left))
			(2
			 (artificial-top top)
			 (right-artificial-lmid-bottom left)
			 (left-artificial-tmid-bottom right))
			((1 3 4 5 6 7 8 9 10 11 12 13 14 15) nil)))
		     (14 ; all but right
		      (ecase water
			(0
			 (artificial-top top)
			 (artificial-left left)
			 (artificial-bottom bottom))
			(1
			 (artificial-left left)
			 (kite-right-bottom bottom)
			 (right-artificial-right-lmid top))
			((2 3 4 5 6 7 8 9 10 11 12 13 14 15) nil)))
		     (15 ; 100% plastic
		      (artificial-top top)
		      (artificial-left left)
		      (artificial-right right)
		      (artificial-bottom bottom)))
		   NIL))

	       (render-water-terrain (top left bottom right)
		 (let ((selector (+ (if (terrain-waterp (terrain-base (point-terrain top))) 8 0)
				    (if (terrain-waterp (terrain-base (point-terrain left))) 4 0)
				    (if (terrain-waterp (terrain-base (point-terrain bottom))) 2 0)
				    (if (terrain-waterp (terrain-base (point-terrain right))) 1 0))))
		   (case selector
		     (0 nil)
		     (1 (natural-right right bottom top))
		     (2 (natural-bottom bottom left right))
		     (4 (natural-left left top bottom))
		     (8 (natural-top top right left))

		     (3 (natural-bottom-right bottom t))
		     (12 (natural-top-left left t))

		     (5 (natural-left left top bottom)
		      (natural-right right bottom top))
		     (10 (natural-top-bottom bottom t))

		     (6 (natural-bottom-left bottom t))
		     (9 (natural-top-right right t))

		     (7 (natural-bottom-right-left bottom right left top))
		     (11 (natural-top-bottom-right bottom top bottom left))
		     (13 (natural-top-right-left right left right bottom))
		     (14 (natural-top-bottom-left bottom bottom top right))
		     (15 (kite-perimeter (terrain-base (point-terrain bottom))))
		     ))))
	  
	  ;;;; 3^4 = 81 permutations, have to render in layers
	  (let* ((topt (point-terrain top))
		 (leftt (point-terrain left))
		 (bottomt (point-terrain bottom))
		 (rightt (point-terrain right))
		 (topb (terrain-base topt))
		 (leftb (terrain-base leftt))
		 (bottomb (terrain-base bottomt))
		 (rightb (terrain-base rightt)))
	    (render-natural-terrain topb leftb bottomb rightb)
	    (render-artificial-terrain topb leftb bottomb rightb)
	    (render-water-terrain top left bottom right))
	  
	  )))))

(defun depth (terrain)
  "Returns water depth of TERRAIN."
  (cdr (assoc :depth (terrain-mod terrain) :test #'eq)))

(defun point-water-elevation (point) ;; TODO: move
  (+ (depth (point-terrain point))
     ;(point-elevation point)
     ))


'(defun water-offset-bottom (depth l-b-ele b-r-ele hex-r)
  (let ((half-down-y (* +sin60+ hex-r))
	(index (abs (- depth
		       (+ (if (< l-b-ele b-r-ele)
			      1 0)
			  l-b-ele))))
	(range (- b-r-ele l-b-ele)))
    (- half-down-y
       (the single-float
	    (contour-offset index
			    range
			    half-down-y)))))

'(defun water-offset-left (depth water-at t-l-ele l-b-ele hex-r)
  (declare ((member :start :end) water-at))
  (let ((half-r (/ hex-r 2.0)) ;; what??
;;	(index (abs (- depth
;;		       (+ (if (< t-l-ele l-b-ele)
;;			      1 0)
;;			  t-l-ele))))
	(range (- l-b-ele t-l-ele)))
    #|(- half-r
       (the single-float 
	    (contour-offset index
			    range
    half-r)))|#

    (case water-at
      (:START
       '(cond ((plusp range)
	      (contour-offset (- (abs range) depth)
			      range
			      half-r))
	     ((zerop range)
	      (contour-offset 0 depth half-r))
	     ((minusp range)
	      (setf range (* depth 2 range))
	      (format t "start minus ~a ~a~%" depth range)
		      ;; (1- (- range (1- depth)))
	      (contour-offset 0 ;;;; TODO  something
			      range
	       half-r)))
       0.0)
      (:END
       half-r 0.0))
    ))


;;;; This is the same code as WATER-OFFSET-RIGHT but START/END cases switched
(defun water-offset-bottom (depth water-at l-b-ele b-r-ele hex-r)
  (declare ((member :start :end) water-at))
  (let ((half-down-y (* +sin60+ hex-r))
	(range (- b-r-ele l-b-ele)))
    (format t "woBOTTOM ")
    (case water-at
      (:end ;; OK
       (format t "end ")
       (cond ((plusp range)
	      (if (<= depth range)
		  (contour-offset (1- depth) range half-down-y)
		  (let ((shift-range (+ (- depth range) range)))
		    (contour-offset (1- shift-range)
				    shift-range
				    half-down-y))))
	     ((zerop range)
	      (contour-offset (1- depth) depth half-down-y))
	     ((minusp range)
	      ;;(format t "minusp -> ~a ~a~%" depth range)
	      (if (<= depth (abs range))
		  (contour-offset (1- depth) range half-down-y)
		  (let ((shift-range (- (+ (- depth (abs range))
					   (abs range)))))
		    (contour-offset (1- (abs shift-range))
				    shift-range
				    half-down-y))))))
      (:start
      (format t "start ")
       (cond ((plusp range)
	      (format t "plusp depth ~a range ~a => ~a // ~a . ~a~%"
		      depth range (1- (+ range depth))
			      (+ range depth) half-down-y)
	      ;;; This will cause water border to move away
	      ;; from cliff drop as depth increases:
	      (if (<= depth (abs range))
		  (contour-offset (- (abs range) depth)
				  (abs range)
				  half-down-y)
		  (let ((shift-range (+ (- depth (abs range))
					(abs range))))
		    (contour-offset 0
				    shift-range
				    half-down-y))))
	     ((zerop range)
	      (format t "zerop range~%")
	      (contour-offset 0 depth half-down-y))
	     ((minusp range)
	      (format t "minusp depth: ~a range: ~a~%" depth range)
	      (contour-offset (1- (+ (abs range) depth))
			      (+ (abs range) depth)
			      half-down-y)))))))

(defun water-offset-right (depth water-at b-r-ele r-t-ele hex-r)
  (declare ((member :start :end) water-at))
  (let ((half-down-y (* +sin60+ hex-r))
	(range (- r-t-ele b-r-ele)))
    (format t "woRIGHT ")
    (case water-at
      (:START ;; OK
       (format t "start ")
       (cond ((plusp range)
	      (format t "plusp~%")
	      (if (<= depth range)
		  (contour-offset (1- depth) range half-down-y)
		  (let ((shift-range (+ (- depth range) range)))
		    (contour-offset (1- shift-range)
				    shift-range
				    half-down-y))))
	     ((zerop range)
	      (format t "zerop~%")
	      (contour-offset (1- depth) depth half-down-y))
	     ((minusp range)
	      (format t "minusp -> ~a ~a~%" depth range)
	      (if (<= depth (abs range))
		  (contour-offset (1- depth) range half-down-y)
		  (let ((shift-range (- (+ (- depth (abs range))
					   (abs range)))))
		    (contour-offset (1- (abs shift-range))
				    shift-range
				    half-down-y))))))
      (:END
       (format t "end ")
       (cond ((plusp range)
	      (format t "plusp~%")
	      ;;; This will cause water border to move away
	      ;; from cliff drop as depth increases:
	      (contour-offset (1- (+ range depth))
			      (+ range depth)
			      half-down-y))
	     ((zerop range)
	      (format t "zerop~%")
	      (contour-offset 0 depth half-down-y))
	     ((minusp range)
	      (format t "Minusp ~a ~a~%" depth range)
	      (if (<= depth (abs range))
		  (contour-offset (- (abs range)
				     depth)
				  range half-down-y)
		  (let ((shift-range (+ (- depth (abs range))
					(abs range))))
		    (contour-offset 0
				    shift-range
				    half-down-y)))))))))

(defun water-offset-top (depth water-at ele-0 ele-1 hex-r)
  (declare ((member :start :end) water-at))
  (let ((minus-half-r (/ hex-r -2.0))
	(range (- ele-1 ele-0)))
    (case water-at
      (:START
       (format t "Water at start ")
       (cond ((plusp range)
	      (format t "plusp range ~a ~a~%" (1- depth) range)
	      (if (<= depth range)
		  (contour-offset (1- depth) range minus-half-r)
		  (contour-offset (1- (+ (abs range) depth))
				  (+ (abs range) depth)
				  minus-half-r)))
	     ((zerop range)
	      (format t "zero range -> ~a ~a~%" 0 1)
	      (contour-offset (1- depth) depth minus-half-r))
	     ((minusp range)
	      (format t "minusp range ~a ~a~%" depth range)
	      ;; Great!
	      (contour-offset (1- (- depth (abs range)))
			      (* 2 (+ (abs range)
				      (- depth
					 (abs range))))
			      minus-half-r))))
      (:END
       (format t "TOP TOP TOP Water at end~%")
       (cond ((plusp range)
	      (format t "plusp range ~a ~a~%" (1- depth) range)
	      (setf range (* depth range 2))
	      (format t "   -> ~a : ~a~%" (1- (- range (1- depth))) range)
	      (contour-offset (1- (- range (1- depth))) ;; ?
			      range minus-half-r))
	     ((zerop range)
	      (format t "zero range -> ~a ~a~%" 0 1)
	      (contour-offset 0 depth minus-half-r))
	     ((minusp range)
	      (format t "minus range Depth: ~a~%Range:~a  -  ~a -> ~a~%~a // ~a~%"
		      depth range ele-0 ele-1
		      #1=(if (<= depth (abs range))
			     (- (abs range)
				depth)
			     0)
		      #2=(if (<= depth (abs range))
			     range
			     (+ (* (signum range)
				   (- depth (abs range)))
				range)))
	      (contour-offset
	       #|
	       (if (<= depth (abs range))
		   (- (abs range)
		      depth)
		   0)
	       (if (<= depth (abs range))
		   range
		   (+ (* (signum range)
			 (- depth (abs range)))
	       range))|#
	       #1# #2#
	       minus-half-r)))))))

(defun water-offset-left (depth water-at t-l-ele l-b-ele hex-r)
  (declare ((member :start :end) water-at))
  (let ((half-r (/ hex-r 2.0))
	(range (- l-b-ele t-l-ele)))
    (case water-at
      (:END
       (format t "LEFT LEFT LEFT Water at END~%")
       (cond ((plusp range)
	      (format t "plusp range ~a ~a~%" (1- depth) range)
	      ;; Great!
	      (contour-offset (1- (- depth range))
			      (* 2 (+ range
				      (- depth
					 range)))
			      half-r))
	     ((zerop range)
	      (format t "zero range -> ~a ~a~%" 0 1)
	      (contour-offset (1- depth) depth half-r))
	     ((minusp range)
	      (format t "minusp range ~a ~a~%" depth range)
	      (if (<= depth (abs range))
		  (contour-offset (1- depth) (abs range) half-r)
		  (contour-offset (1- (+ (abs range) depth))
				  (+ (abs range) depth)
				  half-r))
	      ;(contour-offset (1- depth) range half-r)
	      )))
      (:START
       (format t "LEFT LEFT LEFT Water at START~%")
       (cond ((plusp range)
	      (format t "plusp range ~a ~a~%" (1- depth) range)
	      ;;(setf range (* depth range 2))
	      (contour-offset (1- (- range (1- depth))) ;; ?
			      range half-r)
	      (contour-offset
	       (if (<= depth (abs range))
			     (- (abs range)
				depth)
			     0)
	       (if (<= depth (abs range))
		   range
		   (+ (* (signum range)
			 (- depth (abs range)))
		      range))
	       half-r))
	     ((zerop range)
	      (format t "zero range -> ~a ~a~%" 0 1)
	      (contour-offset 0 depth half-r))
	     ((minusp range)
	      (format t "minus range Depth: ~a Range:~a~%" (1- depth) range)
	      (setf range (* depth (abs range) 2))
	      (prog1
		  (contour-offset
		   #1=(1- (- range (1- depth)))
		   #2=range
		   half-r)
		(format t "   -> ~a : ~a ~%" #1# #2#))))))))

(defun abs-max (number &rest more-numbers)
  "Returns the number that has the greatest absolute value."
  (let ((max number)
	(abs-max (abs number)))
    (dolist (num more-numbers)
      (let ((abs-num (abs num)))
	(when (> abs-num abs-max)
	  (setf max num
		abs-max abs-num))))
    max))

(defun abs-increment (num &optional (delta 1))
  "Returns NUM incremented by DELTA away from 0."
  (+ num (* delta (signum num))))
