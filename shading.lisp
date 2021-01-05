(in-package :hexhammer)

(defparameter *light-vector* (surface-normal (crd 0 0) 0 (crd 0.2 1) 0 (crd 0.2 1) 1))

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
			    (* (mod (1- (x crd)) 2)
			       half-down-y))))

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
			,@body))))

	(cairo:with-context (cairo-context)
	  (cairo:set-line-width 0.0)
	  (cairo:set-antialias :none)

	  (let ((dull-y 0.5))
	    (triangle-bounds
	     (:s :sse)
	     (draw-gouraud-tri
	      dull-y sse s cen
	      r hex-centre-x hex-centre-y (* +sf-pi+ 3/6) cairo-context))
	    (triangle-bounds
	     (:se :e)
	     (draw-gouraud-tri
	      dull-y e se cen
	      r hex-centre-x hex-centre-y (* +sf-pi+ 1/6) cairo-context))
	    (triangle-bounds
	     (:ne :nne)
	     (draw-gouraud-tri
	      dull-y nne ne cen
	      r hex-centre-x hex-centre-y (* +sf-pi+ -1/6) cairo-context))
	    (triangle-bounds
	     (:n :nnw)
	     (draw-gouraud-tri
	      dull-y nnw n cen
	      r hex-centre-x hex-centre-y (* +sf-pi+ -3/6) cairo-context))
	    (triangle-bounds
	     (:nw :w)
	     (draw-gouraud-tri
	      dull-y w nw cen
	      r hex-centre-x hex-centre-y (* +sf-pi+ -5/6) cairo-context))
	    (triangle-bounds
	     (:sw :ssw)
	     (draw-gouraud-tri
	      dull-y ssw sw cen
	      r hex-centre-x hex-centre-y (* +sf-pi+ -7/6) cairo-context)))
	  
	  (let ((dull-y -0.5))
	    (triangle-bounds
	     (:ssw :s)
	     (draw-gouraud-tri
	      dull-y ssw s cen
	      r hex-centre-x hex-centre-y (* +sf-pi+ 3/6) cairo-context))
	    (triangle-bounds
	     (:sse :se)
	     (draw-gouraud-tri
	      dull-y sse se cen
	      r hex-centre-x hex-centre-y (* +sf-pi+ 1/6) cairo-context))
	    (triangle-bounds
	     (:e :ne)
	     (draw-gouraud-tri
	      dull-y e ne cen
	      r hex-centre-x hex-centre-y (* +sf-pi+ -1/6) cairo-context))
	    (triangle-bounds
	     (:nne :n)
	     (draw-gouraud-tri
	      dull-y nne n cen
	      r hex-centre-x hex-centre-y (* +sf-pi+ -3/6) cairo-context))
	    (triangle-bounds
	     (:nnw :nw)
	     (draw-gouraud-tri
	      dull-y nnw nw cen
	      r hex-centre-x hex-centre-y (* +sf-pi+ -5/6) cairo-context))
	    (triangle-bounds
	     (:w :sw)
	     (draw-gouraud-tri
	      dull-y w sw cen
	      r hex-centre-x hex-centre-y (* +sf-pi+ 5/6) cairo-context)))
	  )))))

(defun value-alpha-value (value)
  ;; 0 = no shadow, 1 = total shadow
  (- 1.0 value))

(defun shading-color-stop (gradient offset value)
  (cairo:pattern-add-color-stop-rgba
   gradient offset
   0 0 0 ; Shading is always black, just like real life.
   (value-alpha-value value))) ; white light reveals true color

(defun construct-gradient (value-0 value-1 gradient)
  (shading-color-stop gradient 0.0 value-0)
  (shading-color-stop gradient 1.0 value-1))

(defun relevant-trunc (num)
  (ffloor num 0.000001)) ; ffloor appears to be the fastest of these functions

(defun rf= (number &rest more-numbers)
  (apply #'=
	 (mapcar #'relevant-trunc
		 (cons number more-numbers))))

(defun draw-gouraud-tri (dull-y dull-value right-value origin-value
			 r hex-centre-x hex-centre-y rotation context)
  (let* ((rx +sin60+)
	 (0r-edge (- right-value origin-value))
	 (rd-edge (- dull-value right-value))
	 (d0-edge (- origin-value dull-value))
	 (abs-0r (abs 0r-edge))
	 (abs-rd (abs rd-edge))
	 (abs-d0 (abs d0-edge)))

    ;; We have right triangles with corners O origin(sharp), R right and D dull.
    ;; Points have values associated with them. These are max, min and middle.
    ;; Draw line from middle point to it's value's offset on min-max edge to get
    ;; slope k.
    ;; Perpendicular to this slope draw line from either dull or right corner,
    ;; whichever is NOT the middle corner. This is the gradient's line.
    ;; From the remaining corner draw a line with slope k to intersect with
    ;; previous to find target.
    
    (cond ((rf= dull-value right-value origin-value)
	   (cairo:set-source-rgba
	    0 0 0 (value-alpha-value dull-value) context)
	   (cairo:fill-path context))

	  ((rf= dull-value right-value)
	   (let* ((right-xy (rotate (crd rx 0) rotation))
		  (gradient (cairo:create-linear-pattern
			     (+ hex-centre-x (* r (x right-xy)))
			     (+ hex-centre-y (* r -1 (y right-xy)))
			     hex-centre-x
			     hex-centre-y)))
	     (shading-color-stop gradient 0.0 right-value)
	     (shading-color-stop gradient 1.0 origin-value)
	     (cairo:set-source gradient context)
	     (cairo:fill-path context)
	     (cairo:destroy gradient)))

	  ((rf= right-value origin-value)
	   (let* ((right-xy (rotate (crd rx 0) rotation))
		  (target-xy (rotate (crd rx dull-y) rotation))
		  (gradient (cairo:create-linear-pattern
			     (+ hex-centre-x (* r (x right-xy)))
			     (+ hex-centre-y (* r -1 (y right-xy)))
			     (+ hex-centre-x (* r (x target-xy)))
			     (+ hex-centre-y (* r -1 (y target-xy))))))
	     (construct-gradient right-value dull-value gradient)
	     (cairo:set-source gradient context)
	     (cairo:fill-path context)
	     (cairo:destroy gradient)))

	  ;;((rf= dull-value origin-value))

	  ((>= abs-0r (max abs-rd abs-d0)) ; rv/=0v , dv/=rv
	   (let* ((k (/ dull-y
		       (- rx (* (/ (- dull-value origin-value)
				   (- right-value origin-value))
				rx))))
		  (right-xy (rotate (crd rx 0) rotation))
		  (target-x (/ rx (+ 1 (* k k))))
		  (target-y (* k target-x))
		  (target-xy (rotate (crd target-x target-y)
				     rotation))
		  (gradient (cairo:create-linear-pattern
			     (+ hex-centre-x (* r (x right-xy)))
			     (+ hex-centre-y (* r -1 (y right-xy)))
			     (+ hex-centre-x (* r (x target-xy)))
			     (+ hex-centre-y (* r -1 (y target-xy))))))
	     (construct-gradient right-value origin-value gradient)
	     (cairo:set-source gradient context)
	     (cairo:fill-path context)
	     (cairo:destroy gradient)))

	  ((> abs-d0 (max abs-rd abs-0r))
	   ;; dv/=0v, rv/=0v
	   (let* ((value-ratio (/ (- right-value origin-value)
				  (- dull-value origin-value)))
		  (k (/ (- (* dull-y value-ratio))
			(- rx (* rx value-ratio))))
		  (dull-xy (rotate (crd rx dull-y) rotation))
		  (target-x (/ (+ (* k dull-y)
				  rx)
			       (+ 1 (* k k))))
		  (target-y (* k target-x))
		  (target-xy (rotate (crd target-x target-y)
				     rotation))
		  (gradient (cairo:create-linear-pattern
			     (+ hex-centre-x (* r (x dull-xy)))
			     (+ hex-centre-y (* r -1 (y dull-xy)))
			     (+ hex-centre-x (* r (x target-xy)))
			     (+ hex-centre-y (* r -1 (y target-xy))))))
	     (construct-gradient dull-value origin-value gradient)
	     (cairo:set-source gradient context)
	     (cairo:fill-path context)
	     (cairo:destroy gradient)))

	  ((> abs-rd (max abs-d0 abs-0r))
	   ;; this one could be moved to after (= dull-value right-value)
	   ;; dv/=rv
	   (let* ((value-ratio (/ (- origin-value right-value)
				  (- dull-value right-value)))
		  (k (/ (* dull-y value-ratio)
			rx))
		  (right-xy (rotate (crd rx 0) rotation))
		  (target-x (/ (- (* rx (+ 1 (* k k)))
				  (* k dull-y))
			       (+ 1 (* k k))))
		  (target-y
		    (+ (* k target-x) dull-y (- (* k rx))))
		  (target-xy (rotate (crd target-x target-y)
				     rotation))
		  (gradient (cairo:create-linear-pattern
			     (+ hex-centre-x (* r (x right-xy)))
			     (+ hex-centre-y (* r -1 (y right-xy)))
			     (+ hex-centre-x (* r (x target-xy)))
			     (+ hex-centre-y (* r -1 (y target-xy))))))
	     (construct-gradient right-value dull-value gradient)
	     (cairo:set-source gradient context)
	     (cairo:fill-path context)
	     (cairo:destroy gradient))))))


;; flat shading:
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
