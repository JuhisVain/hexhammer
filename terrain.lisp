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

	(let ((top (point-edge-terrain hex :ne :nne))
	      (left (point-edge-terrain hex :nne :n))
	      (bottom (point-edge-terrain hex :n :cen))
	      (right (point-edge-terrain hex :cen :ne))
	      (angle (* 3/6 +sf-pi+)))
	  (draw-kite-terrain top left bottom right
			     angle hex-centre-x hex-centre-y
			     (hex-r view-state) cairo-context))

	(let ((top (point-edge-terrain hex :se :e))
	      (left (point-edge-terrain hex :e :ne))
	      (bottom (point-edge-terrain hex :ne :cen))
	      (right (point-edge-terrain hex :cen :se))
	      (angle (* 1/6 +sf-pi+)))
	  (draw-kite-terrain top left bottom right
			     angle hex-centre-x hex-centre-y
			     (hex-r view-state) cairo-context))

	(let ((top (point-edge-terrain hex :s :sse))
	      (left (point-edge-terrain hex :sse :se))
	      (bottom (point-edge-terrain hex :se :cen))
	      (right (point-edge-terrain hex :cen :s))
	      (angle (* -1/6 +sf-pi+)))
	  (draw-kite-terrain top left bottom right
			     angle hex-centre-x hex-centre-y
			     (hex-r view-state) cairo-context))

	(let ((top (point-edge-terrain hex :sw :ssw))
	      (left (point-edge-terrain hex :ssw :s))
	      (bottom (point-edge-terrain hex :s :cen))
	      (right (point-edge-terrain hex :cen :sw))
	      (angle (* -3/6 +sf-pi+)))
	  (draw-kite-terrain top left bottom right
			     angle hex-centre-x hex-centre-y
			     (hex-r view-state) cairo-context))

	(let ((top (point-edge-terrain hex :nw :w))
	      (left (point-edge-terrain hex :w :sw))
	      (bottom (point-edge-terrain hex :sw :cen))
	      (right (point-edge-terrain hex :cen :nw))
	      (angle (* -5/6 +sf-pi+)))
	  (draw-kite-terrain top left bottom right
			     angle hex-centre-x hex-centre-y
			     (hex-r view-state) cairo-context))

	)
      (cairo:destroy cairo-context)
      (cairo:destroy cairo-surface))))

;; Insert some test borders in there:
(defun test-terrain ()

  ;; Reset screw-ups:
  (dolist (v +vertex-directions+)
    (dolist (crd (list (crd 1 1)
		       (crd 3 2)
		       (crd 1 3)
		       (crd 5 1)
		       (crd 4 0)
		       (crd 0 0)))
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
	  (list (cons 'forest 'dry))))

  ;; top-left-corner:
  (dolist (v (list :nw :ne :se :sw))
    (setf (point-terrain (hex-vertex (hex-at (crd 5 1) *world*) v))
	  (list (cons 'forest 'dry))))

  (dolist (v (list :nnw :e :ssw))
    (setf (point-terrain (hex-vertex (hex-at (crd 5 1) *world*) v))
	  (list (cons 'lake 'dry))))

  ;;top
  (dolist (v (list :nnw :n :e :se :ssw :sw))
    (setf (point-terrain (hex-vertex (hex-at (crd 4 0) *world*) v))
	  (list (cons 'forest 'dry))))

  (dolist (v (list :cen :nw :n :ne :se :s :sw))
    (setf (point-terrain (hex-vertex (hex-at (crd 0 0) *world*) v))
	  (list (cons 'forest 'dry)))))

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

(defun test-rounded ()
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
		    hex-centre-x hex-centre-y))

	 ;; rotation later:
	 (kite-mid (crd half-kite-long (/ half-r 2)))
	 (b-mid (crd half-kite-long 0))
	 (l-mid (crd half-down-y (/ half-r 2)))
	 (t-mid (crd half-down-y (/ half-r -2)))
	 (r-mid (crd half-kite-long 0)))
    
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

      (rotation (kite-mid b-mid l-mid) (t-mid r-mid))
      
      (cairo:with-context (cairo-context)
	(cairo:set-source-rgb 1.0 0.0 1.0)
	(cairo:set-line-width 0.5)
	(cairo:set-antialias :none)

	(flet ((priority-at-cen (left top right cen)
		 ;; If left and right are same there will be no border
		 ;; if different the border will be a straight line:
		 (render-terrain-path-form
		  left nil
		  (t-l-corner l-b-corner b-r-corner))
		 (render-terrain-path-form
		  right nil
		  (t-l-corner b-r-corner r-t-corner))
		 
		 ;; Need to draw top's terrain?
		 (unless (eq top left) 
		   (let ((xy1 (crd (- half-down-y
				      (* 0.67 (/ half-r 2.0)))
				   (/ half-r 2.0)))
			 (xy2 (crd (- half-down-y
				      (* 0.67 (/ half-r 2.0)))
				   (/ half-r -2.0))))
		     (rotation (xy1) (xy2))

		     (cairo:set-source-rgb 1.0 0.0 0.7)
		     (cairo:move-to (x l-mid) (y l-mid))
		     (cairo:line-to (x xy1) (y xy1))
		     (cairo:line-to (x xy2) (y xy2))
		     (cairo:line-to (x t-mid) (y t-mid))
		     (cairo:set-line-width 1.0)
		     (cairo:stroke)
		     
		     (cairo:move-to (x l-mid) (y l-mid))
		     (cairo:curve-to (x xy1) (y xy1)
				     (x xy2) (y xy2)
				     (x t-mid) (y t-mid))
		     (cairo:line-to (x t-l-corner) (y t-l-corner))
		     (cairo:close-path)
		     (set-terrain-fill top)
		     ;;(set-terrain-line cen)
		     ;; No line! 
		     (cairo:fill-path)))

		 ;; Drawing priority terrain:
		 ;; May need to shift control points based on elevation differences
		 (let ((xy1 (crd half-kite-long
				 (* 0.36 half-kite-long)))
		       (xy2 (crd half-kite-long
				 (* -0.36 half-kite-long))))
		   (rotation (xy1) (xy2))
		   (cairo:move-to (x b-mid) (y b-mid))
		   (cairo:curve-to (x xy1) (y xy1)
				   (x xy2) (y xy2)
				   (x r-mid) (y r-mid))
		   (cairo:line-to (x b-r-corner) (y b-r-corner))
		   (cairo:close-path)
		   (set-terrain-fill cen)
		   ;;(set-terrain-line cen)
		   ;; No line! 
		   (cairo:fill-path)))

	       (render-natural-terrain (top left bottom right)
		 (symbol-macrolet ((kite-perimeter
				     (progn
				       (cairo:move-to (x t-l-corner) (y t-l-corner))
				       (cairo:line-to (x l-b-corner) (y l-b-corner))
				       (cairo:line-to (x b-r-corner) (y b-r-corner))
				       (cairo:line-to (x r-t-corner) (y r-t-corner))
				       (cairo:close-path))))
		   (let ((selector (+ (if (terrain-naturalp top) 8 0)
				      (if (terrain-naturalp left) 4 0)
				      (if (terrain-naturalp bottom) 2 0)
				      (if (terrain-naturalp right) 1 0))))

		     (format t "Selector ~a~%" selector)
		     
		     (case selector
		       (0 ; No natural land terrain
			nil)
		       ((1 2 4 8) ; No natural land terrain boundaries
			(format t "WOW ~a~%" selector)
			kite-perimeter
			(set-terrain-fill (case selector
					    (1 right)
					    (2 bottom)
					    (4 left)
					    (8 top)))
			(cairo:fill-path))
		       (3 ; bottom & right
			(format t "BOTTOM & RIGHT!~%")
			(cond ((eq bottom right)
			       kite-perimeter
			       (set-terrain-fill bottom)
			       (cairo:fill-path))
			      (t;;(or (terrain-artificialp left)
				;;    (terrain-artificialp top))
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
			       (cairo:fill-path)))))))))
	  
	  ;;;; 3^4 = 81 permutations, have to render in layers
	  (let ((top (car left)) ;; kite verts
		(left (car bottom))
		(bottom (car right))
		(right (car top)))
	    (render-natural-terrain top left bottom right)
	    ;(render-artificial-terrain top left bottom right)
	    ;(render-water-terrain top left bottom right)
	    )
	  
	  #|
	  (when (and (equal (car bottom) (car right))
		     (equal (car bottom) (car top))
		     (equal (car bottom) (car left)))
	  ;;; Whole kite is of same terrain type
	    (path-through b-r-corner r-t-corner t-l-corner l-b-corner)
	    (render-terrain-path (car bottom))
	    (return-from draw-kite-terrain))

	  (when (and (eq 'lake (car right))
		     (not (member 'lake (list (car left) (car top) (car bottom)))))
	    (priority-at-cen (car left) (car top) (car bottom) (car right)))
	  |#
	  
	  
          #|
	  (when (terrain-borderp bottom)
	    (let ((probe (car bottom)))
	      (cond ((and (terrain-borderp left)
			  (eq probe (cadr left)))
		     ;; bottom-left corner
		     (let ((terrain-type (car bottom)))
		       ;;(path-through b-mid kite-mid l-mid l-b-corner)
		       ;;(render-terrain-path terrain-type)

		       '(rtp terrain-type
			 b-mid kite-mid l-mid l-b-corner)
		       
		       (render-terrain-path-form
			terrain-type
			nil
			(b-mid kite-mid l-mid l-b-corner))
		       ))
		    ((and (terrain-borderp top)
			  (eq probe (cadr top)))
		     ;; left side
		     (let ((terrain-type (car bottom)))
		       (path-through b-mid kite-mid t-mid t-l-corner l-b-corner)
		       (render-terrain-path terrain-type)))
		    ((and (terrain-borderp right)
			  (eq probe (cadr right)))
		     ;; all except bottom right corner
		     (format t "left-side and top-right~%")
		     (let ((terrain-type (car bottom)))
		       (path-through b-mid kite-mid r-mid r-t-corner t-l-corner l-b-corner)
		       (render-terrain-path terrain-type))))))
	  
	  (when (terrain-borderp left)
	    (let ((probe (car left)))
	      (cond ((and (terrain-borderp top)
			  (eq probe (cadr top)))
		     (let ((terrain-type (car left)))
		       (path-through l-mid kite-mid t-mid t-l-corner)
		       (render-terrain-path terrain-type)))
		    ((and (terrain-borderp right)
			  (eq probe (cadr right)))
		     (let ((terrain-type (car left)))
		       (path-through l-mid kite-mid r-mid r-t-corner t-l-corner)
		       (render-terrain-path terrain-type)))
		    ((and (terrain-borderp bottom)
			  (eq probe (cadr bottom)))
		     (let ((terrain-type (car left)))
		       (path-through l-mid kite-mid b-mid b-r-corner r-t-corner t-l-corner)
		       (render-terrain-path terrain-type))))))
	  
	  (when (terrain-borderp top)
	    (let ((probe (car top)))
	      (cond ((and (terrain-borderp right)
			  (eq probe (cadr right)))
		     (let ((terrain-type (car top)))
		       (path-through t-mid kite-mid r-mid r-t-corner)
		       (render-terrain-path terrain-type)))
		    ((and (terrain-borderp bottom)
			  (eq probe (cadr bottom)))
		     (let ((terrain-type (car top)))
		       (path-through t-mid kite-mid b-mid b-r-corner r-t-corner)
		       (render-terrain-path terrain-type)))
		    ((and (terrain-borderp left)
			  (eq probe (cadr left)))
		     (let ((terrain-type (car top)))
		       (path-through t-mid kite-mid l-mid l-b-corner b-r-corner r-t-corner)
		       (render-terrain-path terrain-type))))))

	  (when (terrain-borderp right)
	    (let ((probe (car right)))
	      (cond ((and (terrain-borderp bottom)
			  (eq probe (cadr bottom)))
		     (let ((terrain-type (car right)))
		       (path-through r-mid kite-mid b-mid b-r-corner)
		       (render-terrain-path terrain-type)))
		    ((and (terrain-borderp left)
			  (eq probe (cadr left)))
		     (let ((terrain-type (car right)))
		       (path-through r-mid kite-mid l-mid l-b-corner b-r-corner)
		       (render-terrain-path terrain-type)))
		    ((and (terrain-borderp top)
			  (eq probe (cadr top)))
		     (let ((terrain-type (car right)))
		       (path-through r-mid kite-mid t-mid t-l-corner l-b-corner b-r-corner)
		       (render-terrain-path terrain-type))))))
	  |#
	  )))))
