(in-package :hexhammer)

;;; NOTE: Bezier control points inside hex at 50%
;; of distance between verts or R, whichever longer. (should say shorter??)

;;; Rivers should live in the same hashtable as other paths
;; However the construction of infrastructure should not affect
;; the graphical position of rivers.

;; key: crd
(defvar *crd-paths* (make-hash-table :test 'equalp))
(defvar *river-rad* 0.05
  "Multiply this by hex radius to get HALF of river width.")

(defstruct river
  (dir nil :type hex-vertex)
  (size 1.0) ; maybe 1.0 = half r ?
  (crd nil :type crd))

(defclass crd-paths ()
  ((rivers ;; ((exit-dir size exit-to-crd) (entry-dir size entry-from-crd)... )
    :initarg :rivers
    :accessor crd-paths-rivers)
   (infra
    :initarg :infra
    :accessor crd-paths-infra)))

(defun rivers-exit (crd-paths)
  (car (crd-paths-rivers crd-paths)))

(defun river-entries (crd-paths)
  (cdr (crd-paths-rivers crd-paths)))

(defun rivers-master-entry (crd-paths)
  (cadr (crd-paths-rivers crd-paths)))

(defun destination-masterp (crd)
  "Is CRD's exit-river's counterpart it's destination's master entry?"
  (let* ((crd-paths (gethash crd *crd-paths*))
	 (exit (rivers-exit crd-paths))
	 (entry-crd (river-crd exit)))
    (eq
     (vertex-alias crd (river-dir exit) entry-crd)
     (river-dir
      (rivers-master-entry (gethash entry-crd *crd-paths*))))))

(defun rivers-sub-entries (crd-paths)
  (cddr (crd-paths-rivers crd-paths)))

(defun make-crd-paths (&key rivers infra)
  (make-instance 'crd-paths :rivers rivers :infra infra))

(defun define-crd-paths (crd &key rivers infra)
  (setf (gethash crd *crd-paths*)
	(make-crd-paths :rivers rivers :infra infra)))

(defun add-river-entry (crd dir from size world)
  "Add a river entry to coordinate CRD coming from
coordinate FROM through DIR in CRD."
  (when (not (gethash crd (world-map world)))
    (return-from add-river-entry))
  (let ((crd-paths (gethash crd *crd-paths*)))
    (if (null crd-paths)
	(define-crd-paths crd
	  :rivers (list nil
			(make-river :dir dir :size size :crd from)) ;(list nil (cons dir from))
	  )
	(if (not (member dir (crd-paths-rivers crd-paths)
			 :key #'river-dir))
	    (setf (crd-paths-rivers crd-paths)
		  (nconc (crd-paths-rivers crd-paths)
			 ;;(list (cons dir from))
			 (list (make-river :dir dir :size size :crd from))
			 ))
	    (error
	     "Trying to push duplicate river entry vertex ~a to rivers ~a~%"
	     (cons dir from) (crd-paths-rivers crd-paths))))))

(defun add-river-exit (crd dir to size world)
  "Add a river exit to coordinate CRD exiting at CRD's DIR to coordinate TO."
  (when (not (gethash crd (world-map world)))
    (return-from add-river-exit))
  (let ((crd-paths (gethash crd *crd-paths*)))
    (if (null crd-paths)
	(define-crd-paths crd
	  :rivers (list (make-river :dir dir :size size :crd to)) ;(cons (cons dir to) nil)
	  )
	(if (and (null (car (crd-paths-rivers crd-paths)))
		 (not (member dir (cdr (crd-paths-rivers crd-paths))
			      :key #'river-dir)))
	    (rplaca (crd-paths-rivers crd-paths)
		    ;;(cons dir to)
		    (make-river :dir dir :size size :crd to)
		    )
	    (error
	     "Trying to set invalid river exit vertex ~a to rivers ~a~%"
	     (cons dir to) (crd-paths-rivers crd-paths)))))
  (add-river-entry to (vertex-alias crd dir to) crd size world))

(defun plumruntest (&optional (world *world*))
  (clrhash *crd-paths*)
  (macrolet
      ((river-exits (&rest instructions)
	 `(progn
	    ,@(loop for (x0 y0 dir x1 y1 size)
		      on instructions
		    by #'(lambda (x) (nthcdr 6 x))
		    collect `(add-river-exit (crd ,x0 ,y0)
					     ,dir
					     (crd ,x1 ,y1)
					     ,(coerce size 'single-float)
					     world)))))
    (river-exits
     46 41 :s 46 40 0.1
     46 40 :sw 45 39  0.1
     45 39 :s 45 38  0.1
     45 38 :sw 44 38  0.1
     44 38 :sw 43 37  0.1
     43 37 :s 43 36  0.1
     43 36 :sw 42 36 0.1)
    ))

(defun rivertest (&optional (world *world*))
  (clrhash *crd-paths*)
  (macrolet
      ((river-exits (&rest instructions)
	 `(progn
	    ,@(loop for (x0 y0 dir x1 y1 size)
		      on instructions
		    by #'(lambda (x) (nthcdr 6 x))
		    collect `(add-river-exit (crd ,x0 ,y0)
					     ,dir
					     (crd ,x1 ,y1)
					     ,(coerce size 'single-float)
					     world)))))

    (river-exits
     8 14 :s 8 13 0.1
     8 13 :s 8 12 0.1
     8 12 :e 9 11 0.1
     9 11 :s 9 10 0.1
     9 10 :s 9 9 0.2
     9 9 :ssw 9 8 0.2
     9 8 :ssw 8 8 0.2
     8 8 :se 9 7 0.3
     9 7 :sse 9 6 0.3
     9 6 :w 8 6 0.4
     8 6 :ssw 7 5 0.4
     7 5 :nw 6 6 0.7
     6 6 :ssw 6 5 0.7
     6 5 :sw 5 4 0.8
     5 4 :nw 4 5 0.8
     4 5 :w 3 4 0.8
     3 4 :e 4 4 1.1
     4 4 :sse 5 3 1.2
     5 3 :sw 4 3 1.2
     4 3 :se 5 2 1.2
     5 2 :nne 6 3 1.3
     6 3 :e 7 2 1.3

     6 15 :sse 6 14 0.1
     6 14 :w 5 13 0.1
     5 13 :s 5 12 0.1
     5 12 :sse 5 11 0.2
     5 11 :e 6 11 0.2
     6 11 :se 7 10 0.2
     7 10 :s 7 9 0.2
     7 9 :s 7 8 0.3
     7 8 :s 7 7 0.3
     7 7 :s 7 6 0.3
     7 6 :s 7 5 0.4

     2 6 :sse 3 5 0.3
     3 5 :s 3 4 0.3

     3 3 :n 3 4 0.2)

    ;; TODO: A river exiting to off map is OK, but if exiting to water body
    ;; or "nowhere" will need to do something
    ;(remhash (crd 7 2) *crd-paths*)
    ))

;; This one could be sped up by interpreting the dirs as integers
(defun right-or-left (this-0 this-1 from-0 from-1)
  "Compares a path (THIS-0 -> THIS-1) to path (FROM-0 -> FROM-1)
where variables are hex border directions and returns THIS path's
relative direction from FROM path when FROM path is looking towards FROM-1."
  (declare ((and hex-vertex
		 (not (eql :cen)))
	    this-0 this-1 from-0 from-1))
  (let* ((all-dirs (dir-list from-0 13 t)) ; 13 to include start
	 (right-dirs (member from-1 all-dirs))
	 (exclusive-right-dirs (butlast (rest right-dirs))))
    (cond ((or (and (eq this-0 from-0)
		    (eq this-1 from-1))
	       (and (eq this-0 from-1)
		    (eq this-1 from-0)))
	   :same) ; will have to check existing waypoint's order in this case
	  ((and (find this-0 right-dirs)
		(find this-1 right-dirs))
	   :right)
	  ((or (and (find this-0 exclusive-right-dirs)
		    (not (find this-1 exclusive-right-dirs)))
	       (and (find this-1 exclusive-right-dirs)
		    (not (find this-0 exclusive-right-dirs))))
	   :cross) ; rivers don't cross
	  (t :left))))

(defun graphical-path-centre (entry-crd exit-crd r x-cen y-cen)
  (let* ((yd (- (- (y entry-crd) (y exit-crd))))
	 (xd (- (x entry-crd) (x exit-crd)))
	 
	 (half-distance (/ (sqrt (+ (expt yd 2)
				    (expt xd 2)))
			   2))
	 ;; midpoint is the middle of line from entry to exit:
	 (midpoint-x (+ (x exit-crd) (/ xd 2)))
	 (midpoint-y (+ (y entry-crd) (/ yd 2)))
	 ;; This is the angle from hex centre to midpoint:
	 (angle (atan midpoint-y midpoint-x))

	 (centre-crd (ntranslate (crd (* (cos angle)
					 ;; use either r or sin60*r :
					 (- r half-distance))
				      (* (sin angle)
					 (- r half-distance)))
				 x-cen y-cen)))
    centre-crd))

(defun crd-graphical-centre (x y view-state)
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
			  (* x three-halfs-r)))
	 (hex-centre-y (+ (- window-centre-y-pix
			     (+ origin-y
				(* y full-down-y)))
			  (- half-down-y)
			  window-centre-y-pix
			  (* -1 full-down-y)
			  (* (mod (1- x) 2)
			     half-down-y))))
    (crd hex-centre-x hex-centre-y)))

(defun crd-centre-river-angle (crd)
  "Compute crosswise angle of river path at river's central position at map
coordinate CRD. Returns angle in radians to right side looking downstream."
  (let* ((crd-paths (gethash crd *crd-paths*))
	 (master-entry-river (when crd-paths (rivers-master-entry crd-paths)))
	 (master-entry-crd
	   (vertex-crd 1.0 ; radius doesn't matter
		       (if master-entry-river
			   (river-dir master-entry-river)
			   :CEN)))
	 (exit-river
	   (when crd-paths (rivers-exit crd-paths)))
	 (exit-crd
	   (vertex-crd 1.0
		       (if exit-river
			   (river-dir exit-river)
			   :CEN))))
    (+ (/ +sf-pi+ 2)
       (atan (- (- (y master-entry-crd) (y exit-crd)))
	     (- (x master-entry-crd) (x exit-crd))))))

(defun river-centre-size (crd)
  "Calculates width of river at centre of hex at CRD."
  (let ((crd-paths (gethash crd *crd-paths*)))
    ;; TODO: Redo. both sizes should be same if water body at either hex centre 
    (if crd-paths
	(/ (+ (if (rivers-exit crd-paths)
		  (river-size (rivers-exit crd-paths))
		  0.0)
	      (if (rivers-master-entry crd-paths)
		  (river-size (rivers-master-entry crd-paths))
		  0.0))
	   2)
	0.0)))

(defun draw-rivers (crd world view-state)
  (let* ((cairo-surface
	   (cairo:create-image-surface-for-data
	    (buffer view-state) :argb32
	    (width view-state) (height view-state)
	    (* 4 (width view-state))))
	 (cairo-context (cairo:create-context cairo-surface))
	 (crd-paths (gethash crd *crd-paths*)))
    
    (when (not crd-paths)
      (return-from draw-rivers))
    
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
			    (* (mod (1- (x crd)) 2)
			       half-down-y))))

      (cairo:with-context (cairo-context)
	(cairo:set-source-rgb 0.0 0.1 0.8)
	(cairo:set-line-width 1.0)
	;; if there is no exit, this river has already been drawn:
	(when (rivers-exit crd-paths)
	  (let* ((exit-river (rivers-exit crd-paths))
		 (exit-dir (river-dir exit-river))
		 (exit-crd-act (river-crd exit-river)) ;; map coordinates
		 (exit-crd (vertex-crd r exit-dir)) ;; screen coordinates rel to hex centre
		 (master-entry-river (rivers-master-entry crd-paths))
		 (master-entry
		   (vertex-crd r (if master-entry-river
				     (river-dir (rivers-master-entry crd-paths))
				     :CEN)))
		 (river-centre-size (/ (+ (river-size exit-river)
					  (if master-entry-river
					      (river-size master-entry-river)
					      0.0))
				       2)))
	    
	    (let ((centre-crd 
		    (graphical-path-centre master-entry exit-crd
					   r hex-centre-x hex-centre-y)))
	      (let* ((centre-angle (crd-centre-river-angle crd))
		     
		     (centre-right-side
		       (nrotate (crd (* *river-rad* r river-centre-size) 0)
				nil (sin centre-angle) (cos centre-angle)))
		     (centre-left-side
		       (nrotate (crd (* (- *river-rad*) r river-centre-size) 0)
				nil (sin centre-angle) (cos centre-angle)))

		     (exit-crd-river (gethash exit-crd-act *crd-paths*))
		     (exit-hex-centre
		       (crd-graphical-centre
			(x exit-crd-act) (y exit-crd-act) view-state))
		     (exit-crd-centre
		       (graphical-path-centre
			(vertex-crd
			 r
			 ;; The master entry of exit-crd-act:
			 (if (rivers-master-entry exit-crd-river)
			     (river-dir (rivers-master-entry exit-crd-river))
			     :CEN))
			(vertex-crd r
				    (if (rivers-exit exit-crd-river)
					(river-dir
					 (rivers-exit exit-crd-river))
					:CEN))
			r (x exit-hex-centre) (y exit-hex-centre)))
		     (exit-angle (+ (/ +sf-pi+ 2)
				    (atan (- (- (y centre-crd) (y exit-crd-centre)))
					  (- (x centre-crd) (x exit-crd-centre)))))
		     (river-exit-size (river-size exit-river))
		     (exit-right-side
		       (nrotate (crd (* *river-rad* r river-exit-size) 0)
				nil (sin exit-angle) (cos exit-angle)))
		     (exit-left-side
		       (nrotate (crd (* (- *river-rad*) r river-exit-size) 0)
				nil (sin exit-angle) (cos exit-angle)))

		   ;;; If drawing exit hex's river in this hex
		     ;; river drawing will have to have separate iteration
		     ;; over displayed hexes

		     (exit-centre-angle
		       ;; Branches joining trunk may cause too thin river paths:
		       (let ((exit-centre-master-angle
			       (crd-centre-river-angle exit-crd-act)))
			 (if (and (not (destination-masterp crd))
				  (> (abs (- exit-centre-master-angle
					     exit-angle))
				     (/ +sf-pi+ 4)))
			     (+ exit-centre-master-angle
				(/ +sf-pi+ 2)) ; branch entry perpendicular to trunk
			     exit-centre-master-angle)))

		     (exit-centre-size (river-centre-size exit-crd-act))

		     (exit-centre-right-side
		       (nrotate (crd (* *river-rad* r exit-centre-size) 0)
				nil (sin exit-centre-angle) (cos exit-centre-angle)))
		     (exit-centre-left-side
		       (nrotate (crd (* (- *river-rad*) r exit-centre-size) 0)
				nil (sin exit-centre-angle) (cos exit-centre-angle)))

		     ;; Now for the bezier curve control points:
		     ;; Compute distance between path crds:
		     (right-c-e-dist (sqrt (+ (expt (- (+ (x centre-right-side)
							  (x centre-crd))
						       (+ (x exit-right-side)
							  (x exit-crd)
							  hex-centre-x))
						    2)
					      (expt (- (+ (y centre-right-side)
							  (y centre-crd))
						       (+ (y exit-right-side)
							  (y exit-crd)
							  hex-centre-y))
						    2))))
		     (dist-div 2.0)
		     ;; Relative to closest start/end point coordinates:
		     (r-c-e-cp1 (nrotate (crd (/ right-c-e-dist
						 dist-div)
					      0)
					 (+ centre-angle
					    (/ +sf-pi+ 2))))
		     (r-c-e-cp2 (nrotate (crd (- (/ right-c-e-dist
						    dist-div))
					      0)
					 (+ exit-angle
					    (/ +sf-pi+ 2))))

		     (right-e-ec-dist (sqrt (+ (expt (- (+ (x exit-right-side)
							   (x exit-crd)
							   hex-centre-x)
							(+ (x exit-centre-right-side)
							   (x exit-crd-centre)))
						     2)
					       (expt (- (+ (y exit-right-side)
							   (y exit-crd)
							   hex-centre-y)
							(+ (y exit-centre-right-side)
							   (y exit-crd-centre)))
						     2))))

		     (r-e-ec-cp1 (nrotate (crd (/ right-e-ec-dist
						  dist-div)
					       0)
					  (+ exit-angle
					     (/ +sf-pi+ 2))))
		     (r-e-ec-cp2 (nrotate (crd (- (/ right-e-ec-dist
						     dist-div))
					       0)
					  (+ exit-centre-angle
					     (/ +sf-pi+ 2))))

		     (left-ec-e-dist (sqrt (+ (expt (- (+ (x exit-left-side)
							  (x exit-crd)
							  hex-centre-x)
						       (+ (x exit-centre-left-side)
							  (x exit-crd-centre)))
						    2)
					      (expt (- (+ (y exit-left-side)
							  (y exit-crd)
							  hex-centre-y)
						       (+ (y exit-centre-left-side)
							  (y exit-crd-centre)))
						    2))))

		     (l-ec-e-cp1 (nrotate (crd (- (/ left-ec-e-dist
						  dist-div))
					       0)
					  (+ exit-centre-angle
					     (/ +sf-pi+ 2))))

		     (l-ec-e-cp2 (nrotate (crd (/ left-ec-e-dist
						     dist-div)
					       0)
					  (+ exit-angle
					     (/ +sf-pi+ 2))))

		     (left-e-c-dist (sqrt (+ (expt (- (+ (x centre-left-side)
							 (x centre-crd))
						      (+ (x exit-left-side)
							 (x exit-crd)
							 hex-centre-x))
						   2)
					     (expt (- (+ (y centre-left-side)
							 (y centre-crd))
						      (+ (y exit-left-side)
							 (y exit-crd)
							 hex-centre-y))
						   2))))

		     (l-e-c-cp1 (nrotate (crd (- (/ left-e-c-dist
						    dist-div))
					      0)
					 (+ exit-angle
					    (/ +sf-pi+ 2))))

		     (l-e-c-cp2 (nrotate (crd (/ left-e-c-dist
						 dist-div)
					      0)
					 (+ centre-angle
					    (/ +sf-pi+ 2)))))

		(cairo:set-source-rgb 0.0 0.1 0.8)
		;; Move to initial point centre-right-side:
		(cairo:move-to (x centre-crd) (y centre-crd))
		(cairo:rel-move-to (x centre-left-side) (y centre-left-side))

		;; Preclosing river section area boundary:
                (cairo:line-to (+ (x centre-right-side)
				  (x centre-crd))
			       (+ (y centre-right-side)
				  (y centre-crd)))

		(cairo:curve-to
		 ;; First control point
		 (+ (x centre-right-side)
		    (x centre-crd)
		    (x r-c-e-cp1))
		 (+ (y centre-right-side)
		    (y centre-crd)
		    (y r-c-e-cp1))
		 ;; Second control point:
		 (+ (x exit-right-side)
		    (x exit-crd)
		    hex-centre-x
		    (x r-c-e-cp2))
		 (+ (y exit-right-side)
		    (y exit-crd)
		    hex-centre-y
		    (y r-c-e-cp2))
		 ;; Edge point:
		 (+ (x exit-right-side)
		    (x exit-crd)
		    hex-centre-x)
		 (+ (y exit-right-side)
		    (y exit-crd)
		    hex-centre-y))
		
		(cairo:curve-to
		 ;; First cp:
		 (+ (x r-e-ec-cp1)
		    (x exit-right-side)
		    (x exit-crd)
		    hex-centre-x)
		 (+ (y r-e-ec-cp1)
		    (y exit-right-side)
		    (y exit-crd)
		    hex-centre-y)
		 ;; Second cp:
		 (+ (x r-e-ec-cp2)
		    (x exit-centre-right-side)
		    (x exit-crd-centre))
		 (+ (y r-e-ec-cp2)
		    (y exit-centre-right-side)
		    (y exit-crd-centre))
		 ;; Right side destination:
		 (+ (x exit-centre-right-side)
		    (x exit-crd-centre))
		 (+ (y exit-centre-right-side)
		    (y exit-crd-centre)))
		
		;; Close destination section boundary:
		(cairo:line-to (+ (x exit-centre-left-side)
				  (x exit-crd-centre))
			       (+ (y exit-centre-left-side)
				  (y exit-crd-centre)))

		(cairo:curve-to
		 (+ (x l-ec-e-cp1)
		    (x exit-centre-left-side)
		    (x exit-crd-centre))
		 (+ (y l-ec-e-cp1)
		    (y exit-centre-left-side)
		    (y exit-crd-centre))

		 (+ (x l-ec-e-cp2)
		    (x exit-left-side)
		    (x exit-crd)
		    hex-centre-x)
		 (+ (y l-ec-e-cp2)
		    (y exit-left-side)
		    (y exit-crd)
		    hex-centre-y)

		 (+ (x exit-left-side)
		    (x exit-crd)
		    hex-centre-x)
		 (+ (y exit-left-side)
		    (y exit-crd)
		    hex-centre-y))
		
		(cairo:curve-to
		 (+ (x l-e-c-cp1)
		    (x exit-left-side)
		    (x exit-crd)
		    hex-centre-x)
		 (+ (y l-e-c-cp1)
		    (y exit-left-side)
		    (y exit-crd)
		    hex-centre-y)

		 (+ (x l-e-c-cp2)
		    (x centre-left-side)
		    (x centre-crd))
		 (+ (y l-e-c-cp2)
		    (y centre-left-side)
		    (y centre-crd))

		 (+ (x centre-left-side)
		    (x centre-crd))
		 (+ (y centre-left-side)
		    (y centre-crd)))
	        
		(cairo:set-source-rgb 0.0 0.2 0.8)
		(cairo:set-line-width 1.0)

		(cairo:set-antialias :none)
		(cairo:fill-preserve)
		(cairo:set-antialias :default)
		(cairo:stroke))
	      
	      #| ;; centre to exit in reddish:
	      (cairo:set-source-rgb 1.0 0.2 0.8)
	      (cairo:move-to (x centre-crd) (y centre-crd))
	      (cairo:line-to (+ (x exit-crd) hex-centre-x)
			     (+ (y exit-crd) hex-centre-y))
	      (cairo:stroke)
	      
	      ;; entry to centre in off white:
	      (dolist (entry (river-entries crd-paths))
		(let* ((entry-dir (river-dir entry))
		       (entry-crd (vertex-crd r entry-dir hex-centre-x hex-centre-y)))
		  (cairo:set-source-rgb 1.0 1.0 0.8)
		  (cairo:move-to (x entry-crd) (y entry-crd))
		  (cairo:line-to (x centre-crd) (y centre-crd)))
		(cairo:stroke))
	      |#
	      )))))))

;; Now this is PRETTY similar to #'unit-hex-crd,
;; except that the coordinate system is Y-inverted...
(defun vertex-crd (r dir &optional (x+ 0) (y+ 0))
  (ntranslate
   (case dir
     (:N (crd 0.0 (* -1 r +sin60+)))
     (:NNE (nrotate (crd (* r +sin60+) (* 0.5 r))
		    (* 1/2 +sf-pi+)))
     (:NE (nrotate (crd (* r +sin60+) 0.0)
		   (* 1/6 +sf-pi+)))
     (:E (crd r 0.0))
     (:SE (nrotate (crd (* r +sin60+) 0.0)
		   (* -1/6 +sf-pi+)))
     (:SSE (nrotate (crd (* r +sin60+) (* -0.5 r))
		    (* -1/2 +sf-pi+)))
     (:S (crd 0.0 (* r +sin60+)))
     (:SSW (nrotate (crd (* r +sin60+) (* 0.5 r))
		    (* -1/2 +sf-pi+)))
     (:SW (nrotate (crd (* r +sin60+) 0.0)
		   (* 7/6 +sf-pi+)))
     (:W (crd (* -1 r) 0.0))
     (:NW (nrotate (crd (* r +sin60+) 0.0)
		   (* 5/6 +sf-pi+)))
     (:NNW (nrotate (crd (* r +sin60+) (* -0.5 r))
		    (* 1/2 +sf-pi+)))
     (:CEN (crd 0.0 0.0)))
   x+ y+))

;; Probably belongs somewhere else
(defun vertex-key (vert)
  "Returns a primary name for VERT to be used for identity in sets."
  (destructuring-bind (crd dir) vert
    (case dir
      ((:NNW :N)
       (let ((ncrd (crd-neighbour crd :N)))
	 (list ncrd (vertex-alias crd dir ncrd))))
      ((:NE :E :NNE)
       (let ((ncrd (crd-neighbour crd :NE)))
	 (list ncrd (vertex-alias crd dir ncrd))))
      ((:SE :SSE)
       (let ((ncrd (crd-neighbour crd :SE)))
	 (list ncrd (vertex-alias crd dir ncrd))))
      (otherwise vert))))

(defun count-connected (crd dir predicate world &key (max 100))
  "Counts the vertices connected to (CRD DIR) that fullfill PREDICATE.
Will return count or MAX."
  (let ((frontier nil)
	(processed (make-hash-table :test 'equalp))
	(init-vert (vertex-key (list crd dir))))
    (setf (gethash init-vert processed) t)
    (do* ((vert init-vert (pop frontier))
	  (count 0))
	 ((or (null vert)
	      (>= count max))
	  count)
      (when (funcall predicate vert)
	(incf count)
	(dolist (neigh (mapcar #'vertex-key (point-neighbours vert world)))
	  (let ((found (gethash neigh processed)))
	    (when (not found)
	      (setf (gethash neigh processed) t)
	      (push neigh frontier))))))))

;; TODO: Move somewhere smarter
(defun random-pick (option-seq)
  "Pick randomly from elements in sequence OPTION-SEQ."
  (elt option-seq (random (length option-seq))))

#|
TODO: Maybe revisit later.
Pooling and river segments should be described by some custom data so
they can be used to populate and check duplicates in the same data structures.
Also choosing the longest path may not be the best solution, though neither is
the shortest path any good...
Graphical rivers either need to take (at least) CEN elevation into account
when moving to farther verts OR pathfinding will need to be used to avoid wonkiness.

(load "ht2.lisp") ; for prigraph 
(load "test.lisp") ; for ht2-search



(defun run-river-from (crd dir world &key (max-range 100) (pooling-size 40))
  "Generate river from source point (CRD DIR), running downhill."
  (declare (optimize debug))
  ;; As it is right now ht2-search searches recursively every child
  (let* ((prigraph
	   (ht2-search (list crd dir)
		       ;;;; Rivers should be composed of a single vert denoting exit
		       #'(lambda (point)
			   ;;(alex:shuffle
			   ;;(point-neighbours point world))
			   (when (eq (cadr point) :cen)
			     (format t "Point ~a cannot be CEN!~%" point))

			   ;;; TODO: (#S(CRD :X 60 :Y 28) . CEN)
			   ;;; Graphical river passes through hill at CEN
			   
			   ;;(alex::shuffle will break
			   (loop for np in (point-neighbours point world)
				 if (eq (cadr np) :CEN)
				   when (<=
					 (point-elevation
					  (vertex-exists (car np) (cadr np) world))
					 (point-elevation
					  (vertex-exists (car point) (cadr point) world)))
				     append (remove :CEN (point-neighbours np world)
						    :test #'eq :key #'cadr)
				 end
				 else collect np)
			   ;;)
			   )
		       #'(lambda (from to)
			   (let ((from-point (vertex-exists (car from) (cadr from) world))
				 (to-point (vertex-exists (car to) (cadr to) world)))
			     (<= (point-elevation to-point)
				 (point-elevation from-point))
			     ))
		       #'(lambda (from to)
			   (declare (ignore from to))
			   1)
		       #'(lambda (from to)
			   (when (< (point-elevation (vertex-exists (car to) (cadr to) world))
				    (point-elevation (vertex-exists (car from) (cadr from) world)))
			     ;;(format t "That's elevations ~a < ~a !~%"to from)
			     (let ((elevation
				     (point-elevation
				      (vertex-exists (car to) (cadr to) world))))
			       (cond ((>= (count-connected
					   (car to) (cadr to)
					   #'(lambda (vert)
					       (<= (point-elevation
						    (vertex-exists (car vert) (cadr vert) world))
						   elevation))
					   world
					   :max pooling-size)
					  pooling-size)
				      ;; This should be a "sea"
				      (format t "End pool detected at ~a~%" to))
				     (t
				      ;; This is a lake or something
				      (format t "A puddle found at ~a~%" to)
				      ))
			       
			       nil)))
		       ;;:key #'car
		       ;;:shortest-path nil ; not a good idea
		       :max-range max-range))
	 (end (high-end-leaf prigraph)))
    (defparameter *pg* prigraph)
    (defparameter *end* (node-key end))
    (riverize ;; TODO:: still broken
     (filter-river-path
      (mapcar #'node-key
	      (follow (node-key end) prigraph) ;; There's some kind of path
	      ))
     world)
    nil
    ))

;;;   (defparameter *test* (run-river-from (crd 61 34) :w *world*))
;;;   (mapcar #'node-key (follow (list (crd 58 26) :nnw) *test*))
;;;   etc..

;; This thing is a hack to get rid of rivers looping back into themselves:
(defun filter-river-path (vert-list)
  (do* ((path (loop for verts on vert-list ;; verts before verts where crd changes
		    when (not (equalp (caadr verts) (caar verts)))
		      collect (car verts))
	      (cdr path))
	(whole-path path))
       ((null path) whole-path)
    (loop with current = (vertex-key (car path))
	  for verts on (cdr path)
	  when (equalp (vertex-key (car verts))
		       current)
	    do (rplacd path (cdr verts)))))

(defun riverize (vert-list world)
  "VERT-LIST should be list of (crd dir) verts."
  (clrhash *crd-paths*) ;; remove when done

  (loop for ((a-crd a-dir) (b-crd b-dir)) on vert-list
	when b-crd
	  do (add-river-exit a-crd a-dir b-crd 0.1 world)
	finally (remhash b-crd *crd-paths*)))

|#
