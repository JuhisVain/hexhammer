(in-package :hexhammer)

;;; NOTE: Bezier control points inside hex at 50%
;; of distance between verts or R, whichever longer. (should say shorter??)

;;; Rivers should live in the same hashtable as other paths
;; However the construction of infrastructure should not affect
;; the graphical position of rivers.

;; key: crd
(defvar *crd-paths* (make-hash-table :test 'equalp))

(defclass crd-paths ()
  ((rivers ;; ((exit-dir . exit-to-crd) (entry-dir . entry-from-crd)... )
    :initarg :rivers
    :accessor crd-paths-rivers)
   (infra
    :initarg :infra
    :accessor crd-paths-infra)))

(defun make-crd-paths (&key rivers infra)
  (make-instance 'crd-paths :rivers rivers :infra infra))

(defun define-crd-paths (crd &key rivers infra)
  (setf (gethash crd *crd-paths*)
	(make-crd-paths :rivers rivers :infra infra)))

(defun add-river-entry (crd dir from world)
  "Add a river entry to coordinate CRD coming from
coordinate FROM through DIR in CRD."
  (when (not (gethash crd (world-map world)))
    (return-from add-river-entry))
  (let ((crd-paths (gethash crd *crd-paths*)))
    (if (null crd-paths)
	(define-crd-paths crd :rivers (list nil (cons dir from)))
	(if (not (member dir (crd-paths-rivers crd-paths)
			 :key #'car))
	    (setf (crd-paths-rivers crd-paths)
		  (nconc (crd-paths-rivers crd-paths) (list (cons dir from))))
	    ;;(push (cons dir from) (cdr (crd-paths-rivers crd-paths)))
	    (error
	     "Trying to push duplicate river entry vertex ~a to rivers ~a~%"
	     (cons dir from) (crd-paths-rivers crd-paths))))))

(defun add-river-exit (crd dir to world)
  "Add a river exit to coordinate CRD exiting at CRD's DIR to coordinate TO."
  (when (not (gethash crd (world-map world)))
    (return-from add-river-exit))
  (let ((crd-paths (gethash crd *crd-paths*)))
    (if (null crd-paths)
	(define-crd-paths crd :rivers (cons (cons dir to) nil))
	(if (and (null (car (crd-paths-rivers crd-paths)))
		 (not (member dir (cdr (crd-paths-rivers crd-paths))
			      :key #'car)))
	    (rplaca (crd-paths-rivers crd-paths)
		    (cons dir to))
	    (error
	     "Trying to set invalid river exit vertex ~a to rivers ~a~%"
	     (cons dir to) (crd-paths-rivers crd-paths)))))
  (add-river-entry to (vertex-alias crd dir to) crd world))

(defun rivertest (&optional (world *world*))
  (clrhash *crd-paths*)
  (macrolet
      ((river-exits (&rest instructions)
	 `(progn
	    ,@(loop for (x0 y0 dir x1 y1)
		      on instructions
		    by #'(lambda (x) (nthcdr 5 x))
		    collect `(add-river-exit (crd ,x0 ,y0) ,dir
					     (crd ,x1 ,y1) world)))))

    (river-exits
     8 14 :s 8 13
     8 13 :s 8 12
     8 12 :e 9 11
     9 11 :s 9 10
     9 10 :s 9 9
     9 9 :ssw 9 8
     9 8 :ssw 8 8
     8 8 :se 9 7
     9 7 :sse 9 6
     9 6 :w 8 6
     8 6 :ssw 7 5
     7 5 :nw 6 6
     6 6 :ssw 6 5
     6 5 :sw 5 4
     5 4 :nw 4 5
     4 5 :w 3 4
     3 4 :e 4 4
     4 4 :sse 5 3
     5 3 :ssw 4 3
     4 3 :se 5 2
     5 2 :nne 6 3
     6 3 :e 7 2

     6 15 :sse 6 14
     6 14 :w 5 13
     5 13 :s 5 12
     5 12 :sse 5 11
     5 11 :e 6 11
     6 11 :se 7 10
     7 10 :s 7 9
     7 9 :s 7 8
     7 8 :s 7 7
     7 7 :s 7 6
     7 6 :s 7 5)

    ;; TODO: A river exiting to off map is OK, but if exiting to water body
    ;; or "nowhere" will need to do something
    (remhash (crd 7 2) *crd-paths*)
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
	
	(let* ((exit-dir (caar (crd-paths-rivers crd-paths)))
	       (exit-crd (vertex-crd r exit-dir ;hex-centre-x hex-centre-y
				     ))
	       (master-entry
		 (vertex-crd r (or (caadr (crd-paths-rivers crd-paths))
				   :CEN)
			     ;hex-centre-x hex-centre-y
			     )))

	  (let* ((yd (- (- (y master-entry) (y exit-crd))))
		 (xd (- (x master-entry) (x exit-crd)))
		 
		 (half-distance (/ (sqrt (+ (expt yd 2)
					    (expt xd 2)))
				   2))
		 
		 (midpoint-x (+ (x exit-crd) (/ xd 2)))
		 (midpoint-y (+ (y master-entry) (/ yd 2)))

		 (angle (atan midpoint-y midpoint-x))

		 (centre-crd (ntranslate (crd (* (cos angle)
						 ;; use either r or sin60*r :
						 (- r;(* +sin60+ r)
						    half-distance))
					      (* (sin angle)
						 (- r;(* +sin60+ r)
						    half-distance)))
					 hex-centre-x hex-centre-y)))

	    ;;TEST
	    ;; NOTE: slopes are always the wrong solution for absolutely everything
	    (let* ((prime-angle (atan (- (- (y master-entry) (y exit-crd)))
				      (- (x master-entry) (x exit-crd))))
		   (angle (+ (/ +sf-pi+ 2) prime-angle))
		   
		   (angle-mark
		     (nrotate (crd (* 0.5 r) 0)
			      nil (sin angle) (cos angle))))

	      (format t "~&r ~a CRD  ~a -> primeangle: ~a -> perp ANGLE: ~a~2%"
		      r crd
		      (* (/ (atan (- (- (y master-entry) (y exit-crd)))
				  (- (x master-entry) (x exit-crd)))
			    +sf-pi+)
			 180)
		      (* (/ angle +sf-pi+) 180))

	      (cairo:set-source-rgb 1.0 0.1 0.5)
	      (cairo:move-to (x centre-crd) (y centre-crd))
	      (cairo:rel-line-to (x angle-mark) (y angle-mark))
	      (cairo:stroke)
	      (cairo:set-source-rgb 0.0 0.1 0.8))
	    ;;TEST OVER
	    

	    (cairo:move-to (x centre-crd) (y centre-crd))
	    (cairo:line-to (+ (x exit-crd) hex-centre-x)
			   (+ (y exit-crd) hex-centre-y))
	    (cairo:stroke)

	    (dolist (entry (cdr (crd-paths-rivers crd-paths)))
	      (let* ((entry-dir (car entry))
		     (entry-crd (vertex-crd r entry-dir hex-centre-x hex-centre-y)))

		(cairo:move-to (x entry-crd) (y entry-crd))
		(cairo:line-to (x centre-crd) (y centre-crd)))
		(cairo:stroke)
		
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
