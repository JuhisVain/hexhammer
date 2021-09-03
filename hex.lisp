(in-package :hexhammer)

(deftype pointspec ()
  `(cons crd (cons (member ,@+vertex-directions+) null)))
(defstruct (point (:constructor point (elevation &optional (raw-water 0))))
  (terrain (make-terrain) :type terrain);;(list (cons 'cultivated 'dry))) ;; TODO ??((base-type . weather-state) . mod-types)??
  (elevation 0 :type elevation)
  (raw-water 0 :type elevation))

(declaim (inline point-water))
(defun point-water (instance)
  (point-raw-water instance))
(defun (setf point-water) (new-value instance)
  (setf (point-raw-water instance)
	(if (<= new-value (point-elevation instance))
	    0
	    new-value)))

(defstruct (hex (:constructor
		    make-hex (cen &optional nne ne e se sse s ssw sw w nw nnw n)))
  (cen (point 0 0) :type point)
  (nne (point 0 0) :type point)
  (ne (point 0 0) :type point)
  (e (point 0 0) :type point)
  (se (point 0 0) :type point)
  (sse (point 0 0) :type point)
  (s (point 0 0) :type point)
  (ssw (point 0 0) :type point)
  (sw (point 0 0) :type point)
  (w (point 0 0) :type point)
  (nw (point 0 0) :type point)
  (nnw (point 0 0) :type point)
  (n (point 0 0) :type point))

(defun chomp-append (&rest lists)
  (labels ((copy (count list)
	     (when (> count 0)
	       (cons (first list)
		     (copy (1- count) (cdr list))))))
    (mapcan #'(lambda (x) (copy 2 x))
	    lists)))

(defun make-hex-by-edge
    (elevation &key ne-edge se-edge s-edge sw-edge nw-edge n-edge)
  (apply #'make-hex
	 elevation
	 (chomp-append ne-edge se-edge s-edge
		       sw-edge nw-edge n-edge)))

(defstruct contours
  (left 0 :type (signed-byte 8))
  (range 0 :type (signed-byte 8))
  (water 0 :type (signed-byte 8))
  (deque nil :type range-deque))

(defun record-contours (hex left right step)
  (let* ((left-point (hex-vertex hex left))
	 (right-point (hex-vertex hex right))
	 (left-ele (point-elevation left-point))
	 (right-ele (point-elevation right-point))
	 (difference (- right-ele left-ele))
	 (contours
	   (make-contours
	    :left left-ele
	    :range difference
	    :water (max (point-water left-point)
			(point-water right-point))
	    ;;:water-right (point-water right-point)
	    :deque (make-range-deque :step step))))
    (set-all-contours contours)
    '(cond ((> difference 0)
	   (loop for elevation
		 from (* (ceiling left-ele step) step) ; round to higher step multiple
		   to right-ele by step
		 do (push-right elevation (contours-deque contours))))
	  ((< difference 0)
	   (loop for elevation
		 from (* (floor left-ele step) step) ; round to lower step multiple
		 downto right-ele by step
		 do (push-right elevation (contours-deque contours)))))
    contours))

(defun contours-right (contours)
  (+ (contours-left contours)
     (contours-range contours)))

(defun contours-max (contours)
  (max (contours-left contours)
       (contours-right contours)))

;;;; TODO: Screwed up contours at at least some step levels
(defun set-all-contours (contours)
  (reset-range-deque (contours-deque contours))
  (let ((step (range-deque-step (contours-deque contours))))
    (cond ((plusp (contours-range contours))
	   '(format t "Loop from ~a to ~a by ~a~%"
		   (* (ceiling (1+ (contours-left contours)) step) step)
		   (contours-right contours)
		   step)
	   (loop for elevation
		 from ;; round to higher step multiple
		 (* (ceiling (1+ (contours-left contours)) step) step)
		   to (contours-right contours) by step
		 do (push-right elevation (contours-deque contours))
		    '(format t "   ---UP PUSHING ~a~%" elevation)
		 ))
	  ((minusp (contours-range contours))
	   '(format t "Loop from ~a DOWN to ~a by ~a~%"
		   (* (floor (contours-left contours) step) step)
		   (1+ (contours-right contours))
		   step)
	   (loop for elevation
		 from (* (floor (contours-left contours) step) step) ; round to lower step multiple
		 downto (1+ (contours-right contours)) by step
		 do (push-right elevation (contours-deque contours))
		    '(format t "   ---DOWN PUSHING ~a~%" elevation)
		 )))))

#|
	((plusp (contours-range contours))
	 (loop for elevation
	       from (1+ (contours-left contours))
		 to (contours-right contours)
	       do (push-right elevation (contours-deque contours))))
	((minusp (contours-range contours))
	 (loop for elevation from (contours-left contours)
	       downto (1+ (contours-right contours))
	       do (push-right elevation (contours-deque contours))))))
|#
(defun set-land-contours (contours)
  "Sets CONTOURS' range-deque to hold only land contours."
  (reset-range-deque (contours-deque contours))
  (cond
    ((plusp (contours-range contours))
     (loop for elevation from (1+ (max (contours-left contours)
				       (1+ (contours-water contours))))
	     to (contours-right contours)
	   do (push-right elevation (contours-deque contours))))
    ((minusp (contours-range contours))
     (loop for elevation from (contours-left contours)
	   downto (1+ (max (contours-right contours)
			   (1+ (contours-water contours))))
	   do (push-right elevation (contours-deque contours))))))

(defun surface-level (contours)
  "Returns integer surface level when shoreline drawable.
If CONTOURS is totally submerged or totally dry returns NIL."
  (let ((surface (contours-water contours)))
    (when (or (>= (contours-left contours)
		  surface
		  (1+ (contours-right contours)))
	      (<= (1+ (contours-left contours))
		  surface
		  (contours-right contours)))
      (contours-water contours))))

(defun set-surface-contours (contours)
  "Sets CONTOURS' range-deque to hold only the water surface contour."
  (reset-range-deque (contours-deque contours))
  (if (or (>= (contours-left contours)
	      (1+ (contours-water contours))
	      (contours-right contours))
	  (<= (contours-left contours)
	      (1+ (contours-water contours))
	      (contours-right contours)))
      (push-right (1+ (contours-water contours))
		  (contours-deque contours))))

(defun set-water-contours (contours)
  "Sets CONTOURS' range-deque to hold only water contours."
  (reset-range-deque (contours-deque contours))
  (cond
    ((plusp (contours-range contours))
     (loop for elevation from (1+ (contours-left contours))
	     to (min (contours-right contours)
		     (1+ (contours-water contours)))
	   do (push-right elevation (contours-deque contours))))
    ((minusp (contours-range contours))
     (loop for elevation from (min (contours-left contours)
				   (1+ (contours-water contours)))
	   downto (1+ (contours-right contours))
	   do (push-right elevation (contours-deque contours))))))

(defun contour-index (elevation contours)
  (let ((index
	  (if (>= (contours-range contours) 0)
	      (- elevation (contours-left contours) 1)
	      (abs (- elevation (contours-left contours))))))
    (if (and (<= 0 index)
	     (< index (abs (contours-range contours))))
	index
	(error "~a is not a contour of ~a!~%" elevation contours))))

(defun is-contour-of (elevation contours)
  (or (= elevation
	 (peek-left (contours-deque contours)))
      (= elevation
	 (peek-right (contours-deque contours)))))

;; Each underwater vert should have an integer water surface altitude
(defun vertex-underwater (hex dir)
  (let ((point (hex-vertex hex dir)))
    (>= (point-water point) (point-elevation point))))

(defvar *point-connections*
  (make-hash-table :test 'eq :size 13))

(defmacro defpointcon (pointdir (&rest connections))
  `(setf (gethash ,pointdir *point-connections*) ',connections))

(defun point-connections (crd dir)
  (mapcan #'(lambda (pre-dir)
	      (etypecase pre-dir
		(keyword (list (list crd pre-dir)))
		(list (mapcar #'(lambda (then-dir)
				  (list
				   (crd-neighbour crd (first pre-dir))
				   then-dir))
			      (rest pre-dir)))))
	  (gethash dir *point-connections*)))

;;          dir/dirs at this crd/go here, then this dir
(defpointcon :N (:NNW :CEN :NNE (:N :CEN)))
(defpointcon :NNE (:N :NE :cen (:N :SE :CEN) (:NE :CEN)))
(defpointcon :NE (:NNE :CEN :E (:NE :CEN)))
(defpointcon :E (:NE :SE :cen (:NE :S :CEN) (:SE :CEN)))
(defpointcon :SE (:E :CEN :SSE (:SE :CEN)))
(defpointcon :SSE (:SE :S :cen (:SE :SW :CEN) (:S :CEN)))
(defpointcon :S (:SSE :CEN :SSW (:S :CEN)))
(defpointcon :SSW (:S :SW :cen (:S :NW :CEN) (:SW :CEN)))
(defpointcon :SW (:SSW :CEN :W (:SW :CEN)))
(defpointcon :W (:SW :NW :cen (:SW :N :CEN) (:NW :CEN)))
(defpointcon :NW (:W :CEN :NNW (:NW :CEN)))
(defpointcon :NNW (:NW :N :cen (:NW :NE :CEN) (:N :CEN)))
(defpointcon :CEN (:N :NE :SE :S :SW :NW :nnw :nne :e :sse :ssw :w))

(defun point-neighbours (point world) ;; vertex / point terminology is screwed
  "Returns list of points that neighbour the point POINT
and exist in world WORLD."
  (let* ((con-verts (point-connections (car point) (cadr point)))
	 (con-points (mapcar
		      #'(lambda (vert)
			  (vertex-exists (car vert) (cadr vert) world))
		      con-verts)))
    (let ((neighbours
	    (loop for vert in con-verts
		  for point in con-points
		  when point collect vert)))
      neighbours)))

(defun increase-water-level (crd dir world &optional (delta 1))
  (let ((point (vertex-exists crd dir world)))
    (format t "in water level ~a ~a ~a~%" crd dir delta)
    (if (plusp delta)
	(flood-fill crd dir
		    (+ delta
		       (max (point-water point)
			    (point-elevation point)))
		    world)
	;; else:
	(let ((water-level (+ delta
			      (max (point-water point)
				   (point-elevation point)))))
	  (format t "new water level will be : ~a~%" water-level)
	  (maphash #'(lambda (crd-vert cf)
		       (declare (ignore cf))
		       (setf (point-water (vertex-exists (car crd-vert) (cadr crd-vert) world))
			     water-level))
		   (breadth-first-search
		    (list crd dir)
		    0 world
		    #'point-neighbours
		    #'(lambda (from to world)
			(declare (ignore from))
			(let ((to-point (vertex-exists (car to) (cadr to) world)))
			  (if (and (>= (point-water to-point)
				       water-level)
				   (>= (point-water to-point)
				       (point-elevation to-point)))
			      0 666)))
		    #'(lambda (x) (declare (ignore x)) nil)))
	  ))))

;; for example (flood-fill (crd 47 14) :cen 10 *world*)
(defun flood-fill (crd dir water-level world)
  (maphash #'(lambda (crd-vert cf)
	       (declare (ignore cf))
	       (setf (point-water (vertex-exists (car crd-vert) (cadr crd-vert) world))
		     water-level))
	   (breadth-first-search
	    (list crd dir)
	    0 world
	    #'point-neighbours
	    #'(lambda (from to world)
		(declare (ignore from))
		(let ((to-point (vertex-exists (car to) (cadr to) world)))
		  (if (or (>= (point-water to-point)
			      water-level)
			  (>= (point-elevation to-point)
			      water-level))
		      666 0)))
	    #'(lambda (x) (declare (ignore x)) nil))))

(defun depress-vert (crd dir amount world)
  (let ((vertex (vertex-exists crd dir world)))
    (decf (point-elevation vertex) amount)
    ;; TODO: check connected verts for water, if yes fill this.
    ))

(defun elevate-vert (crd dir amount world)
  (let ((vertex (vertex-exists crd dir world)))
    (prog1
	(incf (point-elevation vertex) amount)
      ;; Guarantee 0 water level if point not submerged:
      (when (>= (point-elevation vertex)
		(point-water vertex))
	(setf (point-water vertex) 0)))))

(defun sink-vert (crd dir rel-water-level world)
  "Sink the vertex at CRD DIR by REL-WATER-LEVEL and fill depression."
  (let ((vertex (vertex-exists crd dir world)))
    (when vertex
      (setf (point-water vertex) (point-elevation vertex))
      (decf (point-elevation vertex) rel-water-level))))

(defun sinktest ()
  ;;(flood-fill 10 *world* (crd 49 13) :cen)

  (macrolet ((vert-sinker (&rest instructions)
	       `(progn
		  ,@(loop for (x y dir)
			    on instructions
			  by #'(lambda (x) (nthcdr 3 x))
			  collect `(sink-vert (crd ,x ,y)
					      ,dir
					      1
					      *world*)))))
    ;; Buggy behaviour when sinking right next to lower elevations:
    (vert-sinker
     68 44 :nw
     68 44 :ssw
     68 44 :s
     68 44 :sw
     68 44 :w
     68 44 :sse
     69 43 :sw
     69 43 :cen
     69 43 :ne
     69 43 :n
     ;; TODO: Change of plan: Let's make a graphical sinker!
     )))

(defmacro probe-contours ((at-left at-right) var
			  &body body)
  `(when (eql (the (or fixnum null) (peek-left (contours-deque ,at-right)))
	      (the (or fixnum null) (peek-right (contours-deque ,at-left))))
     (do ((,var (the (or fixnum null) (peek-left (contours-deque ,at-right)))
		(the (or fixnum null) (peek-left (contours-deque ,at-right)))))
	 ((or
	   (null (peek-left (contours-deque ,at-right)))
	   (null (peek-right (contours-deque ,at-left)))))
       (progn (pop-left (contours-deque ,at-right))
	      (pop-right (contours-deque ,at-left)))
       
       ;; Example thickness change:
       (if (= (mod ,var 10) 0)
	   (cairo:set-line-width 1.0)
	   (cairo:set-line-width 0.5))
       (cond ((= ,var (max (contours-water ,at-right)
			       (contours-water ,at-left)))
	      (cairo:set-source-rgb 1.0 0.2 0.2))
	     ((< ,var (max (contours-water ,at-right)
			       (contours-water ,at-left)))
	      (cairo:set-source-rgb 0.0 0.0 0.0)) ; testing
	     (t (cairo:set-source-rgb 0.5 0.5 0.5)))
       ,@body
       (cairo:stroke)
       )))

(defun data-at (crd world)
  (list 
   (gethash crd (world-map world))
   (gethash crd (world-features world))))

(defun hex-at (crd world)
  (gethash crd (world-map world)))

(defun (setf hex-at) (new-hex crd world)
  (setf (gethash crd (world-map world)) new-hex))

(defun hex-vertex (hex vert-direction)
  (declare ;(type hex hex)
   (type hex-vertex vert-direction))
  (when (not hex)
    (return-from hex-vertex))
  (ecase vert-direction
    (:CEN (hex-CEN hex))
    (:NNW (hex-NNW hex))
    (:N (hex-N hex))
    (:NNE (hex-NNE hex))
    (:NE (hex-NE hex))
    (:E (hex-E hex))
    (:SE (hex-SE hex))
    (:SSE (hex-SSE hex))
    (:S (hex-S hex))
    (:SSW (hex-SSW hex))
    (:SW (hex-SW hex))
    (:W (hex-W hex))
    (:NW (hex-NW hex))))

(define-compiler-macro hex-vertex (&whole form hex vert-direction)
  (if (typep vert-direction 'hex-vertex)
      (ecase vert-direction
	(:CEN `(hex-CEN ,hex))
	(:NNW `(hex-NNW ,hex))
	(:N `(hex-N ,hex))
	(:NNE `(hex-NNE ,hex))
	(:NE `(hex-NE ,hex))
	(:E `(hex-E ,hex))
	(:SE `(hex-SE ,hex))
	(:SSE `(hex-SSE ,hex))
	(:S `(hex-S ,hex))
	(:SSW `(hex-SSW ,hex))
	(:SW `(hex-SW ,hex))
	(:W `(hex-W ,hex))
	(:NW `(hex-NW ,hex)))
      form))

(defun vertex-exists (crd dir world)
  (declare (crd crd)
	   (hex-vertex dir))
  (or (hex-vertex (hex-at crd *world*) dir)
      (case dir
	((:N :NE :SE :S :SW :NW)
	 (hex-vertex (hex-at (crd-neighbour crd dir)
			     world)
		     (opposite dir)))
	(:NNE (or (hex-vertex (hex-at (crd-neighbour crd :N)
				      world)
			      :SSE)
		  (hex-vertex (hex-at (crd-neighbour crd :NE)
				      world)
			      :W)))
	(:E (or (hex-vertex (hex-at (crd-neighbour crd :NE)
				    world)
			    :SSW)
		(hex-vertex (hex-at (crd-neighbour crd :SE)
				    world)
			    :NNW)))
	(:SSE (or (hex-vertex (hex-at (crd-neighbour crd :SE)
				      world)
			      :W)
		  (hex-vertex (hex-at (crd-neighbour crd :S)
				      world)
			      :NNE)))
	(:SSW (or (hex-vertex (hex-at (crd-neighbour crd :S)
				      world)
			      :NNW)
		  (hex-vertex (hex-at (crd-neighbour crd :SW)
				      world)
			      :E)))
	(:W (or (hex-vertex (hex-at (crd-neighbour crd :SW)
				    world)
			    :NNE)
		(hex-vertex (hex-at (crd-neighbour crd :NW)
				    world)
			    :SSE)))
	(:NNW (or (hex-vertex (hex-at (crd-neighbour crd :N)
				      world)
			      :SSW)
		  (hex-vertex (hex-at (crd-neighbour crd :NW)
				      world)
			      :E))))))

(defun adjacent-vertex-directions (dir)
  "From absolute westmost to absolute eastmost"
  (declare (hex-vertex dir))
  (case dir
    (:N '(:NNW :N :NNE))
    (:NE '(:NNE :NE :E))
    (:SE '(:SSE :SE :E))
    (:S '(:SSW :S :SSE))
    (:SW '(:W :SW :SSW))
    (:NW '(:W :NW :NNW))))

(defun edge-verts (crd dir world)
  "Returns list of points or nils from west to east of edge DIR of hex at CRD"
  (declare (crd crd)
	   (direction dir))
  (mapcar #'(lambda (vert-dir)
	      (vertex-exists crd vert-dir world))
	  (adjacent-vertex-directions dir)))
