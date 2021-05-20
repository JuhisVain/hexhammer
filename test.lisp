(in-package :hexhammer)

(defun path-from (crd ht)
  (when crd (cons crd (path-from (cadr (gethash crd ht)) ht))))

(defun basin (crd ht)
  "Drainage basin of path at CRD."
  (do ((prebasin (list crd))
       (basin)
       (current))
      ((null prebasin) basin)
    (setf current (pop prebasin))
    (push current basin)
    (dolist (n (testneigh current 11 11))
      (when (equalp (cadr (gethash n ht))
		    current)
	(push n prebasin)))))

(defun testneigh (crd width height)
  (let ((ret))
    (when (> (x crd) 0)
      (push (crd (1- (x crd)) (y crd)) ret))
    (when (< (x crd) (1- width))
      (push (crd (1+ (x crd)) (y crd)) ret))
    (when (> (y crd) 0)
      (push (crd (x crd) (1- (y crd))) ret))
    (when (< (y crd) (1- height))
      (push (crd (x crd) (1+ (y crd))) ret))
    (reverse ret)))

(defun display-costs (ht)
  (dotimes (x 11)
    (dotimes (y 11)
      (format t "~2a " (or (car (gethash (crd x y) ht))
			   #\#)))
    (format t "~%")))

(defun runtest ()
  (let* ((world #2A((1 1 1 1 1 1 1 0 0 0 0)
		    (1 1 1 1 1 1 1 0 0 0 0)
		    (1 1 1 1 1 1 1 0 0 0 0)
		    (1 1 1 1 1 1 1 0 0 0 0)
		    (1 1 1 1 9 9 9 1 1 1 0)
		    (1 1 1 1 9 1 1 1 1 1 0)
		    (1 0 1 9 9 9 9 1 0 0 0)
		    (1 1 1 9 1 1 1 1 0 1 1)
		    (1 1 1 9 9 1 1 1 1 1 1)
		    (1 1 1 1 1 1 1 1 1 1 1)
		    (1 1 1 1 1 1 1 1 1 1 1)))
	 (width (array-dimension world 0))
	 (height (array-dimension world 1)))

    (depth-search (crd 5 5)
		  #'(lambda (crd) (testneigh crd width height))
		  #'(lambda (from to)
		      (<= (aref world (x to) (y to))
			  (aref world (x from) (y from))))
		  #'(lambda (from to)
		      ;;(declare (ignore from to))
		      (if (< (aref world (x to) (y to))
			     (aref world (x from) (y from)))
			  1
			  0))
		  #'(lambda (to)
		      (declare (ignore to))
		      nil)
		  )))

(defun degree (node)
  "Number of children for NODE."
  (+ (length (car node))
     (apply #'+ (mapcar #'degree (car node)))))

(defun runtest-tree ()
  (let* ((world #2A((1 1 1 1 1 1 1 0 0 0 0)
		    (1 1 1 1 1 1 1 0 0 0 0)
		    (1 1 1 1 1 1 1 0 0 0 0)
		    (1 1 1 1 1 1 1 0 0 0 -1)
		    (1 1 1 1 9 9 9 1 1 1 0)
		    (1 1 1 1 9 1 1 1 1 1 0)
		    (1 0 1 9 9 9 9 1 0 0 0)
		    (1 1 1 9 1 1 1 1 0 1 -5)
		    (1 1 1 9 9 1 1 1 1 1 -4)
		    (1 1 1 1 1 1 1 1 1 1 0)
		    (1 1 1 1 1 1 1 1 1 0 1)))
	 (width (array-dimension world 0))
	 (height (array-dimension world 1)))

    (block escape
      (tree-search (crd 5 5)
		   #'(lambda (crd) (testneigh crd width height))
		   #'(lambda (from to)
		       (<= (aref world (x to) (y to))
			   (aref world (x from) (y from))))
		   #'(lambda (from to)
		       (cond ((< (aref world (x to) (y to))
				 (aref world (x from) (y from)))
			      2)
			     (t
			      1)))
		   #'(lambda (to)
		       (declare (ignore to))
		       nil)

		   ;; prototype backtrack func
		   #'(lambda (from to from-node to-node)
		       ;; TODO: investigate if possible to avoid internal nodes leaking
		       (when (< (aref world (x to) (y to))
				(aref world (x from) (y from)))
			 ;;; TODO:::WRONG! degree will ignore things that should
			 ;; be counted beyond holes 
			 (when (>= (1+ (degree to-node))
				   9) ;; desired minimum pool size
			   (return-from escape ;to-node)
			     ;; Let's return pool as list instead:
			     (test-floodfill to (aref world (x to) (y to)) world)))))
		   
		   :shortest-path nil
		   ))))

(defun test-floodfill (crd depth world)
  (let ((hashtable (make-hash-table :test 'equalp)))
    (labels ((tfrec (crd)
	       (when (and
		      (not (gethash crd hashtable))
		      (<= (aref world (x crd) (y crd))
			  depth))
		 (setf (gethash crd hashtable) t)
		 (cons crd (mapcan #'(lambda (neigh)
				       (tfrec neigh))
				   (testneigh crd 11 11))))))
      (tfrec crd))))

(defun display-tree-costs (tree)
  (let ((world (make-array '(11 11) :initial-element #\#)))
    (sera:leaf-walk #'(lambda (payload)
			(when payload
			  (setf (aref world (x (seekee-data payload))
				      (y (seekee-data payload)))
				(seekee-priority payload))))
		    tree)
    (dotimes (x 11)
      (dotimes (y 11)
	(format t "~2a " (aref world x y)))
      (format t "~%"))))

;;; The problem with this implementation is that it has no understading of the path-tree's
;; actual structure making it unable to return an acceptable dead-end early.
;; Finding a 'pool' of suitable size will require an actual tree structure.
(defun depth-search (start get-neighbours-func moveable-func move-cost-func end-when-func
		     &key (shortest-path t) max-range (data-key #'identity))
  (let ((frontier (sera:make-heap :element-type 'seekee
				  :key #'seekee-priority
				  :test #'>=))
	(came-from (make-hash-table :test 'equalp)))
    (sera:heap-insert frontier (make-seekee :data start))
    (setf (gethash (funcall data-key start) came-from)
	  (list 0 nil)) ;; (distance-to-get-here came-from-here)
    
    (do ((current))
	((null (sera:heap-maximum frontier)))
      (setf current (sera:heap-extract-maximum frontier))
      (dolist (neighbour (funcall get-neighbours-func (seekee-data current)))
	(cond ((null neighbour)
	       nil)
	      ((not (funcall moveable-func (seekee-data current) neighbour))
	       nil)
	      ((null (gethash (funcall data-key neighbour) came-from))
	       (let ((distance (+ (seekee-priority current)
				  (funcall
				   move-cost-func
				   (seekee-data current) neighbour))))
		 (when (or (not max-range)
			   (<= distance max-range))
		   (sera:heap-insert frontier (make-seekee :data neighbour
							   :priority distance))
		   (setf (gethash (funcall data-key neighbour) came-from)
			 (list distance (seekee-data current))))))
	      ((and shortest-path ;; if search used for filling, no need for shortest path
		    (gethash (funcall data-key neighbour) came-from))
	       (let ((distance (+ (seekee-priority current)
				  (funcall
				   move-cost-func
				   (seekee-data current) neighbour))))
		 (when (< distance
			  (car (gethash (funcall data-key neighbour) came-from)))
		   (sera:heap-insert frontier (make-seekee :data neighbour
							   :priority distance))
		   (setf (gethash (funcall data-key neighbour) came-from)
			 (list distance (seekee-data current)))))))
	(when (funcall end-when-func neighbour)
	  (return-from DEPTH-SEARCH came-from))))
    came-from))

(defun ht2-search (start;of type keytype
		   get-neighbours-func;(keytype)
		   moveable-func;(from to)
		   move-cost-func;(from to)
		   &key
		     (shortest-path t)
		     max-range ; Should be set when shortest-path nil
		   )
  (let ((pg (make-prigraph start)))
    (labels ((seek (current-node)
	       '(format t "(~a;~a)~%"
		       (x (node-key current-node))
		       (y (node-key current-node)))
	       (dolist (neigh (remove (when (node-parent current-node)
					(node-key (node-parent current-node)))
				      (funcall get-neighbours-func (node-key current-node))
				      :test #'equalp))
		 (when (funcall moveable-func (node-key current-node) neigh)
		   (let* ((move-cost (funcall move-cost-func (node-key current-node) neigh))
			  (total-cost (+ (node-priority current-node) move-cost))
			  (old-top-node (first (get-nodes neigh pg))))

		     (cond ((and max-range (> total-cost max-range))
			    NIL)
			   ((and shortest-path old-top-node
				 (>= total-cost (node-priority old-top-node)))
			    NIL)
			   (t
			    (seek
			     (add-child neigh
					total-cost
					current-node
					pg)))))))))
      (seek (prigraph-root-node pg))
      pg)))

(defun display-ht2 (pg)
  (dotimes (i 11)
    (dotimes (j 11)
      (format t "~2a " (or (when (get-nodes (crd i j) pg)
			     (node-priority (car (get-nodes (crd i j) pg))))
			   #\#)))
    (format t "~%")))

;;; Shows downstream children of START annotated with a letter designating lineage
(defun display-signed-ht2 (start pg)
  (let ((world (make-array '(11 11) :initial-element "###"))
	(sigils (list 'a 'b 'c 'd)))

    (labels ((follow (node sigil)
	       (when (< (node-priority node)
			(or (parse-integer (aref world
						 (x (node-key node))
						 (y (node-key node)))
					   :start 1 :junk-allowed t)
			    666))
		 (setf (aref world
			     (x (node-key node))
			     (y (node-key node)))
		       (format nil "~a~2a" sigil (node-priority node))))
	       (dolist (child (node-children node))
		 (follow child sigil))))

      (setf (aref world
		  (x start)
		  (y start))
	    (format nil "~a~2a" "X" (node-priority (car (get-nodes start pg)))))

      (loop for child in (node-children (car (get-nodes start pg)))
	    for sigil in sigils
	    do (follow child sigil))
      
      (dotimes (i 11)
	(dotimes (j 11)
	  (format t "~3a " (aref world i j)))
	(format t "~%")))))

(defun runtest-ht2 (&optional (short t) (maxr nil))
  (let* ((world #2A((1 1 1 1 1 1 1 0 0 0 0)
		    (1 1 1 1 1 1 1 0 0 0 0)
		    (1 1 1 1 1 1 1 0 0 0 0)
		    (1 1 1 1 1 1 1 0 0 0 -1)
		    (1 1 1 1 9 9 9 1 1 1 0)
		    (1 1 1 1 9 1 1 1 1 1 0)
		    (1 0 1 9 9 9 9 1 0 0 0)
		    (1 1 1 9 1 1 1 1 0 1 -5)
		    (1 1 1 9 9 1 1 1 1 1 -4)
		    (1 1 1 1 1 1 1 1 1 1 0)
		    (1 1 1 1 1 1 1 1 1 0 1)))
	 (width (array-dimension world 0))
	 (height (array-dimension world 1)))

    (ht2-search (crd 5 5)
		#'(lambda (crd) (testneigh crd width height))
		#'(lambda (from to)
		    (<= (aref world (x to) (y to))
			(aref world (x from) (y from))))
		#'(lambda (from to)
		    (cond ((< (aref world (x to) (y to))
			      (aref world (x from) (y from)))
			   1)
			  (t
			   2)))
		:shortest-path short
		:max-range maxr
		)))
      

(defun tree-search (start get-neighbours-func moveable-func move-cost-func end-when-func
		    backtrack-func
		    &key (shortest-path t) max-range (key #'identity))
  (let ((discovered (make-hash-table :test 'equalp))
	(path-tree (list () (make-seekee :data start))))
    (setf (gethash (funcall key start) discovered) 0)

    (labels ((seek (current-node)
	       (setf (car current-node)
		     (sera:filter-map
		      #'(lambda (neigh)
			  ;; This does not prune already found nodes
			  (let ((move-cost
				  (+ (seekee-priority (cadr current-node))
				     (funcall move-cost-func
					      (seekee-data (cadr current-node)) neigh))))
			    (when (and
				   ;; should be before move-cost:
				   (funcall moveable-func
					    (seekee-data (cadr current-node)) neigh)
				   
				   (let ((found (gethash (funcall key neigh) discovered)))
				     (or
				      (not found)
				      (and shortest-path
					   (< move-cost found)))))
			      
			      (setf (gethash (funcall key neigh) discovered)
				    move-cost)
			      (list () (make-seekee :data neigh
						    :priority move-cost)))))
		      (funcall get-neighbours-func (seekee-data (cadr current-node)))))
	       
	       (dolist (neigh (car current-node))
		 (seek neigh)
		 (funcall backtrack-func
			  (seekee-data (cadr current-node))
			  (seekee-data (cadr neigh))
			  current-node neigh)
		 )))
      (seek path-tree)
      path-tree)))

(defun hashtree-search (start get-neighbours-func moveable-func move-cost-func end-when-func
			backtrack-func
			&key (shortest-path t) max-range (key #'identity))
  (let ((path-tree (make-keyed-hashtree key #'< start 0)))
    (labels ((seek (current)
	       (let ((current-node (access current path-tree))
		     (current-neighbours (funcall get-neighbours-func current)))
		 (dolist (neighbour current-neighbours)
		   (when (funcall moveable-func current neighbour)
		     (let ((move-cost (+ (priority current-node)
					 (funcall move-cost-func current neighbour))))
		       (when (or
			      (null (access neighbour path-tree))
			      (> (priority (access neighbour path-tree))
				 move-cost))
			 (add-child-node current neighbour
					 move-cost
					 path-tree))
		       )))
		 (dolist (child (children current-node))
		   (seek child)
		   (funcall backtrack-func current child :tree path-tree)))))
      (seek start)
      path-tree)))

(defun display-hashtree (ht)
  (dotimes (x 11)
    (dotimes (y 11)
      (format t "~2a " (if (access (crd x y) ht)
			   (priority (access (crd x y) ht))
			   #\#)))
    (format t "~%")))

(defun runtest-hashtree ()
  (let* ((world #2A((1 1 1 1 1 1 1 0 0 0 0)
		    (1 1 1 1 1 1 1 0 0 0 0)
		    (1 1 1 1 1 1 1 0 0 0 0)
		    (1 1 1 1 1 1 1 0 0 0 -1)
		    (1 1 1 1 9 9 9 1 1 1 0)
		    (1 1 1 1 9 1 1 1 1 1 0)
		    (1 0 1 9 9 9 9 1 0 0 0)
		    (1 1 1 9 1 1 1 1 0 1 -5)
		    (1 1 1 9 9 1 1 1 1 1 -4)
		    (1 1 1 1 1 1 1 1 1 1 0)
		    (1 1 1 1 1 1 1 1 1 0 1)))
	 (width (array-dimension world 0))
	 (height (array-dimension world 1)))

    (block escape
      (hashtree-search (crd 5 5)
		   #'(lambda (crd) (testneigh crd width height))
		   #'(lambda (from to)
		       (<= (aref world (x to) (y to))
			   (aref world (x from) (y from))))
		   #'(lambda (from to)
		       (cond ((< (aref world (x to) (y to))
				 (aref world (x from) (y from)))
			      1)
			     (t
			      0)))
		   #'(lambda (to)
		       (declare (ignore to))
		       nil)

		   ;;; Maybe this is a bit too ambitious...
		   ;; prototype backtrack func
		   #'(lambda (from to &key tree)
		       ;; TODO: investigate if possible to avoid internal nodes leaking
		       (when (< (aref world (x to) (y to))
				(aref world (x from) (y from)))
			 (format t "Backtraking at f:~a -> ~a~%" from to)
			 ;;; TODO:::WRONG! degree will ignore things that should
			 ;; be counted beyond holes
			 ;; nah
			 '(when (>= (1+ (hashtree-degree to tree))
				   9) ;; desired minimum pool size
			   (return-from escape ;to-node)
			     ;; Let's return pool as list instead:
			     (test-floodfill to (aref world (x to) (y to)) world)))))
		   
		   :shortest-path nil
		   ))))

(defun make-perf-test-world (dims)
  (destructuring-bind (x y) dims
    (let ((world (make-array dims)))
      (dotimes (xi x)
	(dotimes (yi y)
	  (setf (aref world xi yi) (random 100))))
      world)))

(defun perf-test (dim)
  (let* ((width dim)
	 (height dim)
	 (world (make-perf-test-world (list width height))))
    (flet ((moveable (from to)
	     (<= (abs (- (aref world (x to) (y to))
			 (aref world (x from) (y from))))
		 50))
	   (movecost (from to)
	     (cond ((< (aref world (x to) (y to))
		       (aref world (x from) (y from)))
		    1)
		   (t
		    2))))
      (format t "TREE~%")
      (time
       (tree-search (crd (floor dim 2) (floor dim 2))
		    #'(lambda (crd) (testneigh crd width height))
		    #'moveable
		    #'movecost
		    #'(lambda (to)
			(declare (ignore to))
			nil)
		    #'(lambda (from to from-node to-node) nil)
		    :shortest-path nil))
      ;;;; UNACCEPTABLE
      (format t "~%HASHTREE~%")
      (time
       (hashtree-search (crd (floor dim 2) (floor dim 2))
			#'(lambda (crd) (testneigh crd width height))
			#'moveable
			#'movecost
			#'(lambda (to)
			    (declare (ignore to))
			    nil)
			#'(lambda (from to &key tree) nil)
			:shortest-path nil))
      NIL)))
