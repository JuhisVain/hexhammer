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
