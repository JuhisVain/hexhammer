(in-package :hexhammer)

;; Better do still bodies of water first -- DONE

;;; NOTE: Bezier control points inside hex at 50%
;; of distance between verts or R, whichever longer. (should say shorter??)

;;; Rivers should live in the same hashtable as other paths
;; However the construction of infrastructure should not affect
;; the graphical position of rivers.

;; All pathways should be ordered AT EACH HEX-VERTEX POINT
;; Pathway drawing must be done after other terrain features
;;

;; key: crd
(defvar *crd-paths* (make-hash-table :test 'equalp))

(defclass crd-paths ()
  ((trunks ;; (master left-slaves . right-slaves)
    :initarg :trunks
    :accessor crd-paths-trunks)
   (tributaries
    :initarg :tributaries
    :accessor crd-paths-tributaries)))

(defclass crd-river () ;; Tributaries have (at least?) one point in a foreign trunk
  ((entry ;; waypoint. could be joinpoint in a delta?
    :initarg :entry
    :accessor crd-river-entry)
   (exit ;; either a waypoint or a joinpoint
    :initarg :exit
    :accessor crd-river-exit)))

(defclass joinpoint ()
  ((trunk ; a crd-river. Could be a tributary too?
    :initarg :trunk
    :accessor joinpoint-trunk)
   (master
    :initarg :master
    :accessor joinpoint-master)
   (master-left
    :initarg :master-left
    :accessor joinpoint-master-left)
   (master-right
    :initarg :master-right
    :accessor joinpoint-master-right)))

(defclass waypoint ()
  ((vertex ; a hex-vertex
    :initarg :vertex
    :accessor waypoint-vertex)
   (master
    :initarg :master
    :accessor waypoint-master)
   ;; right and left are binary trees
   ;; where lefts are rivers/other natural features and rights are infra
   (master-left
    :initarg :master-left
    :accessor waypoint-master-left)
   (master-right
    :initarg :master-right
    :accessor waypoint-master-right)))

(defmethod get-point ((point joinpoint))
  (joinpoint-trunk point))
(defmethod get-point ((point waypoint))
  (waypoint-vertex point))

(defun trunkp (river)
  (and (typep (crd-river-entry river) 'waypoint)
       (typep (crd-river-exit river) 'waypoint)))

(defun tributaryp (river)
  (or (typep (crd-river-entry river) 'joinpoint)
      (typep (crd-river-exit river) 'joinpoint)))

(defun make-crd-paths (&key (trunks (list nil)) tributaries)
  (make-instance 'crd-paths
		 :trunks trunks
		 :tributaries tributaries))

(defun ensure-crd-paths (crd)
  (or (gethash crd *crd-paths*)
      (setf (gethash crd *crd-paths*)
	    (make-crd-paths))))

(defun ensure-waypoint (dir/river crd-paths)
  (or (find-point dir/river crd-paths)
      (waypoint dir/river)))

(defun make-crd-river (&key entry-point exit-point crd-paths)
  "ENTRY-POINT and EXIT-POINT should be of type waypoint."
  (let ((river (make-instance 'crd-river)))
    (when entry-point
      (setf (crd-river-entry river) entry-point))
    (when exit-point
      (setf (crd-river-exit river) exit-point))

    (when crd-paths
      (crd-paths-push-trunk crd-paths river))
    river))

(defun crd-paths-push-trunk (crd-paths river)
  (format t "trunks: ~a~%      river: ~a~2%" (crd-paths-trunks crd-paths) river)
  (if (null (car (crd-paths-trunks crd-paths)))
      (setf (car (crd-paths-trunks crd-paths)) river)
      (setf (cdr (crd-paths-trunks crd-paths))
	    (case (crd-river-right-left
		   river (car (crd-paths-trunks crd-paths)))
	      (:left (cons (sort (cons river (cadr (crd-paths-trunks crd-paths)))
				 #'(lambda (this from)
				     (ecase (crd-river-right-left this from)
				       ((:left :same) t)
				       (:right nil))))
			   (cddr (crd-paths-trunks crd-paths))))
	      ((:right :same)
	       (cons (cadr (crd-paths-trunks crd-paths))
		     (sort (cons river (cddr (crd-paths-trunks crd-paths)))
			   #'(lambda (this from)
			       (ecase (crd-river-right-left this from)
				 ((:right :same) t)
				 (:left nil))))
		     ))))))

(defun make-waypoint (&key vertex master master-left master-right)
  (make-instance 'waypoint
		 :vertex vertex :master master
		 :master-left master-left :master-right master-right))
(defun make-joinpoint (&key trunk master master-left master-right)
  (make-instance 'joinpoint
		 :trunk trunk :master master
		 :master-left master-left :master-right master-right))

(defgeneric waypoint (point))
(defmethod waypoint ((point symbol))
  (make-waypoint :vertex point))

(defmethod waypoint ((point crd-river))
  (make-joinpoint :trunk point))

(defun list-river-points (crd-paths)
  (remove-duplicates
   (mapcan #'(lambda (crd-riv)
	      (list (crd-river-entry crd-riv)
		    (crd-river-exit crd-riv)))
	   (append (let ((trunks (crd-paths-trunks crd-paths)))
		     (when (car trunks)
		       (list* (car trunks)
			      (append (cadr trunks) (cddr trunks)))))
		   (crd-paths-tributaries crd-paths)))
   :test #'eq))

(defun find-point (point crd-paths)
  (let ((defined-points (list-river-points crd-paths)))
    (find point defined-points :key #'get-point :test #'eq)))

(defun hex-vertexp (dir &rest more-dirs)
  (and (typep dir 'hex-vertex)
       (or (null more-dirs)
	   (apply #'hex-vertexp (first more-dirs) (rest more-dirs)))))

(defun crd-river-right-left (this-river from-river)
  (right-or-left (waypoint-vertex (crd-river-entry this-river))
		 (waypoint-vertex (crd-river-exit this-river))
		 (waypoint-vertex (crd-river-entry from-river))
		 (waypoint-vertex (crd-river-exit from-river))))

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


(defun push-sub-river (river waypoint)
  (labels ((place-in-tree (dir tree)
	     (cond ((null tree)
		    (list river nil nil))
		   ((not (eq dir
			     (crd-river-right-left river (car tree))))
		    (list river tree))
		   (t
		    (list (car tree)
			  (place-in-tree dir (cadr tree)))))))
    
    (let ((direction
	    (crd-river-right-left river (waypoint-master waypoint))))
      (case direction
	(:left
	 (setf (waypoint-master-left waypoint)
	       (place-in-tree :left (waypoint-master-left waypoint))))
	((:right :same)
	 (setf (waypoint-master-right waypoint)
	       (place-in-tree :right (waypoint-master-right waypoint))))))))


;; No idea if any of this is going to work out
(defun add-river-to-crd (crd entry exit)
  (let* ((crd-paths (ensure-crd-paths crd))
	 (entry-point (ensure-waypoint entry crd-paths))
	 (exit-point (ensure-waypoint exit crd-paths))
	 (new-river (make-crd-river :entry-point entry-point
				    :exit-point exit-point
				    :crd-paths crd-paths)))

    ;; Initialize those with no master:
    (cond ((not (waypoint-master entry-point))
	   (setf (waypoint-master entry-point) new-river))
	  (t (push-sub-river new-river entry-point)))

    (cond ((not (waypoint-master exit-point))
	   (setf (waypoint-master exit-point) new-river))
	  (t (push-sub-river new-river exit-point)))
    ))

(defun waypoint-total-natural-features (waypoint)
  (if (waypoint-master waypoint)
      (+ 1
	 (length (waypoint-master-left waypoint))
	 (length (waypoint-master-right waypoint)))
      0))

(defun waypoint-natural-left-count (waypoint)
  (length (waypoint-master-left waypoint)))
(defun waypoint-natural-right-count (waypoint)
  (length (waypoint-master-right waypoint)))
    
(defun draw-rivers (crd world view-state)
  (let* ((cairo-surface
	   (cairo:create-image-surface-for-data
	    (buffer view-state) :argb32
	    (width view-state) (height view-state)
	    (* 4 (width view-state))))
	 (cairo-context (cairo:create-context cairo-surface))
	 (crd-paths (gethash crd *crd-paths*)))

    ;; natural features won't change. They should be all drawn at once!
    (when crd-paths
      (when (crd-paths-trunks crd-paths)
	(draw-trunks crd (crd-paths-trunks crd-paths) world view-state cairo-context))
    
    )))

(defun testrivers ()
  (add-river-to-crd (crd 0 0) :n :s)
  (add-river-to-crd (crd 0 0) :nne :e)
  (add-river-to-crd (crd 0 0) :n :sw)
  (add-river-to-crd (crd 0 0) :nnw :sw))

(defun draw-trunks (crd trunk-list world view-state cairo-context)
  (let* ((window-centre-x-pix (/ (width view-state) 2))
	 (window-centre-y-pix (/ (height view-state) 2))

	 (origin-x (- window-centre-x-pix (centre-x view-state)))
	 (origin-y (- window-centre-y-pix (centre-y view-state)))

	 (r (hex-r view-state))
	 (half-r (/ r 2))
	 (three-quarters-r (* r 3/4))
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
      (cairo:set-source-rgb 0.0 0.0 0.7)
      (cairo:set-line-width 1.0)

      (destructuring-bind (master-trunk left-slaves . right-slaves)
	  trunk-list
	(let* ((master-entry (waypoint-vertex (crd-river-entry master-trunk)))
	       (master-exit (waypoint-vertex (crd-river-exit master-trunk)))
	       (master-entry-crd
		 (vertex-crd r master-entry hex-centre-x hex-centre-y))
	       (master-exit-crd
		 (vertex-crd r master-exit hex-centre-x hex-centre-y)))
	  (cairo:move-to (x master-entry-crd) (y master-entry-crd))
	  (cairo:line-to hex-centre-x hex-centre-y)
	  (cairo:line-to (x master-exit-crd) (y master-exit-crd))
	  (cairo:stroke)
#|
	  (dolist (trunk slave-trunks)
	    (let* ((slave-entry (waypoint-vertex (crd-river-entry trunk)))
		   (slave-exit (waypoint-vertex (crd-river-exit trunk)))
		   (from-master (right-or-left slave-entry slave-exit
					       master-entry master-exit)))
	      ;; TODO: Will need to have a centre vert ordering
	      ))
|#
      )))))

;; Now this is PRETTY similar to #'unit-hex-crd,
;; except that the coordinate system is Y-inverted...
(defun vertex-crd (r dir &optional (x+ 0) (y+ 0))
  (ntranslate
   (case dir
     (:N (crd 0.0 (* -1 r +sin60+)))
     (:NNE (nrotate (crd (* r +sin60+) (* -0.5 r))
		    (* 1/2 +sf-pi+)))
     (:NE (nrotate (crd (* r +sin60+) 0.0)
		   (* 1/6 +sf-pi+)))
     (:E (crd r 0.0))
     (:SE (nrotate (crd (* r +sin60+) 0.0)
		   (* -1/6 +sf-pi+)))
     (:SSE (nrotate (crd (* r +sin60+) (* 0.5 r))
		    (* -1/2 +sf-pi+)))
     (:S (crd 0.0 (* r +sin60+)))
     (:SSW (nrotate (crd (* r +sin60+) (* -0.5 r))
		    (* -1/2 +sf-pi+)))
     (:SW (nrotate (crd (* r +sin60+) 0.0)
		   (* 7/6 +sf-pi+)))
     (:W (crd (* -1 r) 0.0))
     (:NW (nrotate (crd (* r +sin60+) 0.0)
		   (* 5/6 +sf-pi+)))
     (:NNW (nrotate (crd (* r +sin60+) (* 0.5 r))
		    (* 1/2 +sf-pi+))))
   x+ y+))
