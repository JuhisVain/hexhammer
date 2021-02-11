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
(defvar *crd-paths* (make-hash-table :test 'equal))

(defclass crd-paths ()
  ((trunks
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

(defun make-crd-paths (&key trunks tributaries)
  (make-instance 'crd-paths :trunks trunks :tributaries tributaries
			    :centre (make-waypoint :vertex :cen)))

(defun ensure-crd-paths (crd)
  (or (gethash crd *crd-paths*)
      (setf (gethash crd *crd-paths*)
	    (make-crd-paths))))

(defun ensure-waypoint (dir/river crd-paths)
  (or (find-point dir/river crd-paths)
      (waypoint dir/river)))

(defun make-crd-river (&key entry exit crd-paths)
  (let ((river (make-instance 'crd-river)))
    (when entry
      (setf (crd-river-entry river)
	    (ensure-waypoint entry crd-paths)))
    (when exit
      (setf (crd-river-exit river)
	    (ensure-waypoint exit crd-paths)))
    river))

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

(defun find-point (point crd-paths)
  (let ((defined-points
	  (mapcan #'(lambda (crd-riv)
		      (list (crd-river-entry crd-riv)
			    (crd-river-exit crd-riv)))
		  (append (crd-paths-trunks crd-paths)
			  (crd-paths-tributaries crd-paths)))))
    (find point defined-points :key #'get-point :test #'eq)))

(defun hex-vertexp (dir &rest more-dirs)
  (and (typep dir 'hex-vertex)
       (or (null more-dirs)
	   (apply #'hex-vertexp (first more-dirs) (rest more-dirs)))))

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


;;TEST
(defparameter *xxx* '(5 (7 NIL NIL)))
(defun xxx (addme tree)
  (if (<= addme (car tree))
      (list addme tree)
      (list (car tree)
	    (if (cadr tree)
		(xxx addme (cadr tree))
		(list addme nil nil)))))

;; TODO: test this
(defun push-sub-river (river waypoint)
  (labels ((place-in-tree (dir tree)
	     (if (eq dir
		     (right-or-left
		      (waypoint-vertex (crd-river-entry river))
		      (waypoint-vertex (crd-river-exit river))
		      (waypoint-vertex (crd-river-entry (car tree)))
		      (waypoint-vertex (crd-river-exit (car tree)))))
		 (list river tree)
		 (list (car tree)
		       (if (cadr tree)
			   (place-in-tree dir (cadr tree))
			   (list river nil nil))))))
    
    (let ((direction
	    (right-or-left
	     (waypoint-vertex (crd-river-entry river))
	     (waypoint-vertex (crd-river-exit river))
	     (waypoint-vertex (crd-river-entry (waypoint-master waypoint)))
	     (waypoint-vertex (crd-river-exit (waypoint-master waypoint))))))
      (case direction
	(:left
	 (setf (waypoint-master-left waypoint)
	       (place-in-tree :left (waypoint-master-left waypoint))))
	((:right :same)
	 (setf (waypoint-master-right waypoint)
	       (place-in-tree :right (waypoint-master-right waypoint))))))))


;; No idea if any of this is going to work out
'(defun add-river-to-crd (crd entry exit)
  (let* ((crd-paths (ensure-crd-paths crd))
	 (entry-point (ensure-waypoint entry crd-paths))
	 (exit-point (ensure-waypoint exit crd-paths))
	 (new-river (make-crd-river)))

    ;; Initialize those with no master:
    (if (not (waypoint-master entry-point))
	(setf (waypoint-master entry-point) new-river)
	(case (right-or-left entry exit
			     (crd-river-entry (waypoint-master entry-point))
			     (crd-river-exit (waypoint-master entry-point)))
	  (:LEFT (setf (waypoint-master-left entry-point)))))

    
    (if (not (waypoint-master exit-point))
	(setf (waypoint-master exit-point) new-river))
    
    
    ))
    
