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
    :accessor crd-paths-tributaries)
   (centre
    :initarg :centre
    :accessor crd-paths-centre
    )))

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

(defun make-crd-paths (&key trunks tributaries centre)
  (make-instance 'crd-paths :trunks trunks :tributaries tributaries :centre centre))

(defun ensure-crd-paths (crd)
  (or (gethash crd *crd-paths*)
      (setf (gethash crd *crd-paths*)
	    (make-crd-paths))))

(defun ensure-waypoint (dir/river crd-paths)
  (or (find-point dir/river crd-paths)
      (waypoint dir/river)))

(defun make-crd-river (&key entry exit)
  (make-instance 'crd-river :entry entry :exit exit))

(defun make-waypoint (&key vertex master master-left master-right)
  (make-instance 'waypoint
		 :vertex vertex :master master
		 :master-left master-left :master-right master-right))
(defun make-joinpoint (&key trunk master master-left master-right)
  (make-instance 'joinpoint
		 :trunk trunk :master master
		 :master-left master-left :master-right master-right))

(defmethod waypoint ((point symbol))
  (make-waypoint :vertex point))

(defmethod waypoint ((point crd-river))
  (make-joinpoint :trunk point))

(defun find-point (point crd-paths)
  (when (eq point :CEN)
    (return-from find-point (crd-paths-centre crd-paths)))
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

'(defun waypoint-add-river (waypoint river)
  (let* ((master (waypoint-master waypoint))
	 (master-entry-dir (get-point (crd-river-entry master)))
	 (master-exit-dir (get-point(crd-river-exit master))))
    ))

'(defun add-river (river crd-paths)
  (cond ((trunkp river)
	 (crd-river-entry river))))

'(defun def-river-paths (crd entry exit)
  (let* ((crd-paths (ensure-crd-paths crd))
	 (entry-point (ensure-waypoint entry crd-paths))
	 (exit-point (ensure-waypoint exit crd-paths)))
    (cond ((hex-vertexp entry exit)
	   (make-crd-river
	    :entry entry-point
	    :exit exit-point)
	   ))))
