(in-package :hexhammer)
(setf *print-circle* t)

(defstruct node
  (key nil)
  (priority 0 :type (integer 0)) ; Smaller is more priorious
  (parent nil)
  (children nil :type list))

(defstruct (prigraph
	    (:constructor make-raw-priority-graph (root-node &optional (set nil))))
  (priorities (make-hash-table :test 'equalp
			       :size 100))
  (root-node nil :type node)
  (set nil :type boolean))

(defun make-prigraph (root)
  (let* ((root-node (make-node :key root))
	 (graph (make-raw-priority-graph root-node)))
    (setf (gethash root (prigraph-priorities graph)) (list root-node))
    graph))

(declaim (inline get-nodes))
(defun get-nodes (key prigraph)
  (gethash key (prigraph-priorities prigraph)))

(declaim (inline push-child-node))
(defun push-child-node (child-node parent-node) ; but parent-node is part of child-node...?
  (push child-node (node-children parent-node)))

(defmacro sort-push-node (node nodes)
  "Destructively inserts NODE into the sorted list NODES."
  (let ((pre (gensym))
	(post (gensym))
	(sort-push-block (gensym))
	(letnode (gensym)))
    `(block ,sort-push-block
       (let ((,letnode ,node))
	 (if (or (null ,nodes)
		 (< (node-priority ,letnode)
		    (node-priority (car ,nodes))))
	     (push ,letnode ,nodes)
	     (loop for ,pre on ,nodes
		   for ,post on (cdr ,nodes)
		   when (or (null ,post)
			    (< (node-priority ,letnode)
			       (node-priority (car ,post))))
		     do (rplacd ,pre (cons ,letnode ,post))
			(return-from ,sort-push-block ,nodes)))))))

(defmacro sort-push (obj list predicate)
  (let ((sort-push-block (gensym "SORT-PUSH"))
	(letobj (gensym "OBJ"))
	(letlist (gensym "LIST"))
	(pre (gensym "PRE"))
	(post (gensym "POST")))
    `(block ,sort-push-block
       (let ((,letobj ,obj)
	     (,letlist ,list))
	 (if (or (null ,list)
		 (funcall ,predicate ,letobj (car ,letlist)))
	     (push ,letobj ,list) ;; Pushing to ,list will work when lexical arg list empty
	     (loop for ,pre on ,letlist
		   and ,post = (cdr ,letlist) then (cdr ,post)
		   when (or (null ,post)
			    (funcall ,predicate ,letobj (car ,post)))
		     do (rplacd ,pre (cons ,letobj ,post))
			(return-from ,sort-push-block ,letlist)))))))

;;; Appears to take about twice the time of above macro
;;; If we declaim so:
;;(declaim (inline nsort-insert))
;; and say that keyed-x and funcall key (car list) are fixnums below we could get better perf
(defun nsort-insert (x list predicate &key key)
  "Insert X into it's position in sorted list LIST according to PREDICATE.
May modify LIST."
  (declare (optimize speed (compilation-speed 0))
	   (list list)
	   (function predicate)
	   ((or function null) key))
  (let* ((key (if key key #'identity))
	 (keyed-x (funcall key x)))
    (labels ((rec-nsortins (x list)
	       (cond ((null list)
		      (setf list (list x)))
		     ((funcall predicate
			       keyed-x
			       (funcall key (car list)))
		      (rplaca (rplacd list (cons (car list) (cdr list)))
			      x))
		     ((null (cdr list))
		      (rplacd list (list x)))
		     (t
		      (rec-nsortins x (cdr list))))))
      (rec-nsortins x list)
      list)))
(defun nsi-test ()
  (let ((l (sort (loop repeat 100000
		       collect (make-node :priority (random 100000)))
		 #'<= :key #'node-priority))
	(add-list (loop repeat 1000
			collect (make-node :priority (random 100000)))))
    (time (progn (dolist (n add-list)
		   (setf *foo*
			 (nsort-insert n l #'< :key #'node-priority)
			 ;;(sort-push-node n l)
			 ))
		 nil))))

(defun store-priority (node prigraph)
  "Will return this NODE's key's priority list or NIL if adding NODE failed."
  (let ((old-nodes (gethash (node-key node) (prigraph-priorities prigraph)))
	(node-priority (node-priority node)))
    (cond ((prigraph-set prigraph) ; There can be only one!
	   (when (< node-priority (node-priority (car old-nodes)))
	     (let ((old-parent (node-parent (car old-nodes))))
	       (setf (node-children old-parent)
		     (delete (node-key node)
			     (node-children old-parent)
			     :key #'node-key :test #'equalp))
	       (push-child-node node (node-parent node))
	       (setf (gethash (node-key node) (prigraph-priorities prigraph))
		     (list node)))))
	  (t
	   (push-child-node node (node-parent node))
	   (setf (gethash (node-key node) (prigraph-priorities prigraph))
		 (sort-push-node node old-nodes))))))

(defun add-child (child priority parent-node prigraph)
  (let ((new-child-node (make-node :key child
				   :priority priority
				   :parent parent-node)))
    (when (store-priority new-child-node prigraph)
      new-child-node)))

(defun highest-priority (key prigraph)
  (let ((first (first (get-nodes key prigraph))))
    (when first (node-priority first))))
