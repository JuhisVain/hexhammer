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
