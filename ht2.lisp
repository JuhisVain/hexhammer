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

(defun merge-child-node (child-node parent-node) ; but parent-node is part of child-node...?
  ;; This isn't supposed to be ordered??
  (setf (node-children parent-node)
	(merge 'list (list child-node) (node-children parent-node)
	       #'< :key #'node-priority)))

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
	       (merge-child-node node (node-parent node))
	       (setf (gethash (node-key node) (prigraph-priorities prigraph))
		     (list node)))))
	  (t
	   (merge-child-node node (node-parent node))
	   (setf (gethash (node-key node) (prigraph-priorities prigraph))
		 (merge 'list (list node) old-nodes
			#'< :key #'node-priority))))))

(defun add-child (child priority parent-node prigraph)
  (let ((new-child-node (make-node :key child
				   :priority priority
				   :parent parent-node)))
    (when (store-priority new-child-node prigraph)
      new-child-node)))

(defun get-nodes (key prigraph)
  (gethash key (prigraph-priorities prigraph)))

(defun highest-priority (key prigraph)
  (let ((first (first (get-nodes key prigraph))))
    (when first (node-priority first))))
