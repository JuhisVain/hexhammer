(in-package :hexhammer)

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

(defun merge-child-node (child-node children)
  (merge 'list (list child-node) children #'< :key #'node-priority))

(defun add-child (child priority parent-node prigraph)
  (let* ((priorities (prigraph-priorities prigraph))
	 (children-nodes (gethash child priorities))
	 (new-child-node (make-node :key child
				    :priority priority
				    :parent (node-key parent-node))))

    (pushnew child (node-children parent-node) :test #'equalp)
    (cond ((null (prigraph-set prigraph)) ; merge child into it's position
	   (setf (gethash child priorities)
		 (merge-child-node new-child-node children-nodes)))
	  ((< priority (car children-nodes)) ; replace old child with better child
	   (setf (gethash child priorities)
		 (list new-child-node))))))
