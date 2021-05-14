(in-package :hexhammer)

(defclass node-value ()
  ((priority :initform 0
	     :initarg :priority
	     :accessor priority)
   (parent :initform nil
	   :initarg :parent
	   :accessor parent)
   (children :initform nil
	     :initarg :children
	     :accessor children)))
(defmethod print-object ((object node-value) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "[~a]; parent: ~a, ~a children"
	    (priority object)
	    (parent object)
	    (length (children object)))))

(defclass hashtree ()
  ((tree :initform (make-hash-table :test 'equalp)
	 :reader tree
	 :type hash-table)
   (ordering :initarg :ordering
	     :reader ordering
	     :type function)))

(defclass keyed ()
  ((key :initarg :key
	:reader key
	:type function)
   (primary-keys :initform (make-hash-table :test 'equalp)
		 :reader keys
		 :type hash-table)))

(defclass keyed-hashtree (keyed hashtree)
  ())

(defmethod access :around (data (hashtree keyed))
  (call-next-method
   (gethash (funcall (key hashtree) data) (keys hashtree))
   hashtree))
(defmethod access (data (hashtree hashtree))
  (gethash data (tree hashtree)))

(defmethod (setf access) :around (new-node-value data (hashtree keyed))
  (let* ((key-key (funcall (key hashtree) data))
	 (old-node-key (gethash key-key (keys hashtree)))
	 (old-node (gethash old-node-key (tree hashtree))))
    (cond ((null old-node-key) ;; Completely new node
	   (setf (gethash key-key (keys hashtree)) data)
	   (call-next-method))
	  ((and old-node-key ;; New node has higher priority than old
		(funcall (ordering hashtree)
			 (priority new-node-value)
			 (priority old-node)))
	   
;	   (setf (children (parent old-node))
;		 (delete old-node-key (children (parent old-node))
;			 :test #'equalp))
	   
	   (remhash old-node-key (tree hashtree))
	   (setf (gethash key-key (keys hashtree)) data)
	   (call-next-method)))))


(defmethod (setf access) (new-node-value data (hashtree hashtree))
  (setf (gethash data (tree hashtree)) new-node-value))

(defun make-node (priority &optional parent children)
  (make-instance 'node-value
		 :priority priority
		 :parent parent
		 :children children))

(defun child-of (parent child hashtree)
  "Returns PARENT's children list starting from cons where car is equivalent to CHILD.
Returns NIL if not found."
  (loop with children = (children (access parent hashtree))
	with target = (funcall (key hashtree) child)
	for child-key in (mapcar #'(lambda (child)
				     (funcall (key hashtree) child))
				 children)
	for rest-children on children
	when (equalp target child-key)
	  do (return rest-children)))

(defmethod priority-ordered (prio-a prio-b hashtree)
  (funcall (ordering hashtree)
	   prio-a
	   prio-b))

(defmethod add-child-node (parent child child-priority hashtree)
  "Returns CHILD on success, NIL on failure."
  (let ((old-rest-child (child-of parent child hashtree)))
    (if old-rest-child ; parent already has child with this key?
	(when (priority-ordered ; new child has higher priority than old?
	       child-priority
	       (priority (access (car old-rest-child) hashtree))
	       hashtree)
	  (setf (car old-rest-child) child) ; replace old child with new
	  ;; This will write over old child, but grandchildren will be left dangling in hashtable:
	  (setf (access child hashtree) ; add node for new child
		(make-node child-priority parent))
	  child)
	;; using parent arg of no consequence here as long as hashtree-key is immutable
	(progn ; no old children with same key
	  (push child (children (access parent hashtree)))
	  (setf (access child hashtree)
		(make-node child-priority parent))
	  child))))


'(setf (access (list 5 'test 'test 'test) *ht*) (make-node 0))
'(add-child-node (list 5 1 2 3) (list 'a) 6 *ht*)
