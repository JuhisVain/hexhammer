(in-package :hexhammer)

;; Doubly linked integer list:
(defstruct (link (:print-object link-printer))
  (left nil :type (or null link))
  (this 0 :type (signed-byte 8))
  (right nil :type (or null link)))

;; Deque:
(defstruct (linkage (:print-object linkage-printer))
  (leftmost nil :type (or null link))
  (rightmost nil :type (or null link)))

(defun link-printer (this stream)
  (format stream "~d=~a" (link-this this) (link-right this)))

(defun linkage-printer (this stream)
  (format stream "{~a}" (linkage-leftmost this)))

(defun push-link-left (new-link link)
  (setf (link-right new-link) link
	(link-left link) new-link)
  new-link)

(defun push-link-right (new-link link)
  (setf (link-left new-link) link
	(link-right link) new-link)
  new-link)

(defun pop-link-left (link)
  (prog1 (link-this link)
    (psetf (link-left (link-right link)) nil
	   ;; Should not be needed:
	   (link-right link) nil)))

(defun pop-link-right (link)
  (prog1 (link-this link)
    (psetf (link-right (link-left link)) nil
	   ;; Should not be needed:
	   (link-left link) nil)))

(defun push-left (element linkage)
  (if (linkage-leftmost linkage)
      (setf (linkage-leftmost linkage)
	    (push-link-left (make-link :this element)
			    (linkage-leftmost linkage)))
      (setf (linkage-leftmost linkage) (make-link :this element)
	    (linkage-rightmost linkage) (linkage-leftmost linkage))))

(defun push-right (element linkage)
  (if (linkage-rightmost linkage)
      (setf (linkage-rightmost linkage)
	    (push-link-right (make-link :this element)
			     (linkage-rightmost linkage)))
      (setf (linkage-rightmost linkage) (make-link :this element)
	    (linkage-leftmost linkage) (linkage-rightmost linkage))))

(defun pop-left (linkage)
  (when (linkage-leftmost linkage)
    (let* ((leftmost (linkage-leftmost linkage))
	   (value (link-this leftmost))
	   (second (link-right leftmost)))
      (if second
	  (pop-link-left leftmost)
	  (setf (linkage-rightmost linkage) nil))
      (setf (linkage-leftmost linkage) second)
      value)))

(defun pop-right (linkage)
  (when (linkage-rightmost linkage)
    (let* ((rightmost (linkage-rightmost linkage))
	   (value (link-this rightmost))
	   (second-last (link-left rightmost)))
      (if second-last
	  (pop-link-right rightmost)
	  (setf (linkage-leftmost linkage) nil))
      (setf (linkage-rightmost linkage) second-last)
      value)))

(defun peek-left (linkage)
  (if (linkage-leftmost linkage)
      (link-this (linkage-leftmost linkage))
      nil))

(defun peek-right (linkage)
  (if (linkage-rightmost linkage)
      (link-this (linkage-rightmost linkage))
      nil))
