(in-package :hexhammer)

(defconstant +sin60+ (sqrt (/ 3 4)))
(defconstant +cos60+ 0.5)
(defconstant +tan60+ (coerce (tan (/ pi 3)) 'single-float))
(defconstant +cos30+ (coerce (cos (/ pi 6)) 'single-float))
(defconstant +sin30+ (coerce (sin (/ pi 6)) 'single-float))
(defconstant +sf-pi+ (coerce pi 'single-float))

(defstruct range-deque
  (initp nil :type boolean)
  (left 0 :type fixnum) ; aka. start
  (range 0 :type fixnum))

(defun range-deque-right (ranged)
  (+ (range-deque-left ranged)
     (range-deque-range ranged)))

(defun (setf range-deque-right) (new-right ranged)
  (let ((diff (- new-right (range-deque-right ranged))))
    (if (and (or (= (signum diff)
		    (signum (range-deque-range ranged)))
		 (zerop (range-deque-range ranged)))
	     (= 1 (abs diff)))
	(incf (range-deque-range ranged) diff)
	(error "Bad values~%"))))

(defun push-left (element ranged)
  (let ((diff (- (range-deque-left ranged)
		 element)))
    (cond ((not (range-deque-initp ranged)) ;; Initialize
	   (setf (range-deque-left ranged) element
		 (range-deque-initp ranged) t))
	  ((/= 1 (abs diff)) ;; Element does not follow LEFT by 1
	   (error "Trying to push with bad interval ~a~%" diff))
	  ((zerop (range-deque-range ranged)) ;; Initialize range sign
	   (setf (range-deque-left ranged) element
		 (range-deque-range ranged) diff))
	  ((/= diff ;; Element follows in wrong direction
	       (signum (range-deque-range ranged)))
	   (error "Trying to push with bad sign ~a to ~a~%"
		  diff (range-deque-range ranged)))
	  (t
	   (setf (range-deque-left ranged) element)
	   (incf (range-deque-range ranged) diff)))
    nil))

(defun push-right (element ranged)
  (cond ((not (range-deque-initp ranged)) ;; Initialize
	 (setf (range-deque-left ranged) element
	       (range-deque-initp ranged) t))
	(t (setf (range-deque-right ranged) element))))

(defun pop-left (ranged)
  (cond ((zerop (range-deque-range ranged))
	 (setf (range-deque-initp ranged) nil))
	((range-deque-initp ranged)
	 (let ((unit (signum (range-deque-range ranged))))
	   (incf (range-deque-left ranged) unit)
	   (decf (range-deque-range ranged) unit)))))

(defun pop-right (ranged)
  (cond ((zerop (range-deque-range ranged))
	 (setf (range-deque-initp ranged) nil))
	((range-deque-initp ranged)
	 (let ((unit (signum (range-deque-range ranged))))
	   (decf (range-deque-range ranged) unit)))))

(defun peek-left (ranged)
  (when (range-deque-initp ranged)
    (range-deque-left ranged)))

(defun peek-right (ranged)
  (when (range-deque-initp ranged)
    (range-deque-right ranged)))


#|
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
|#
