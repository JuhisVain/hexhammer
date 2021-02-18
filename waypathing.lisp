(in-package :hexhammer)

;;; NOTE: Bezier control points inside hex at 50%
;; of distance between verts or R, whichever longer. (should say shorter??)

;;; Rivers should live in the same hashtable as other paths
;; However the construction of infrastructure should not affect
;; the graphical position of rivers.

;; key: crd
(defvar *crd-paths* (make-hash-table :test 'equalp))



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

(defun draw-rivers (crd world view-state)
  (let* ((cairo-surface
	   (cairo:create-image-surface-for-data
	    (buffer view-state) :argb32
	    (width view-state) (height view-state)
	    (* 4 (width view-state))))
	 (cairo-context (cairo:create-context cairo-surface))
	 (crd-paths (gethash crd *crd-paths*)))
    
    ))

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
