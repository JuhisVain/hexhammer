(in-package :hexhammer)

#|
There needs to be a user controlled element (ie. an army) encompassing
individual units (tanks, foot soldiers)

An army's top level units should be the primary means of determining it's
movement costs across different terrains.

Example on some specific terrain:
modern artillery -> fm 1       ; crew pushing
                    size 20
                    inv 0

footman -> fm 2       ; marching
           size 1
           inv 0

jeep -> fm 10         ; normal driving
        size 15
        inv 5

truck -> fm 9
         size 70
         inv 50

ship -> fm 15
        

horse -> fm 5
         size 8
         inv 2

|#
(defmacro quotes (&rest args)
  `(list ,@(mapcar #'(lambda (x) `(quote ,x)) args)))


(defstruct location
  (crd :type (or crd null)))

(defvar *terrain-types*
  (quotes plains
	  swamp
	  desert
	  forest
	  jungle
	  lake
	  sea
	  ))

(defvar *terrain-modifiers*
  (quotes rough))

(defvar *terrain-states*
  (quotes dry wet frozen))

(defstruct movement-type
  (name "" :type string)
  (costs '((plains . 5))))



(defstruct unit-type
  (name "" :type string)
  (movement))

(defmacro defunit ())
