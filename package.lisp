(in-package :common-lisp-user)

(defpackage :hexhammer
  (:use #:cl)
  (:local-nicknames (:alex :alexandria)
		    (:sera :serapeum))
  (:import-from serapeum "EQ*"))
