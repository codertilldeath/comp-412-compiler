(defpackage :renamer
  (:use :cl :alexandria)
  (:export
   :rename-registers))

(in-package :renamer)

(defparameter *SR-to-VR* nil)
(defparameter *last-use* nil)
(defparameter *VR-name* nil)

(defun update (ir))

(defun rename-registers (ll register-max)
  (setf *VR-name* 0
        *last-use* (make-array register-max :element-type 'fixnum :initial-element -1)
        *SR-to-VR* (make-array register-max :element-type 'fixnum :initial-element -1))
  (loop for i = (ll::ll-tail ll) then (ll::ll-node-prev i)
     while i
     for data = (ll::ll-node-data i)
     do (progn
          (if ))
  ll)

