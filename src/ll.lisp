(defpackage :ll
  (:use :cl :alexandria)
  (:export :make-LL
           :to-list
           :insert-back))

(in-package :ll)

(defstruct (LL :conc-name) head tail size)

(defstruct (ll-node :conc-name) data prev next)

(defun to-list (ll)
  (loop for i = (head ll) then (next i)
     while i
     collect (data i)))

(defun link-nodes (n1 n2)
  (setf (next n1) n2)
  (setf (prev n2) n1))

(defun insert-back (LL data)
  (let ((new-node (make-ll-node :data data)))
    (if (null (head LL))
        (setf (head LL) new-node)
        (link-nodes (tail LL) new-node))
    (setf (tail LL) new-node)
    (incf (size LL))
    LL))
