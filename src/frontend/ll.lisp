(defpackage :412fe.ll
  (:use :cl :alexandria)
  (:export :make-LL
           :to-list
           :insert-back))

(in-package :412fe.ll)

(defstruct LL head tail)

(defstruct ll-node data prev next)

(defun to-list (ll)
  (loop for i = (LL-head ll) then (ll-node-next i)
     while i
     collect (ll-node-data i)))

(defun link-nodes (n1 n2)
  (setf (ll-node-next n1) n2)
  (setf (ll-node-prev n2) n1))

(defun insert-back (LL data)
  (let ((new-node (make-ll-node :data data)))
    (if (not (null (LL-head LL)))
        (link-nodes (LL-tail LL) new-node)
        (setf (LL-head LL) new-node))
    (setf (LL-tail LL) new-node)
    LL))
