(defpackage :scheduler
  (:use :cl :alexandria)
  (:export :schedule))

(in-package :scheduler)

(defun schedule (ir)
  (let ((ll (ll::make-LL)))
    ;; (loop for node = (ll::tail ir) then (ll::prev node)
    ;;    while node
    ;;    for data = (ll::data node)
    ;;    do
    ;;      (ll:insert-back ll data))
    (make-graph ir)
    ;; Ready instructions will be the list of instructions that have no predecessors
    ll))
