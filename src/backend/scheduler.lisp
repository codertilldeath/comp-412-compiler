(defpackage :scheduler
  (:use :cl :alexandria)
  (:export :schedule))

(in-package :scheduler)

(defparameter *ready* nil)

(defun update-actives (actives)
  (let ((new-ready '()))
    (loop for i in actives
       for node = (aref *node-table* i)
       do
         (decf (node-exec-time node))
         (when (zerop (node-exec-time node))
           (loop for i in (get-predecessors i)
              for node = (aref *node-table* i)
              do (decf (node-dep-left node))
                (when (zerop (node-dep-left node))
                  (push i new-ready)))))
    new-ready))

(defun get-schedules ()
  (let* ((mul-inst (loop for i in *ready*
                      for inst = (unless (eq :memop (ir::category (node-inst (aref *node-table* i))))
                                   i)
                      while (null inst)
                      finally (return inst)))
        (mem-inst (loop for i in *ready*
                     for inst = (unless (or (eq :mult (ir::opcode (node-inst (aref *node-table* i))))
                                            (and mul-inst (= i mul-inst)))
                                  i)
                     while (null inst)
                     finally (return inst))))
    (setf *ready* (remove mul-inst (remove mem-inst *ready*)))
    (cons mem-inst mul-inst)))

(defun schedule (ir)
  (let ((ll (ll::make-LL)))
    (make-graph ir)
    (setf *ready* (get-leaves))
    (let ((active '())
          (left (ll::size ir)))
      (loop while (> left 0)
         do
           (destructuring-bind (mem-inst . mul-inst) (get-schedules)
             (if (null mem-inst)
                 (ll::insert-back ll
                                  (ir::make-IR :opcode :nop
                                               :category :nop))
                 (let ((inst (node-inst (aref *node-table* mem-inst))))
                   (push mem-inst active)
                   (ll::insert-back ll inst)
                   (decf left)))
             (if (null mul-inst)
                 (ll::insert-back ll
                                  (ir::make-IR :opcode :nop
                                               :category :nop))
                 (let ((inst (node-inst (aref *node-table* mul-inst))))
                   (push mul-inst active)
                   (ll::insert-back ll inst)
                   (decf left))))
           (when-let (new-ready (update-actives active))
             (appendf *ready* new-ready))))
    ll))
