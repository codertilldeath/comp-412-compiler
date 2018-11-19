(defpackage :scheduler
  (:use :cl :alexandria)
  (:export :schedule))

(in-package :scheduler)

(defparameter *ready* nil)

(defun update-actives (actives)
  (let ((new-ready '()))
    (loop for i in actives
       for n = (aref *node-table* i)
       do
         (decf (node-exec-time n))
         (loop for e in (get-predecessor-edges i)
            for i = (edge-source e)
            for node = (aref *node-table* i)
            do (when (= (edge-weight e) (node-exec-time n))
                 (decf (node-dep-left node))
                 (when (zerop (node-dep-left node))
                   (push i new-ready)))))
    new-ready))

(defun get-cost (i)
  (node-priority (aref *node-table* i)))

(defun get-schedules ()
  (let (mem-inst mem-prio
        mul-inst mul-prio
        reg1 reg1-prio
        reg2 reg2-prio)
    (loop for i in *ready*
       for node = (aref *node-table* i)
       for inst = (node-inst node)
       do
         (cond ((eq (ir::opcode inst) :|mult|)
                (when (or (null mul-inst)
                          (< mul-prio (node-priority node)))
                  (setf mul-inst i)
                  (setf mul-prio (node-priority node))))
               ((eq (ir::category inst) :memop)
                (when (or (null mem-inst)
                          (< mem-prio (node-priority node)))
                  (setf mem-inst i)
                  (setf mem-prio (node-priority node))))
               (t (when (or (null reg1)
                            (< reg1-prio (node-priority node)))
                    (setf reg2 reg1)
                    (setf reg2-prio reg1-prio)
                    (setf reg1 i)
                    (setf reg1-prio (node-priority node))))))
    (if (and (null mem-inst)
             (null mul-inst))
        (setf mul-inst reg1
              mem-inst reg2)
        (progn
          (when reg1
            (cond ((null mul-inst) (setf mul-inst reg1))
                  ((null mem-inst) (setf mem-inst reg1))
                  ((and reg2
                        (< mul-prio reg2-prio)
                        (< mem-prio reg2-prio))
                   (setf mul-inst reg1
                         mem-inst reg2))
                  ((and (< mul-prio reg1-prio)
                        (< mul-prio mem-prio))
                   (setf mul-inst reg1))
                  ((and (< mem-prio reg1-prio)
                        (< mem-prio mul-prio))
                   (setf mem-inst reg1))))))
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
           ;;(format t "~a~%" *ready*)
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
           ;; (let* ((tail (ll::tail ll))
           ;;        (prev (ll::prev tail)))
           ;;   (ir::string-2-instructions (ll::data prev) (ll::data tail) #'ir::virtual))
           (when-let (new-ready (update-actives active))
             (appendf *ready* new-ready))
           (setf active (remove-if (lambda (x)
                                     (zerop (node-exec-time (aref *node-table* x))))
                                   active))
           ))
    ll))
