(defpackage :scheduler
  (:use :cl :alexandria)
  (:export :schedule))

(in-package :scheduler)

(defun update-actives (actives)
  (let ((new-ready '()))
    (loop for i in actives
       for node = (aref *node-table* i)
       do
         (decf (node-exec-time node))
         (when (zerop (node-exec-time node))
           (appendf new-ready (get-predecessors i))))
    new-ready))

(defun get-memop-inst (ready))

(defun get-mult-inst (ready))

(defun schedule (ir)
  (let ((ll (ll::make-LL)))
    ;; (loop for node = (ll::tail ir) then (ll::prev node)
    ;;    while node
    ;;    for data = (ll::data node)
    ;;    do
    ;;      (ll:insert-back ll data))
    (make-graph ir)
    ;; Ready instructions will be the list of instructions that have no predecessors
    (let ((ready *leafs*)
          (active '())
          (left (ll::size ir)))
      (loop while (> left 0)
         do
           (if (null ready)
               (progn 
                 (ll::insert-back ll (ir::make-IR :opcode :nop
                                                  :category :nop))
                 (ll::insert-back ll (ir::make-IR :opcode :nop
                                                  :category :nop)))
               (let* ((index (pop ready))
                      (inst (node-inst (aref *node-table* index))))
                 (push index active)
                 (ll::insert-back ll inst)
                 (decf left)
                 (if (null ready)
                     (ll::insert-back ll (ir::make-IR :opcode :nop
                                                      :category :nop))
                     (let* ((index (pop ready))
                            (inst (node-inst (aref *node-table* index))))
                       (push index active)
                       (ll::insert-back ll inst)
                       (decf left)))))
           (when-let (new-ready (update-actives active))
             (appendf ready new-ready))))
    ll))
