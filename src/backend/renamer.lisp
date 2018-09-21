(defpackage :renamer
  (:use :cl :alexandria)
  (:export
   :rename-registers))

(in-package :renamer)

(defun rename-registers (ll)
  (loop for i = (ll::ll-tail ll) then (ll::ll-node-prev i)
       while i
       for data = (ll::ll-node-data i)
       do (print (ir::IR-opcode data))))

