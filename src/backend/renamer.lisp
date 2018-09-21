(defpackage :412fe.renamer
  (:use :cl :alexandria)
  (:export
   :rename-registers))

(in-package :412fe.renamer)

(defun rename-registers (ll)
  (loop for i = (412fe.ll::ll-head ll) then (412fe.ll::ll-node-prev i)
       while i
       for data = (412fe.ll::ll-node-data i)
       do (print (412fe.ir::IR-opcode data))))

