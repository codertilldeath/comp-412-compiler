(defpackage :allocator
  (:use :cl :alexandria)
  (:import-from :renamer
                :rename-registers)
  (:export :allocate-registers))


(in-package :allocator)

(defun allocate-registers (ir registers)
  ir)
