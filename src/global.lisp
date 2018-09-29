(defpackage :global
  (:use :cl)
  (:export :compile-start
           :*max-register*))

(in-package :global)

(defvar *max-register* 0)

(defun compile-start ()
  (setq *max-register* 0))
