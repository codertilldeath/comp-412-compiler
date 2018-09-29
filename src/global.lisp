(defpackage :global
  (:use :cl)
  (:export :compile-start
           :*max-register*
           :*max-live*
           :*current-live*))

(in-package :global)

(defvar *max-register* 0)
(defvar *current-live* 0)
(defvar *max-live* 0)

(defun compile-start ()
  (setq *max-register* 0
        *max-live* 0
        *current-live* 0))
