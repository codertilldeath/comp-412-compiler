(defpackage 412fe.scanner
  (:use :cl)
  (:export :scan
           :*parts-of-speech*))

(in-package :412fe.scanner)

(defun scan (f)
  (print f))

(defun next-word (stream)
  (read-char stream))
