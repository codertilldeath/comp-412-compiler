 (defpackage 412fe.scanner
  (:use :cl)
  (:import-from :412fe.scanner.table
                :lookup
                :*parts-of-speech*
                :*start-state*
                :*valid-terminators*
                :*table*)
  (:export :scan))

(in-package :412fe.scanner)

(defun scan (f)
  (print f))

(defun valid-terminator (current-termination next-char)
  (or (not next-char)
      (when (< current-termination *start-state*)
        (aref *valid-terminators* current-termination (char-code next-char)))))

(defun follow-word (stream)
  (let ((state *start-state*)
        (fstr (make-array '(0) :element-type 'base-char
                          :fill-pointer 0 :adjustable t)))
    (with-output-to-string (s fstr)
      (loop for ch = (when (not (valid-terminator state
                                                  (peek-char nil stream nil)))
                       (read-char stream nil))
         while ch
         do
           (let ((c (char-code ch)))
             (format s "~a" ch)
             (setf state
                   (aref *table* state c)))))
    (cons state fstr)))

(follow-word (make-string-input-stream "0"))

(defun follow (word)
  (with-input-from-string (stream word)
    (loop for ch = (follow-word stream)
       while (and (not (eq ch 'error))
                  (not (eq ch 'start)))
       do
         (format t "~a~%" ch))))

(defun follow-file (file)
  (with-open-file (stream file)
    (loop for ch = (follow-word stream)
       while (and (not (eq (lookup (car ch)) '412fe.scanner.table::error))
                  (not (eq (lookup (car ch)) '412fe.scanner.table::start)))
       do
         (progn
           (format t "~a " (cons (lookup (car ch)) (cdr ch)))
           (when (eq (lookup (car ch)) '412fe.scanner.table::newline)
             (format t "~%")))
       finally
         (format t "~%~a~%" (lookup (car ch))))))
