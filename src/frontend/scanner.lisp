(defpackage 412fe.scanner
  (:use :cl)
  (:import-from :412fe.table
                :lookup
                :*parts-of-speech*
                :*start-state*
                :*valid-terminators*
                :*valid-lexemes*
                :*table*)
  (:export :scan))

(in-package :412fe.scanner)

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
             (unless (member ch '(#\space #\tab))
               (format s "~a" ch))
             (setf state
                   (aref *table* state c)))))
    (cons state fstr)))
