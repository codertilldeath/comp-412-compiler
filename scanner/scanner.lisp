(defpackage 412fe.scanner
  (:use :cl)
  (:export :scan
           :*parts-of-speech*))

(in-package :412fe.scanner)

(defun scan (f)
  (print f))

(defun valid-terminator (current-termination next-char)
  (or (not next-char)
      (when (< current-termination *start-state*)
        (aref *valid-terminators* current-termination (char-code next-char)))))

(defun follow-word (stream)
  (let ((state *start-state*))
    (loop for ch = (when (not (valid-terminator state
                                                (peek-char nil stream nil)))
                     (read-char stream nil))
       while ch
       do
         (let ((c (char-code ch)))
           (setf state
                 (aref *table* state c))))
    (lookup state)))

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
       while (and (not (eq ch 'error))
                  (not (eq ch 'start)))
       do
         (progn
           (format t "~a " ch)
           (when (eq ch 'newline)
             (format t "~%")))
       finally
         (format t "~%~a~%" ch))))
