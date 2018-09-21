
(defpackage :parser
  (:use :cl :alexandria)
  (:import-from :scanner
                :follow-word)
  (:import-from :parser.errors
                :any-errors
                :report-lex-error
                :report-eof-error
                :report-incorrect-word)
  (:import-from :ll
                :make-ll
                :insert-back)
  (:import-from :ir
   :make-internal
   :pprint-ir)
  (:export :parse-file))

(in-package :parser)

;; Helper funs
(defun empty-line? (line)
  (= (the fixnum (caar line)) 9))

(defun eof? (line)
  (= (the fixnum (caar line)) 13))

(defun comment? (p)
  (declare (fixnum p))
  (= p 10))

(defun eof-char? (p)
  (declare (fixnum p))
  (= p 13))

(defun slurp-sentence (stream)
  (loop for lex = (follow-word stream)
        for p = (car lex)
        when (not (comment? p))
          collect lex
        while (not (or (= p 9)
                       (= p 13)))))

(defun parse-file (file)
  (let ((success t)
        (count 0)
        (ll (make-LL :size 0)))
    (declare (fixnum count))
    (with-open-file (stream file)
      (loop for linum fixnum from 1
         for line = (slurp-sentence stream)
         while (not (eof? line))
         for errors = (any-errors (caar line) (cdar line) line)
         if errors
         do
           (format t "On line ~a: ~a" linum errors)
           (setf success nil)
         else if (not (empty-line? line))
         do
           (insert-back ll (make-internal line))
           (incf count))
      ll)))
