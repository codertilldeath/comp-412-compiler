
(defpackage :parser
  (:use :cl :alexandria)
  (:import-from :scanner-table
                :lookup)
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
  (:export
   :print-lexemes
   :parse-file
   :print-ir))

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

;; Print the lexemes and parts of speech and line numbers
(defun pprint-lexeme (ch l)
  (declare ((vector character *) l))
  (if (string= l "
")
      (format t "{ ~a, \"\\n\" }" (lookup ch))
      (format t "{ ~a, ~S } " (lookup ch) l)))

(defun print-lexemes (file)
  (let ((linum 1))
    (declare (fixnum linum))
    (with-open-file (stream file)
      (loop for (pos . lex) = (follow-word stream)
         while (not (eof-char? pos))
         do
           (progn
             (format t "~a: " linum)
             (pprint-lexeme pos lex)
             (format t "~%")
             (case pos
               (11 (format t (report-lex-error lex)))
               (9 (incf linum))))))))

;; Report whether parsing was successful

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
        (ll (make-LL)))
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
           (incf count)
         finally
           (when success
             (format t "Successfully parsed file! ~a ILOC commands parsed.~%" count)))
      ll)))

;; Print the ir
(defun print-ir (file)
  (let ((success t)
        (count 0))
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
                   do (incf count)
                      (pprint-ir (make-internal line))
            finally
               (when success
                 (format t "Successfully parsed file! ~a ILOC commands parsed.~%" count))))))
