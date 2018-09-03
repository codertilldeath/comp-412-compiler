(defpackage :412fe.parser
  (:use :cl)
  (:import-from :412fe.table
                :lookup)
  (:import-from :412fe.scanner
                :follow-word)
  (:import-from :412fe.parser.errors
                :any-errors
                :report-lex-error
                :report-eof-error
                :report-incorrect-word)
  (:export :print-lexemes
           :parse-file))

(in-package :412fe.parser)

(defun pprint-lexeme (ch l)
  (if (string= l "
")
      (format t "{ ~a, \"\\n\" }" (lookup ch))
      (format t "{ ~a, ~S } " (lookup ch) l)))

(defun slurp-sentence (stream lex)
  (cons lex
        (loop for (p . lex) = (follow-word stream)
           while (not (eq (lookup p)
                          :newline))
           collect (cons p lex))))

(defun next-sentence (stream pos lex linum)
  (let* ((result (slurp-sentence stream (cons pos lex)))
         (errors (any-errors pos lex result)))
    (if errors
        (format t "On line ~a: ~a" linum errors)
        result)))

(defun parse-file (file)
  (let ((success t))
    (with-open-file (stream file)
      (loop for (pos . lex) = (follow-word stream)
         while (not (eq (lookup pos) :start))
         for linum from 1
         for i = (next-sentence stream
                                pos lex
                                linum)
         do (unless i 
              (setf success nil))
         collect i))
    (when success
      (format t "Successfully parsed file!"))))

(defun get-ir (file)
  (let ((success t)
        ir)
    (with-open-file (stream file)
      (loop for (pos . lex) = (follow-word stream)
         while (not (eq (lookup pos) :start))
         for linum from 1
         for i = (next-sentence stream
                                pos lex
                                linum)
         do (if (null i)
                (setf success nil)
                ;; (setf ir )
                )))
    (when success
      ir)))

(defun print-ir (file)
  (let ((success t))
    (with-open-file (stream file)
      (loop for (pos . lex) = (follow-word stream)
         while (not (eq (lookup pos) :start))
         for linum from 1
         for i = (next-sentence stream
                                pos lex
                                linum)
         do (unless i 
              (setf success nil))
         collect i))
    (when success
      (format t "Successfully parsed file!"))))

(defun print-lexemes (file)
  (let ((linum 1))
    (with-open-file (stream file)
      (loop for (pos . lex) = (follow-word stream)
         while (not (eq (lookup pos)
                        :start))
         finally
           (when (eq :error (lookup pos))
             (report-lex-error lex))
         do
           (progn
             (format t "~a: " linum)
             (pprint-lexeme pos lex)
             (format t "~%")
             (case (lookup pos)
               (:error (format t (report-lex-error lex)))
               (:newline (incf linum))))))))
