(defpackage :412fe.parser
  (:use :cl :alexandria)
  (:import-from :412fe.table
                :lookup)
  (:import-from :412fe.scanner
                :follow-word)
  (:import-from :412fe.parser.errors
                :any-errors
                :report-lex-error
                :report-eof-error
                :report-incorrect-word)
  (:import-from :412fe.ir
   :make-internal
   :pprint-ir)
  (:export
   :print-lexemes
   :parse-file
   :print-ir))

(in-package :412fe.parser)

;; Print the lexemes and parts of speech and line numbers
(defun pprint-lexeme (ch l)
  (if (string= l "
")
      (format t "{ ~a, \"\\n\" }" (lookup ch))
      (format t "{ ~a, ~S } " (lookup ch) l)))

(defun print-lexemes (file)
  (let ((linum 1))
    (with-open-file (stream file)
      (loop for (pos . lex) = (follow-word stream)
         while (not (eq (lookup pos)
                        :start))
         do
           (progn
             (format t "~a: " linum)
             (pprint-lexeme pos lex)
             (format t "~%")
             (case (lookup pos)
               (:error (format t (report-lex-error lex)))
               (:newline (incf linum))))))))

;; Report whether parsing was successful
(defun slurp-sentence (stream)
  (loop for (p . lex) = (follow-word stream)
     when (not (eq (lookup p)
                   :comment))
     collect (cons p lex)
     while (not (member (lookup p) '(:start :newline)))))

(defun next-sentence (stream linum)
  (let* ((result (slurp-sentence stream))
         (errors (any-errors (caar result) (cdar result) result))
         )
    (if errors
        (format t "On line ~a: ~a" linum errors)
        result))
)

;; (defun parse-file (file)
;;   (let ((success t))
;;     (with-open-file (stream file)
;;       (loop for linum fixnum from 1
;;             for line = (next-sentence stream linum)
;;             when (null line)
;;               do (setf success nil)
;;             while (or (null line)
;;                       (not (eq (lookup (caar line))
;;                                :start)))
;;             when (and line
;;                       (not (eq (lookup (caar line))
;;                                :newline)))
;;               collect (make-internal line)
;;             finally
;;                (when success
;;                  (format t "Successfully parsed file! ~a ILOC commands parsed.~%" linum))))))

(defun parse-file (file)
  (let ((success t)
        (count 0))
    (with-open-file (stream file)
      (loop for linum fixnum from 1
            for line = (slurp-sentence stream)
            while (not (eq (lookup (caar line))
                           :start))
            for errors = (any-errors (caar line) (cdar line) line)
            if errors
              do
                 (format t "On line ~a: ~a" linum errors)
                 (setf success nil)
            else if (not (eq (lookup (caar line))
                             :newline))
                   collect (make-internal line)
                   and do (incf count)
            finally
               (when success
                 (format t "Successfully parsed file! ~a ILOC commands parsed.~%" count))))))

;; Print the ir
(defun print-ir (file)
  (let ((success t))
    (with-open-file (stream file)
      (loop for linum fixnum from 1
            for line = (next-sentence stream linum)
            when (null line)
              do (setf success nil)
            while (or (null line)
                      (not (eq (lookup (caar line))
                               :start)))
            when (and line
                      (not (eq (lookup (caar line))
                               :newline)))
            do (pprint-ir (make-internal line))
            finally
               (when success
                 (format t "Successfully parsed file! ~a ILOC commands parsed.~%" linum))))))
