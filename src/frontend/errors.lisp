(defpackage :412fe.parser.errors
  (:use :cl)
  (:import-from :412fe.table
   :lookup
   :index-of
                :get-example)
  (:export :any-errors
           :report-lex-error
           :report-eof-error
           :report-incorrect-word))

(in-package :412fe.parser.errors)

(defparameter *grammar-debug*
  '((:memop :register :into :register :newline)
    (:loadi :constant :into :register :newline)
    (:arithop :register :comma :register :into :register :newline)
    (:output :constant :newline)
    (:nop :newline)
    (:newline)))

(defparameter *grammar-rules*
  (mapcar (lambda (x)
         (mapcar #'index-of x))
       *grammar-debug*))

(defun report-lex-error (s)
  (format nil "Unrecognized lexeme \"~a\"~%" s))

(defun report-eof-error (p)
  (format nil "Incomplete sentence. Missing ~a~%" p))

(defun report-incorrect-word (p1 p2)
  (format nil "Expected ~a, got ~a~%" p1 p2))

;; (defun grammar-matches (grammar sentence)
;;   (and
;;    (= (length grammar) (length sentence))
;;    (every (lambda (x y)
;;             (eq x (lookup (car y))))
;;           grammar
;;           sentence)))

(defun print-sentence (sentence)
  (loop for (p . lex) in sentence
     do (format t "~a " lex)))

(defun print-example (s)
  (format t "Example:~%~{~a~}~%" (mapcar #'get-example s)))

(defun lex-error (result)
  (let (word)
    (loop for (pos . lex) in result
       while (null word)
       when (= (the fixnum pos) 11)
       do (setf word lex))
    (when word
      (report-lex-error word))))

(defun missing-grammar (pos lex)
  (declare (fixnum pos))
  (if (= pos 11)
      (report-lex-error lex)
      (when (null (assoc pos *grammar-rules*))
        (format nil "Start of line should start with an operator, got ~a~%" (lookup pos)))))

(defun mismatched-grammar (result)
  (let ((rules (assoc (the fixnum (caar result))
                      *grammar-rules*))
        grammar-expected
        grammar-got)
    (loop for part in rules
          for remaining = result then (cdr remaining)
          for (pos . lex) = (car remaining)
          while (null grammar-expected)
          do
             (when (and (not (= (the fixnum pos) (the fixnum part)))
                        ;; This is for when the end of the file is in place of an endline
                        (not (and (= pos 13)
                                  (= part 9))))
               (setf grammar-expected part)
               (setf grammar-got pos)))
    (when grammar-expected
      (report-incorrect-word (lookup grammar-expected)
                             (lookup grammar-got)))))

(defun grammar-error (pos lex result)
  (or (missing-grammar pos lex)
      (mismatched-grammar result)))

(defun any-errors (pos lex result)
  (declare (fixnum pos))
  (and result
       (not (= pos 13))
       (or (lex-error result)
           (grammar-error pos lex result))))
