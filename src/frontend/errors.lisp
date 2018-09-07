(defpackage :412fe.parser.errors
  (:use :cl)
  (:import-from :412fe.table
                :lookup
                :get-example)
  (:export :any-errors
           :report-lex-error
           :report-eof-error
           :report-incorrect-word))

(in-package :412fe.parser.errors)

(defparameter *grammar-rules*
  '((:memop :register :into :register :newline)
    (:loadi :constant :into :register :newline)
    (:arithop :register :comma :register :into :register :newline)
    (:output :constant :newline)
    (:nop :newline)
    (:newline)))

(defun report-lex-error (s)
  (format nil "Unrecognized lexeme \"~a\"~%" s))

(defun report-eof-error (p)
  (format nil "Incomplete sentence. Missing ~a~%" p))

(defun report-incorrect-word (p1 p2)
  (format nil "Expected ~a, got ~a~%" p1 p2))

(defun grammar-matches (grammar sentence)
  (and
   (= (length grammar) (length sentence))
   (every (lambda (x y)
            (eq x (lookup (car y))))
          grammar
          sentence)))

(defun print-sentence (sentence)
  (loop for (p . lex) in sentence
     do (format t "~a " lex)))

(defun print-example (s)
  (format t "Example:~%~{~a~}~%" (mapcar #'get-example s)))

(defun lex-error (result)
  (let (word)
    (loop for (pos . lex) in result
       for part-speech = (lookup pos)
       while (null word)
       when (eq part-speech
                :error)
       do (setf word lex))
    (when word
      (report-lex-error word))))

(defun missing-grammar (pos lex)
  (let* ((part-speech (lookup pos)))
    (if (eq part-speech :error)
        (progn (report-lex-error lex)
               t)
        (when (null (assoc part-speech *grammar-rules*))
          (format nil "Start of line should start with an operator, got ~a~%" part-speech)))))

(defun mismatched-grammar (pos lex result)
  (let ((rules (assoc (lookup pos)
                      *grammar-rules*))
        grammar-expected
        grammar-got)
    (loop for part in rules
          for remaining = result then (cdr remaining)
          for (pos . lex) = (car remaining)
          while (null grammar-expected)
          do
             (when (and (not (eq (lookup pos) part))
                        (not (and (eq (lookup pos) :start)
                                  (eq part :newline))))
               (setf grammar-expected part)
               (setf grammar-got (lookup pos))))
    (when grammar-expected
      (report-incorrect-word grammar-expected grammar-got))))

(defun grammar-error (pos lex result)
  (or (missing-grammar pos lex)
      (mismatched-grammar pos lex result)))

(defun any-errors (pos lex result)
  (and result
       (not (eq (lookup pos) :start))
       (or (lex-error result)
           (grammar-error pos lex result))))
