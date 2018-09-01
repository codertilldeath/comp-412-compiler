(defpackage :412fe.parser
  (:use :cl)
  (:import-from :412fe.table
                :lookup
                :get-example)
  (:import-from :412fe.scanner
                :follow-word)
  (:export :follow-file))

(in-package :412fe.parser)

(defparameter *grammar-rules*
  '((:memop :register :into :register :newline)
    (:loadi :constant :into :register :newline)
    (:arithop :register :comma :register :into :register :newline)
    (:output :constant :newline)
    (:nop :newline)
    (:newline)))

(defun pprint-lexeme (ch l)
  (if (string= l "
")
      (format t "{~a, \"\\n\"}" (lookup ch))
      (format t "{~a, ~S} " (lookup ch) l)))

(defun report-lex-error (s)
  (format t "~%Unrecognized lexeme: ~a~%~%" s))

(defun report-eof-error (p)
  (format t "~%Incomplete sentence. Missing ~a~%~%" p))

(defun report-incorrect-word (p1 p2)
  (format t "Expected ~a, got ~a~%~%" p1 p2))

(defun follow-file (file)
  (with-open-file (stream file)
    (loop for (pos . lex) = (follow-word stream)
       while (not (member (lookup pos)
                          '(:error :start)))
       finally
         (when (eq :error (lookup pos))
           (report-lex-error lex))
       do
         (progn
           (pprint-lexeme pos lex)
           (when (eq (lookup pos) :newline)
             (format t "~%"))))))

(defun follow-word-proper (stream expected)
  (destructuring-bind (p lex) (follow-word stream)
    (cond ((eq (lookup p) expected) (cons p lex))
          ((eq (lookup p) :error) (report-lex-error lex))
          (t (report-incorrect-word expected (lookup p))))))

(defun slurp-sentence (stream s lex)
  (cons lex
        (loop for part-speech in (cdr s)
           for (p . lex) = (follow-word stream)
           for pos = (lookup p)
           collect (cons p lex)
           do (cond ((eq pos :error)
                     (report-lex-error lex))
                    ((eq pos :start)
                     (report-eof-error pos))
                    ((not (eq pos part-speech))
                     (report-incorrect-word part-speech pos))))))

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

(defun next-sentence (stream s lex linum)
  (if (null s)
      (progn
        (report-lex-error (cdr lex))
        (format t "On line ~a" linum))
      (let ((result (slurp-sentence stream s lex)))
        (if (grammar-matches s result)
            (lookup (car (car result)))
            (progn
              (print-example s)
              (format t "Given on line ~a:~%" linum)
              (print-sentence result))))))

(defun parse-file (file)
  (with-open-file (stream file)
    (loop for (pos . lex) = (follow-word stream)
       for grammar-rules = (assoc (lookup pos) *grammar-rules*)
       for linum from 1
       for i = (next-sentence stream
                              grammar-rules
                              (cons pos lex)
                              linum)
       while (and i
                  (not (member (lookup pos)
                               '(:error :start)))))))
