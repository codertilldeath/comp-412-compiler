(defpackage :412fe.parser
  (:use :cl)
  (:import-from :412fe.table
                :lookup)
  (:import-from :412fe.scanner
                :follow-word)
  (:export :follow-file))

(in-package :412fe.parser)

(defparameter *grammar-rules*
  '((:memop :register :into :register :newline)
    (:loadi :constant :into :register :newline)
    (:arithop :reg :comma :reg :into :reg :newline)
    (:output :constant :newline)
    (:nop :newline)))

(defparameter *opcodes* nil)

(defun pprint-lexeme (ch l)
  (if (string= l "
")
      (format t "{~a, \"\\n\"}" (lookup ch))
      (format t "{~a, ~S} " (lookup ch) l)))

(defun report-lex-error (s)
  (format t "~%ERROR: Invalid lexeme: ~a~%" s))

(defun report-eof-error (p)
  (format t "~%Incomplete sentence. Missing ~a~%" p))

(defun report-incorrect-word (p1 p2)
  (format t "Expected ~a, got ~a~%" p1 p2))

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

(defun next-sentence (stream s lex)
  (cons (cons (car s) lex)
        (loop for part-speech in (cdr s)
           for (p . lex) = (follow-word stream)
           for pos = (lookup p)
           while (and (eq pos part-speech)
                      (not (member pos
                                   '(:error :start))))
           collect (progn
                     (pprint-lexeme p lex)
                     (cons p lex))
           finally (cond ((eq pos :error)
                          (report-lex-error lex))
                         ((eq pos :start)
                          (report-eof-error pos))
                         ((not (eq pos part-speech))
                          (report-incorrect-word part-speech pos))))))

(defun parse-file (file)
  (with-open-file (stream file)
    (loop for (pos . lex) = (follow-word stream)
       for grammar-rules = (assoc (lookup pos) *grammar-rules*)
       while (and grammar-rules
                  (not (member (lookup pos)
                               '(:error :start))))
       for i = (next-sentence stream
                              grammar-rules
                              lex)
       collect i)))
