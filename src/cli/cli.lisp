(defpackage 412fe.cli
  (:use cl)
  (:export :parse-args
           :output-help))

(in-package :412fe.cli)

(defun cli-flag-p (f)
  (eql #\-
       (char f 0)))

(defun parse-args (args)
  (if (cli-flag-p (cadr args))
      (cons (intern (cadr args) "KEYWORD")
            (caddr args))
      (cons (ir::str->int (cadr args))
            (caddr args))))

(defun output-help ()
  (format t "COMP 412, Fall 2018 Front End (lab 1)
Command Syntax:
	./412fe [flags] filename

Required arguments:
	filename  is the pathname (absolute or relative) to the input file

Optional flags:
	-h	 prints this message
	-s	 prints tokens in token stream
	-p	 invokes parser and reports on success or failure
	-r	 prints human readable version of parser's IR
"))
