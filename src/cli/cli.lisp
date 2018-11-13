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
  (format t "Syntax: 412sched filename [-h] [-g] [-s] [-v]
	filename is the pathname (absolute or relative) to the input file
	-h prints this message

The following flags are additive (multiple flags produce stronger results):
	-g : creates a '.dot' file for graphviz
"
      ;;-s : disables dependence trimming for LOAD, STORE, & OUTPUT
      ;;-t : turns on internal timing reports
      ;;-v : verifies dependence graph structure before use.
          ))
