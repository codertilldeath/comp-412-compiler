(defpackage 412fe.cli
  (:use cl)
  (:export :parse-args
           :output-help))

(in-package :412fe.cli)

(defun cli-flag-p (f)
  (eql #\-
       (char f 0)))

(defun higher (new old)
  (let ((pl (list :|-s| :|-p| :|-r| :|-h|)))
    (< (position old pl)
       (position new pl))))

(defun parse-args (args)
  (let (filename option too-many)
    
    ;; Determine present flags
    (loop for arg in (cdr args) do
         (let ((arg-s (intern arg "KEYWORD")))
           (cond ((not (cli-flag-p arg))
                  (setq filename arg))
                 ((null option)
                  (setq option arg-s))
                 (t
                  (setq too-many t)
                  (when (higher arg-s option)
                    (setq option arg-s))))))

    (cond ((null option)
           ;; No option was given, show the help
           (format t "ERROR: No option specified~%~%")
           (setq option (intern "-h" "KEYWORD")))
          ((and (not (eql :|-h| option))
                (null filename))
           ;; Command line argument that required file, didn't receive file
           (format t "ERROR: No file passed.~%~%")
           (setq option (intern "-h"))))

    (when too-many
      (format t "Too many arguments passed, defaulting to ~a~%~%" option))

    (list option filename)))

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
