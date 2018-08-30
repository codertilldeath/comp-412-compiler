(defpackage 412fe.cli
  (:use cl)
  (:export :parse-args
           :output-help))

(in-package :412fe.cli)

(defun cli-flag-p (f)
  (eql #\-
       (char f 0)))

(defun higher (new old)
  (let ((pl (list '|-h| '|-s| '|-p| '|-r|)))
    (< (position old pl)
       (position new pl))))

(defun parse-args (args)
  (let (filename option too-many)
    
    ;; Determine present flags
    (loop for arg in (cdr args) do
         (let ((arg-s (intern arg)))
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
           (setq option (intern "-h")))
          ((and (not (eql '|-h| option))
                (null filename))
           ;; Command line argument that required file, didn't receive file
           (format t "ERROR: No file passed.~%~%")
           (setq option (intern "-h"))))

    (when too-many
      (format t "Too many arguments passed, defaulting to ~a~%~%" option))

    (list option filename)))

(defun output-help ()
  (format t "This is the help message!~%"))
