(defpackage 412fe
  (:use cl)
  (:import-from :412fe.scanner
                :scan)
  (:export :entry))

(in-package :412fe)

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
          ((and (not (eq '|-h| option))
                (null filename))
           ;; Command line argument that required file, didn't receive file
           (format t "ERROR: No file passed.~%~%")
           (setq option (intern "-h"))))

    (when too-many
      (format t "Too many arguments passed, defaulting to ~a~%~%" option))

    (list option filename)))

(defun output-help ()
  (format t "This is the help message!~%"))

(defun main (argl)
  (destructuring-bind (option file) (parse-args argl)
    (case option
      (|-h| (output-help))
      (|-s| (format t "Print out all tokens!"))
      (|-p| (format t "Report if the program compiles!"))
      (|-r| (format t "Show internal representation!")))))

(defun entry ()
  (main sb-ext:*posix-argv*))
