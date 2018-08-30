(defpackage 412fe
  (:use cl)
  (:import-from :412fe.scanner
                :scan)
  (:import-from :412fe.cli
                :output-help
                :parse-args)
  (:export :entry))

(in-package :412fe)

(defun main (argl)
  (destructuring-bind (option file) (parse-args argl)
    (case option
      (|-h| (output-help))
      (|-s| (format t "Print out all tokens!"))
      (|-p| (format t "Report if the program compiles!"))
      (|-r| (format t "Show internal representation!")))))

(defun entry ()
  (main sb-ext:*posix-argv*))
