(defpackage 412fe
  (:use cl)
  (:import-from :412fe.parser
                :print-lexemes
                :parse-file
                :print-ir)
  (:import-from :412fe.cli
                :output-help
                :parse-args)
  (:export :entry))

(in-package :412fe)

(defun main (argl)
  (destructuring-bind (option file) (parse-args argl)
    (case option
      (:|-h| (output-help))
      (:|-s| (print-lexemes file))
      (:|-p| (parse-file file))
      (:|-r| (print-ir file)))))

(defun entry ()
  (main sb-ext:*posix-argv*))
