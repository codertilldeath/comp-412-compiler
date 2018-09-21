(defpackage 412fe
  (:use cl)
  (:import-from :parser :parse-file)
  (:import-from :412fe.cli
                :output-help
                :parse-args)
  (:import-from :renamer
                :rename-registers)
  (:export :entry))

(in-package :412fe)

(defun main (argl)
  (destructuring-bind (f s) (parse-args argl)
    (case f
      (:|-x| (ir:output-virtual (rename-registers (parse-file s) 8))))))

(defun entry ()
  (main sb-ext:*posix-argv*))
