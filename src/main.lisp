(defpackage 412fe
  (:use cl)
  (:import-from :parser :parse-file)
  (:import-from :412fe.cli
                :output-help
                :parse-args)
  (:import-from :renamer
                :rename-registers)
  (:import-from :global
                :compile-start)
  (:export :entry))

(in-package :412fe)

(defun main (argl)
  (destructuring-bind (f . s) (parse-args argl)
    (case f
      (:|-x| (ir::output-ir (rename-registers (parse-file s))
                           #'ir::virtual)))))

(defun entry ()
  (main sb-ext:*posix-argv*))
