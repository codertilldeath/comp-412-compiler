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
  (:import-from :ir
                :output-ir)
  (:import-from :allocator
                :allocate-registers)
  (:export :entry))

(in-package :412fe)

(defun main (argl)
  (compile-start)
  (destructuring-bind (f . s) (parse-args argl)
    (case f
      (:|-x| (output-ir (rename-registers (parse-file s))
                            #'ir::virtual))
      (t (output-ir (allocate-registers (parse-file s) f)
                    #'ir::physical)))))

(defun entry ()
  (main sb-ext:*posix-argv*))
