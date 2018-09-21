(defpackage 412fe
  (:use cl)
  (:import-from :parser
                :print-lexemes
                :parse-file
                :print-ir)
  (:import-from :412fe.cli
                :output-help
                :parse-args)
  (:import-from :renamer
                :rename-registers)
  (:export :entry))

(in-package :412fe)

(defun main (argl))


(rename-registers (parse-file "../../test/nope.txt"))

(defun entry ()
  (main sb-ext:*posix-argv*))
