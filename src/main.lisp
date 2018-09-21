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


(print (ll::to-list (rename-registers (parser:parse-file "../test/nope.txt") 10)))

(defun entry ()
  (main sb-ext:*posix-argv*))
