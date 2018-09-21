(require 'asdf)

(asdf:defsystem :412fe
  :serial t
  :build-operation "program-op"
  :build-pathname "../build/debug"
  :entry-point "412fe:entry"
  :depends-on ("alexandria")
  :components ((:file "./frontend/table")
               (:file "./frontend/table-init")
               (:file "./frontend/table-opt")
               (:file "./frontend/scanner")
               (:file "./frontend/errors")
               (:file "./frontend/ir")
               (:file "./frontend/ll")
               (:file "./frontend/parser")
               (:file "./backend/renamer")
               (:file "./cli/cli")
               (:file "main")))
