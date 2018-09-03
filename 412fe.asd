(require 'asdf)

(asdf:defsystem :412fe
  :serial t
  :build-operation "program-op"
  :entry-point "412fe:entry"
  :components ((:file "./frontend/table")
               (:file "./frontend/scanner")
               (:file "./frontend/errors")
               (:file "./frontend/ir")
               (:file "./frontend/parser")
               (:file "./cli/cli")
               (:file "main")))
