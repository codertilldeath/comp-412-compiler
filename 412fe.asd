(asdf:defsystem :412fe
  :serial t
  :build-operation "program-op"
  :build-pathname "412fe"
  :entry-point "412fe:entry"
  :components ((:file "./parser/table")
               (:file "./parser/scanner")
               (:file "./cli/cli")
               (:file "main")))
