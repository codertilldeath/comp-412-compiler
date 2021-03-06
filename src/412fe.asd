(require 'asdf)

(asdf:defsystem :412fe
  :serial t
  :build-operation "program-op"
  :build-pathname "../build/debug"
  :entry-point "412fe:entry"
  :depends-on ("alexandria")
  :components ((:file "global")
               (:file "ll")
               
               (:file "./frontend/table")
               (:file "ir")
               (:file "ir-parallel")
               (:file "./frontend/table-init")
               (:file "./frontend/table-opt")
               (:file "./frontend/scanner")
               (:file "./frontend/errors")
               (:file "./frontend/parser")
               
               (:file "./backend/renamer")
               ;;(:file "./backend/allocator-safe")
               ;;(:file "./backend/allocator-unsafe")
               (:file "./backend/scheduler")
               (:file "./backend/graph")
               (:file "./backend/algebra")
               
               (:file "./cli/cli")
               (:file "main")))
