(require 'asdf)

(asdf:defsystem :412fe-superspeed
  :serial t
  :build-operation "program-op"
  :build-pathname "../scheduler"
  :entry-point "412fe:entry"
  :depends-on ("alexandria")
  :around-compile (lambda (next)
                    (proclaim '(optimize (debug 0) 
                                         (safety 0)
                                         (speed 3)))
                    (funcall next))
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
