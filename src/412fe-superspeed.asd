(require 'asdf)

(asdf:defsystem :412fe-superspeed
  :serial t
  :build-operation "program-op"
  :build-pathname "../build/412fealloc"
  :entry-point "412fe:entry"
  :depends-on ("alexandria")
  :around-compile (lambda (next)
                    (proclaim '(optimize (debug 0) 
                                         (safety 0)
                                         (speed 3)))
                    (funcall next))
  :components ((:file "./frontend/table")
               (:file "./frontend/table-init")
               (:file "./frontend/table-opt")
               (:file "./frontend/scanner")
               (:file "./frontend/errors")
               (:file "ll")
               (:file "ir")
               (:file "./frontend/parser")
               (:file "./backend/renamer")
               (:file "./cli/cli")
               (:file "main")))
