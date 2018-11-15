
(progn
  (global:compile-start)
  (let* ((ir (parser:parse-file "../test/lab2_cc1/cc1.i"))
         (ir2 (scheduler::schedule ir)))
    (renamer:rename-registers ir)
    (scheduler::make-graph ir)

    (ir::output-parallel-ir ir2 #'ir::virtual)
    ;;(format t "~a" scheduler::*node-table*)
    ;;(scheduler::output-graph)
    ;;(format t "~a" scheduler::*node-table*)
    ;;(ir::output-parallel-ir (scheduler:schedule ir) #'ir::virtual)
    ))

(require 'sb-sprof)

(let ((ir (parser:parse-file "../../../../students/lab2/timing/T128k.i")))
  (time (allocator:allocate-registers ir 5))
  (sb-sprof:with-profiling (:report :graph)
    (allocator:allocate-registers ir 5))
  nil)

(sb-sprof:with-profiling (:report :flat)
  (let ((ir (parser:parse-file "../../../../students/lab2/timing/T128k.i")))))

global:*max-live*
global:*max-register*


(declaim (inline popper))
(defun popper (x)
  (pop x))

(defun aoeu ()
  (let ((stack '(1 2 3)))
    (popper stack)
    (print stack)))

(aoeu)

