

(progn
  (global:compile-start)
  (let* ((ir (parser:parse-file "../test/my/lab3/test_stores.i")))
    (renamer:rename-registers ir)
    (scheduler::make-graph ir)
    ;; (format t "~a" (ir::string-instruction (scheduler::node-inst (aref scheduler::*node-table* 17))
    ;;                                        #'ir::virtual))
    ;; (print (scheduler::node-dep-left (aref scheduler::*node-table* 17)))
    ;;(ir::output-parallel-ir (scheduler::schedule ir) #'ir::virtual)
    ;; (scheduler::schedule ir)
    ;; nil
    ;;(format t "~a" scheduler::*node-table*)
    (scheduler::output-graph "../test/my/lab3/test_stores")
    ;;(scheduler::remove-edge 2 1)
    ;;(scheduler::output-graph nil)
    ;;(format t "~a" scheduler::*node-table*)
    ;;(ir::output-parallel-ir (scheduler:schedule ir) #'ir::virtual)
    ))

(progn
  (global:compile-start)
  (let* ((ir (parser:parse-file "../../../../students/lab2/timing/T128k.i")))
    (renamer:rename-registers ir)
    (ir::output-parallel-ir (scheduler::schedule ir) #'ir::virtual)
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

