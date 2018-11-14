
(progn
  (global:compile-start)
  (let* ((ir (parser:parse-file "../test/my/lab3/test_parallel_output.i")))
    (ir::output-parallel-ir ir #'ir::source)))

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

