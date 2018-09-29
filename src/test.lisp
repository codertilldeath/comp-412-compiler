
(progn
  (global:compile-start)
  (let* ((ir (parser:parse-file "../../../../students/lab2/code_check_1/cc5.i")))
    (renamer:rename-registers ir)
    (ir::output-ir ir #'ir::virtual)))


(progn
  (global:compile-start)
  (let* ((ir (parser:parse-file "../test/example.i")))
    (renamer:rename-registers ir)
    (ir::output-ir ir #'ir::virtual)))


(progn
  (global:compile-start)
  (let* ((ir (parser:parse-file "../test/example.i")))
    (allocator:allocate-registers ir 4)
    (ir::output-ir ir #'ir::physical)))

(progn
  (global:compile-start)
  (let* ((ir (parser:parse-file "../../../../students/lab2/code_check_1/cc1.i")))
    (allocator:allocate-registers ir 7)
    (ir::output-ir ir #'ir::physical)))

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

