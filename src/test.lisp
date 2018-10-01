
(progn
  (global:compile-start)
  (let* ((ir (parser:parse-file "../../../../../students/lab2/code_check_1/cc1.i")))
    (allocator:allocate-registers ir 5)
    (ir::output-ir ir #'ir::physical)))


(progn
  (global:compile-start)
  (let* ((ir (parser:parse-file "../../test/example2.i")))
    (renamer:rename-registers ir)
    (ir::output-ir-table ir )))


(progn
  (global:compile-start)
  (let* ((ir (parser:parse-file "../../test/example2.i")))
    (allocator:allocate-registers ir 3)
    ;;(renamer:rename-registers ir)
    (ir::output-ir ir #'ir::physical)))


(progn
  (global:compile-start)
  (let* ((ir (parser:parse-file "../test/my.i")))
    (allocator:allocate-registers ir 2)
    (ir::output-ir ir #'ir::physical)))

(progn
  (global:compile-start)
  (let* ((ir (parser:parse-file "../../../../../students/lab2/code_check_1/cc1.i")))
    (allocator:allocate-registers ir 5)
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

