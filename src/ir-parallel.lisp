
(in-package :ir)

(defun string-instruction (data f)
  (case (category data)
    (:memop   (format nil "~a r~a => r~a" (opcode data) (funcall f (r1 data)) (funcall f (if (store data)
                                                                                            (r2 data)
                                                                                            (r3 data)))))
    (:loadi   (format nil "~a ~a => r~a" (opcode data) (constant data) (funcall f (r3 data))))
    (:arithop (format nil "~a r~a, r~a => r~a" (opcode data) (funcall f (r1 data)) (funcall f (r2 data)) (funcall f (r3 data))))
    (:output  (format nil "output ~a" (constant data)))
    (:nop     (format nil "nop"))))

(defun output-parallel-ir (ir f)
  (loop for node = (ll::head ir) then (ll::next (ll::next node))
     while node
       for data = (ll::data node)
     do
       (format t "[~a;~a]" (string-instruction data f) (string-instruction data f))))
