
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

(defun string-2-instructions (data1 data2 f)
  (format t "[~a;~a]~%" (string-instruction data1 f) (string-instruction data2 f)))

(defun output-parallel-ir (ir f)
  (loop for node = (ll::head ir) then (let ((next (ll::next node)))
                                        (if (null next)
                                            nil
                                            (ll::next next)))
     while node
     for data = (ll::data node)
     for data2 = (ll::data (ll::next node))
     do
       (string-2-instructions data data2 #'virtual)))
