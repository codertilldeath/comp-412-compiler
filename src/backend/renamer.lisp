(defpackage :renamer
  (:use :cl :alexandria)
  (:export
   :rename-registers))

(in-package :renamer)

(defparameter *SR-to-VR* nil)
(defparameter *last-use* nil)
(defparameter *VR-name* nil)


(defun update-reg (register count)
  (let ((s (ir::source register)))
    (unless (= s -1)
      (when (= (aref *SR-to-VR* s) -1)
        (setf (aref *SR-to-VR* s) *VR-name*)
        (incf *VR-name*))
      (setf (ir::virtual register) (aref *SR-to-VR* s))
      (setf (ir::next-use register) (aref *last-use* s))
      (setf (aref *last-use* s) count))))

(defun update (ir count)
  (let ((s (ir::source (ir::r3 ir))))
    (unless (= s -1)
      (when (= (aref *SR-to-VR* s) -1)
        (setf (aref *SR-to-VR* s) *VR-name*)
        (incf *VR-name*))
      (setf (ir::virtual (ir::r3 ir)) (aref *SR-to-VR* s))
      (setf (ir::next-use (ir::r3 ir)) (aref *last-use* s))
      (setf (aref *last-use* s) count)
    (unless (string= (ir::opcode ir) "store")
      (kill ir))
    ))

  (update-reg (ir::r2 ir) count)
  (update-reg (ir::r1 ir) count))

(defun kill (ir)
  (let ((s (ir::source (ir::r3 ir))))
    (setf (aref *SR-to-VR* s) -1)
    (setf (aref *last-use* s) -1)))

(defun rename-registers (ll register-max)
  (setf *VR-name* 0
        *last-use* (make-array register-max :element-type 'fixnum :initial-element -1)
        *SR-to-VR* (make-array register-max :element-type 'fixnum :initial-element -1))
  (let ((current (1- (ll::size ll))))
    (loop for i = (ll::tail ll) then (ll::prev i)
       while i
       for data = (ll::data i)
       do
         (progn
           (update data current)
           (decf current))))
  ll)
