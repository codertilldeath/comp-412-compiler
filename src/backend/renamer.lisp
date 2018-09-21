(defpackage :renamer
  (:use :cl :alexandria)
  (:export
   :rename-registers))

(in-package :renamer)

(defparameter *SR-to-VR* nil)
(defparameter *last-use* nil)
(defparameter *VR-name* nil)

(defun update (ir count)
  (let ((s (ir::r3-s ir)))
    (unless (= s -1)
      (when (= (aref *SR-to-VR* s) -1)
        (setf (aref *SR-to-VR* s) *VR-name*)
        (incf *VR-name*))
      (setf (ir::r3-v ir) (aref *SR-to-VR* s))
      (setf (ir::r3-nu ir) (aref *last-use* s))
      (setf (aref *last-use* s) count)
    (unless (string= (ir::opcode ir) "store")
      (kill ir))
    ))

  (let ((s (ir::r2-s ir)))
    (unless (= s -1)
      (when (= (aref *SR-to-VR* s) -1)
        (setf (aref *SR-to-VR* s) *VR-name*)
        (incf *VR-name*))
      (setf (ir::r2-v ir) (aref *SR-to-VR* s))
      (setf (ir::r2-nu ir) (aref *last-use* s))
      (setf (aref *last-use* s) count)))
  
  (let ((s (ir::r1-s ir)))
    (unless (= s -1)
      (when (= (aref *SR-to-VR* s) -1)
        (setf (aref *SR-to-VR* s) *VR-name*)
        (incf *VR-name*))
      (setf (ir::r1-v ir) (aref *SR-to-VR* s))
      (setf (ir::r1-nu ir) (aref *last-use* s))
      (setf (aref *last-use* s) count))))

(defun kill (ir)
  (let ((s (ir::r3-s ir)))
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
