(defpackage :renamer
  (:use :cl :alexandria)
  (:import-from :global
                :*max-register*)
  (:export
   :rename-registers))

(in-package :renamer)

(defparameter *SR-to-VR* nil)
(defparameter *last-use* nil)
(defparameter *VR-name* nil)

(defun update (register count)
  (let ((s (ir::source register)))
    (unless (= s -1)
      (when (= (aref *SR-to-VR* s) -1)
        (setf (aref *SR-to-VR* s) *VR-name*)
        (incf *VR-name*))
      (setf (ir::virtual register) (aref *SR-to-VR* s))
      (setf (ir::next-use register) (aref *last-use* s))
      (setf (aref *last-use* s) count))))

(defun kill (ir)
  (let ((s (ir::source ir)))
    (unless (= s -1)
      (setf (aref *SR-to-VR* s) -1)
      (setf (aref *last-use* s) -1))))

(defun rename-registers (ll)
  (setf *VR-name* 0
        *last-use* (make-array *max-register* :element-type 'fixnum :initial-element -1)
        *SR-to-VR* (make-array *max-register* :element-type 'fixnum :initial-element -1))
  (let ((current (1- (ll::size ll))))
    (loop for i = (ll::tail ll) then (ll::prev i)
       while i
       for data = (ll::data i)
       do
         (progn
           (update (ir::r3 data) current)
           (unless (string= (ir::opcode data) "store")
             (kill (ir::r3 data)))
           (update (ir::r2 data) current)
           (update (ir::r1 data) current)
           (decf current))))
  ll)
