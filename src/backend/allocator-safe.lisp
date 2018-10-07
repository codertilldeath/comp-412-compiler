(defpackage :allocator
  (:use :cl :alexandria)
  (:import-from :renamer
                :rename-registers
                :*VR-name*)
  (:import-from :global
                :*max-live*
                :*max-register*)
  (:export :allocate-registers))


(in-package :allocator)

(defparameter *VR-to-PR* nil)
(defparameter *PR-to-VR* nil)
;; Available physical register stack
(defparameter *register-stack* nil)

(defun number-list (iter num)
  (if (= iter num)
      '()
      (cons iter (number-list (1+ iter) num))))

(defun associate (register pr)
  (setf (aref *VR-to-PR* (ir::virtual register)) pr)
  (setf (aref *PR-to-VR* pr) (ir::physical register)))

(defun disassociate (register pr)
  (setf (aref *VR-to-PR* (ir::virtual register)) -1)
  (setf (aref *PR-to-VR* pr) -1))

(defun get-pr (vr)
  (aref *VR-to-PR* vr))

(defun allocate-safe (register)
  (let ((v (ir::virtual register)))
    (unless (= -1 v)
      ;; First time using or defining VR
      (when (= -1 (aref *VR-to-PR* v))
        (associate register (pop *register-stack*)))
      ;; Set the register
      (setf (ir::physical register) (get-pr v)))))

(defun clear-last-use (register)
  (when (and (not (= -1
                     (ir::virtual register)))
             (= -1 (ir::next-use register)))
    (disassociate register (ir::physical register))
    (push (ir::physical register) *register-stack*)))

(defun allocate-full (ir registers)
  (setf *VR-to-PR* (make-array *VR-name* :element-type 'fixnum :initial-element -1)
        *PR-to-VR* (make-array registers :element-type 'fixnum :initial-element -1)
        *register-stack* (number-list 0 registers))
  (loop for i = (ll::head ir) then (ll::next i)
     while i
     for data = (ll::data i)
     do
       (progn
         (allocate-safe (ir::r1 data))
         (allocate-safe (ir::r2 data))
         (clear-last-use (ir::r2 data))
         (clear-last-use (ir::r1 data))
         (allocate-safe (ir::r3 data))))
  ir)

