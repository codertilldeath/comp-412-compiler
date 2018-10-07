(defpackage :renamer
  (:use :cl :alexandria)
  (:import-from :global
                :*max-register*
                :*max-live*
                :*current-live*)
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
        ;; This is a def
        (setf (aref *SR-to-VR* s) *VR-name*)
        (incf *VR-name*)
        (incf *current-live*))
      ;; Use code
      (setf (ir::virtual register) (aref *SR-to-VR* s))
      (setf (ir::next-use register) (aref *last-use* s))
      ;; This line causes the same-line-next-use bug
      ;; The previous register has already updated the last-use table
      (setf (aref *last-use* s) count))))

(defun kill (ir)
  (let ((s (ir::source ir)))
    (unless (= s -1)
      (decf *current-live*)
      (setf (aref *SR-to-VR* s) -1)
      (setf (aref *last-use* s) -1))))

(defun update-line (data current)
  (update (ir::r3 data) current)
  (kill (ir::r3 data))
  (update (ir::r2 data) current)
  (update (ir::r1 data) current)
  (when (> *current-live* *max-live*)
    (setq *max-live* *current-live*)))

(defun rename-registers (ll)
  (setf *VR-name* 0
        *last-use* (make-array *max-register* :element-type 'fixnum :initial-element -1)
        *SR-to-VR* (make-array *max-register* :element-type 'fixnum :initial-element -1))
  (let ((current (1- (ll::size ll))))
    (loop for i = (ll::tail ll) then (ll::prev i)
       for current = (1- (ll::size ll)) then (1- current)
       while i
       for data = (ll::data i)
       do
         (update-line data current)))
  ll)
