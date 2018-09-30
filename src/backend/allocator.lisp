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
(defparameter *regs* nil)
(defparameter *VR-spilled?* nil)

(defun number-list (iter num)
  (if (= iter num)
      '()
      (cons iter (number-list (1+ iter) num))))

(defun associate (vr pr)
  (setf (aref *VR-to-PR* (ir::virtual vr)) pr)
  (setf (aref *PR-to-VR* pr) vr))

(defun disassociate (vr pr)
  (setf (aref *VR-to-PR* (ir::virtual vr)) -1)
  (setf (aref *PR-to-VR* pr) (ir::make-Register)))

(defun get-pr (vr)
  (aref *VR-to-PR* vr))

(defun allocate-safe (register)
  (let ((v (ir::virtual register)))
    (unless (= -1 v)
      ;; First time using or defining VR
      (when (= -1 (aref *VR-to-PR* v))
        (associate register (pop *regs*)))
      ;; Set the register
      (setf (ir::physical register) (get-pr v)))))

(defun clear-last-use (register)
  (when (and (not (= -1
                     (ir::virtual register)))
             (= -1 (ir::next-use register)))
    (disassociate register (ir::physical register))
    (push (ir::physical register) *regs*)))

(defun allocate-full (ir registers)
  (setf *VR-to-PR* (make-array *VR-name* :element-type 'fixnum :initial-element -1)
        *PR-to-VR* (make-array registers :element-type 'ir::register :initial-element (ir::make-Register))
        *regs* (number-list 0 registers))
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

;; (defun nu (i)
;;   (ir::make-Register :next-use i))

;; (defparameter *PR-to-VR* (make-array 3 :initial-contents (list (nu 10)
;;                                                                (nu 12)
;;                                                                (nu 2))))
;; (defparameter *regs* (number-list 0 3))

;; (print (get-a-register))

(defun spill-a-register ()
    (loop for i from 0 to (1- (car (array-dimensions *PR-to-VR*)))
       for max = (list i (aref *PR-to-VR* i))
       then
         (let ((new (aref *PR-to-VR* i)))
           (if (> (ir::next-use new)
                  (ir::next-use (cadr max)))
               (list i new)
               max))
       finally
         (return (car max))))


(defun generate-spill (register regs)
  (let ((ll (ll:make-LL)))
    (ll:insert-back ll (ir::make-IR :opcode "loadI"
                                 :category :loadI
                                 :constant (+ 32764 (* (ir::virtual register) 4))
                                 :r3 (ir::make-Register
                                      :physical (1- regs))))
    (ll:insert-back ll (ir::make-IR :opcode "store"
                                 :category :memop
                                 :r1 (ir::make-Register
                                      :physical (ir::physical register))
                                 :r3 (ir::make-Register
                                      :physical (1- regs))))
    ll))


(defun generate-restore (register regs dest)
  (let ((ll (ll:make-LL)))
    (ll:insert-back ll (ir::make-IR :opcode "loadI"
                                 :category :loadI
                                 :constant (+ 32764
                                              (* (ir::virtual register) 4))
                                 :r3 (ir::make-Register
                                      :physical (1- regs))))
    (ll:insert-back ll (ir::make-IR :opcode "load"
                                 :category :memop
                                 :r1 (ir::make-Register
                                      :physical (1- regs))
                                 :r3 (ir::make-Register
                                      :physical dest)))
    ll))

(defun allocate-unsafe (ll ir register rcount)
  (let ((v (ir::virtual register))
        spilled)
    (unless (= -1 v)
      ;; First time using or defining VR
      (when (= -1 (aref *VR-to-PR* v))
        (if-let (reg (pop *regs*))
          (associate register reg)
          (let* ((reg (spill-a-register))
                 (to-spill (aref *PR-to-VR* reg)))
            (disassociate to-spill reg)
            (ll:insert-before ll ir (generate-spill to-spill rcount))
            (associate register reg)
            (setf (aref *VR-spilled?* v) t)
            (setf spilled t)))
        (when (and (not spilled)
                   (aref *VR-spilled?* v))
          (ll:insert-before ll ir
                            (generate-restore register rcount (get-pr v)))
          (setf (aref *VR-spilled?* v) nil)))
      ;; Set the register
      (setf (ir::physical register) (get-pr v))
      ;; Update next-use, because the "when" above prevents uses from updating
      (setf (aref *PR-to-VR* (get-pr v)) register))))

(defun allocate-spill (ir registers)
  (setf *VR-to-PR* (make-array *VR-name* :element-type 'fixnum :initial-element -1)
        *PR-to-VR* (make-array registers :element-type 'ir::register :initial-element (ir::make-register))
        *VR-spilled?* (make-array *VR-name* :element-type 'boolean :initial-element nil)
        *regs* (number-list 0 (1- registers)))
  (loop for i = (ll::head ir) then (ll::next i)
     while i
     for data = (ll::data i)
     do
       (progn
         (allocate-unsafe ir i (ir::r1 data) registers)
         (allocate-unsafe ir i (ir::r2 data) registers)
         (clear-last-use (ir::r2 data))
         (clear-last-use (ir::r1 data))
         (allocate-unsafe ir i (ir::r3 data) registers)))
  ir)

(defun allocate-registers (ir registers)
  (rename-registers ir)
  (if (<= *max-live* registers)
      (allocate-full ir registers)
      (allocate-spill ir registers)))
