(in-package :allocator)

(defparameter *VR-spilled?* nil)
(defparameter *remat?* nil)

(defun associate-unsafe (register pr)
  (setf (aref *VR-to-PR* (ir::virtual register)) pr)
  (setf (aref *PR-to-VR* pr) register))

(defun disassociate-unsafe (register pr)
  (setf (aref *VR-to-PR* (ir::virtual register)) -1)
  (setf (aref *PR-to-VR* pr) (ir::make-register)))

(defun choose-spill-register ()
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

(defun safety-check (linum)
  (loop for i from 0 to (1- (car (array-dimensions *VR-to-PR*)))
     do
       (if (and (not (= -1 (aref *VR-to-PR* i)))
                (not (= (ir::virtual (aref *PR-to-VR* (aref *VR-to-PR* i)))
                        i)))
           (format t "We have a problem! Line number ~a! vr~a does not match pr~a!~%" linum i (ir::virtual (aref *PR-to-VR* (aref *VR-to-PR* i))))))
  (loop for i from 0 to (1- (car (array-dimensions *PR-to-VR*)))
     do
       (if (and (not (= -1 (ir::virtual (aref *PR-to-VR* i))))
                (not (= (aref *VR-to-PR* (ir::virtual (aref *PR-to-VR* i)))
                        i)))
           (format t "We have a problem! Line number ~a! pr~a does not match vr~a!~%" linum i (aref *VR-to-PR* (ir::virtual (aref *PR-to-VR* i)))))))

(defun get-register-or-spill (ll ir rcount)
  (if-let (reg (pop *register-stack*))
    reg
    (let* ((reg (choose-spill-register))
           (to-spill (aref *PR-to-VR* reg)))
      (disassociate-unsafe to-spill reg)
      (if (aref *remat?* (ir::physical to-spill))
          ;; Rematerializeable
          (progn
            ;; Remove the loadI instruction
            )
          ;; Regular spill
          (progn
            (ll:insert-before ll ir (generate-spill to-spill rcount))
            (setf (aref *VR-spilled?* (ir::virtual to-spill)) t)))
      reg)))

(defun allocate-unsafe (ll ir register rcount)
  (let ((v (ir::virtual register)))
    (unless (= -1 v)
      ;; First time using or defining VR
      (when (= -1 (aref *VR-to-PR* v))
        ;; def
        (let ((reg (get-register-or-spill ll ir rcount)))
          (associate-unsafe register reg))
        ;; restore
        (when (aref *VR-spilled?* v)
          (ll:insert-before ll ir
                            (generate-restore register rcount (get-pr v)))
          (setf (aref *VR-spilled?* v) nil)))
      ;; Update physical-register in IR
      (setf (ir::physical register) (get-pr v))
      ;; Update next-use, because the "when" above prevents uses from updating
      (setf (aref *PR-to-VR* (get-pr v)) register))))

(defun allocate-spill (ir registers)
  (setf *VR-to-PR* (make-array *VR-name* :element-type 'fixnum :initial-element -1)
        *PR-to-VR* (make-array registers :element-type 'ir::register :initial-element (ir::make-register))
        *VR-spilled?* (make-array *VR-name* :element-type 'boolean :initial-element nil)
        *remat?* (make-array *VR-name* :element-type 'boolean :initial-element nil)
        *register-stack* (number-list 0 (1- registers)))
  (loop for i = (ll::head ir) then (ll::next i)
     for iter from 0
     while i
     for data = (ll::data i)
     do
       (if (and (eq :memop (ir::category data))
                (ir::store data))
           (progn
             (allocate-unsafe ir i (ir::r1 data) registers)
             (allocate-unsafe ir i (ir::r3 data) registers)
             (clear-last-use (ir::r3 data))
             (clear-last-use (ir::r1 data)))
           (progn
             (allocate-unsafe ir i (ir::r1 data) registers)
             (allocate-unsafe ir i (ir::r2 data) registers)
             (clear-last-use (ir::r2 data))
             (clear-last-use (ir::r1 data))
             (allocate-unsafe ir i (ir::r3 data) registers))))
  ir)

(defun allocate-registers (ir registers)
  (rename-registers ir)
  (if (<= *max-live* registers)
      (allocate-full ir registers)
      (allocate-spill ir registers)))
