(in-package :allocator)

(defparameter *VR-spilled?* nil)
(defparameter *PR-next-use* nil)
(defparameter *remat?* nil)
(defparameter *VR-definst* nil)
(defparameter *VR-clean?* nil)

(defun set-definst (data)
  (let* ((v (ir::virtual (ir::r3 (ll::data data)))))
    (unless (= v -1)
      (setf (aref *VR-definst* v)
            data))))

(defun associate-unsafe (register pr)
  (setf (aref *VR-to-PR* (ir::virtual register)) pr)
  (setf (aref *PR-to-VR* pr) (ir::virtual register)))

(defun disassociate-unsafe (register pr)
  (setf (aref *VR-to-PR* register) -1)
  (setf (aref *PR-to-VR* pr) -1))

(defun generate-spill (vr pr regs)
  (let ((ll (ll:make-LL)))
    (ll:insert-back ll (ir::make-IR :opcode :|loadI|
                                 :category :loadI
                                 :constant (+ 32764 (* vr 4))
                                 :r3 (ir::make-Register
                                      :physical (1- regs))))
    (ll:insert-back ll (ir::make-IR :opcode :|store|
                                 :category :memop
                                 :r1 (ir::make-Register
                                      :virtual vr
                                      :physical pr)
                                 :r2 (ir::make-Register
                                      :physical (1- regs))
                                 :store t))
    ll))


(defun generate-restore (vr spill-register dest)
  (let ((ll (ll:make-LL)))
    (ll:insert-back ll (ir::make-IR :opcode :|loadI|
                                 :category :loadI
                                 :constant (+ 32764
                                              (* vr 4))
                                 :r3 (ir::make-Register
                                      :physical spill-register)))
    (ll:insert-back ll (ir::make-IR :opcode :|load|
                                 :category :memop
                                 :r1 (ir::make-Register
                                      :physical spill-register)
                                 :r3 (ir::make-Register
                                      :physical dest)))
    ll))

;; (defun safety-check (linum)
;;   (loop for i from 0 to (1- (car (array-dimensions *VR-to-PR*)))
;;      do
;;        (if (and (not (= -1 (aref *VR-to-PR* i)))
;;                 (not (= (ir::virtual (aref *PR-to-VR* (aref *VR-to-PR* i)))
;;                         i)))
;;            (format t "We have a problem! Line number ~a! vr~a does not match pr~a!~%" linum i (ir::virtual (aref *PR-to-VR* (aref *VR-to-PR* i))))))
;;   (loop for i from 0 to (1- (car (array-dimensions *PR-to-VR*)))
;;      do
;;        (if (and (not (= -1 (ir::virtual (aref *PR-to-VR* i))))
;;                 (not (= (aref *VR-to-PR* (ir::virtual (aref *PR-to-VR* i)))
;;                         i)))
;;            (format t "We have a problem! Line number ~a! pr~a does not match vr~a!~%" linum i (aref *VR-to-PR* (ir::virtual (aref *PR-to-VR* i)))))))

(defun choose-spill-register (dont-use)
  (let (max-next-use max-register
        max-nu-remat max-remat)
    (loop for i from 0 to (- (car (array-dimensions *PR-to-VR*)) 2)
       do
         (unless (= dont-use i)
           (if (eq (ir::opcode (ll::data (aref *VR-definst* (aref *PR-to-VR* i))))
                   :|loadI|)
               ;; Rematerializeable
               (if (null max-nu-remat)
                   (progn
                     (setf max-nu-remat (aref *PR-next-use* i))
                     (setf max-remat i))
                   (when (> (aref *PR-next-use* i)
                            max-nu-remat)
                     (setf max-nu-remat (aref *PR-next-use* i))
                     (setf max-remat i)))
               (if (null max-next-use)
                   (progn
                     (setf max-next-use (aref *PR-next-use* i))
                     (setf max-register i))
                   (when (> (aref *PR-next-use* i)
                            max-next-use)
                     (setf max-next-use (aref *PR-next-use* i))
                     (setf max-register i))))))
    (cond ((null max-remat) max-register)
          ((null max-register) max-remat)
          ((< max-next-use max-nu-remat) max-register)
          (t max-remat))))

(defun get-register-or-spill (ll ir rcount dont-use linum)
  (if-let (reg (pop *register-stack*))
    reg
    (let* ((pr (choose-spill-register dont-use))
           (vr (aref *PR-to-VR* pr))
           (inst (ll::data (aref *VR-definst* vr))))
      (if (eq (ir::opcode inst) :|loadI|)
          ;; Rematerializeable
          (progn
            ;; Remove the loadI instruction
            (when (< linum (ir::next-use (ir::r3 inst)))
              (ll:del ll (aref *VR-definst* vr)))
            )
          ;; Regular spill
          (progn
            (when (= (aref *VR-clean?* vr) -1)
              ;; If value has been spilled before, don't spill again
              (ll:insert-before ll ir (generate-spill vr pr rcount)))
            (setf (aref *VR-clean?* vr) (+ 32764 (* vr 4)))))
      (setf (aref *VR-spilled?* vr) t)
      (disassociate-unsafe vr pr)
      pr)))

(defun allocate-unsafe (ll ir register rcount dont-use linum)
  (let ((v (ir::virtual register)))
    (unless (= -1 v)
      ;; First time using or defining VR
      (when (= -1 (aref *VR-to-PR* v))
        ;; def
        (let ((reg (get-register-or-spill ll ir rcount dont-use linum)))
          (associate-unsafe register reg))
        ;; restore
        (when (aref *VR-spilled?* v)
          (if (eq (ir::opcode (ll::data (aref *VR-definst* v)))
                  :|loadI|)
              (let ((inst (ll::data (aref *VR-definst* v))))
                (ll:insert-before-raw ll ir
                                      (ir::make-IR :opcode :|loadI|
                                                   :category :loadI
                                                   :constant (ir::constant inst)
                                                   :r3 (ir::make-Register
                                                        :physical (get-pr v)
                                                        :virtual (ir::virtual (ir::r3 inst))))))
              (progn 
                (ll:insert-before ll ir
                                  (generate-restore (ir::virtual register)
                                                    (1- rcount)
                                                    (get-pr v)))
                (setf (aref *VR-spilled?* v) nil)))))
      ;; Update physical-register in IR
      (setf (ir::physical register) (get-pr v))
      ;; Update next-use, because the "when" above prevents uses from updating
      (setf (aref *PR-next-use* (get-pr v)) (ir::next-use register)))))

(defun allocate-spill (ir registers)
  (setf *VR-to-PR* (make-array *VR-name* :element-type 'fixnum :initial-element -1)
        *PR-to-VR* (make-array registers :element-type 'fixnum :initial-element -1)
        *PR-next-use* (make-array registers :element-type 'fixnum :initial-element -1)
        *VR-spilled?* (make-array *VR-name* :element-type 'boolean :initial-element nil)
        *VR-clean?* (make-array *VR-name* :element-type 'fixnum :initial-element -1)
        *VR-definst* (make-array *VR-name* :element-type 'll::ll-node :initial-element (ll::make-ll-node))
        *register-stack* (number-list 0 (1- registers)))
  (loop for i = (ll::head ir) then (ll::next i)
     for iter from 0
     while i
     for data = (ll::data i)
     do
       (allocate-unsafe ir i (ir::r1 data) registers -1 iter)
       (allocate-unsafe ir i (ir::r2 data) registers (ir::physical (ir::r1 data)) iter)
       (clear-last-use (ir::r2 data))
       (clear-last-use (ir::r1 data))
       (allocate-unsafe ir i (ir::r3 data) registers -1 iter)
       (set-definst i))
  ir)

(defun allocate-registers (ir registers)
  (rename-registers ir)
  (if (<= *max-live* registers)
      (allocate-full ir registers)
      (allocate-spill ir registers)))
