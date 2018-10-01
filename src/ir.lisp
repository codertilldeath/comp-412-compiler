(defpackage :ir
  (:use :cl)
  (:import-from :scanner-table
                :lookup)
  (:import-from :global
               :*max-register*)
  (:export
   :make-internal :pprint-ir :output-ir))

(in-package :ir)

(defstruct (Register :conc-name)
  (source -1 :type fixnum)
  (virtual -1 :type fixnum)
  (physical -1 :type fixnum)
  (next-use -1 :type fixnum))

(defstruct (IR :conc-name)
  (opcode "" :type string)
  (category nil :type symbol)
  (constant -1 :type fixnum)
  (r1 (make-Register) :type Register)
  (r2 (make-Register) :type Register)
  (r3 (make-Register) :type Register))

;; :memop :loadi :arithop :output :nop

;; (make-IR :opcode "loadI" :constant 10 :dest 2)

(defun chr->int (c)
  (- (char-code c) 48))

(defun str->int (s)
  (let ((acc 0))
    (declare (fixnum acc))
    (with-input-from-string (stream s)
      (loop for c = (read-char stream nil)
            while c
            for n = (chr->int c)
            do (setf acc (+ (the fixnum (* acc 10)) (the fixnum n)))))
    acc))

(defun register->int (r)
  (declare ((vector character *) r)
           (fixnum *max-register*))
  (let ((num (the fixnum (str->int (subseq r 1)))))
    (when (> (1+ num) *max-register*)
      (setq *max-register* (1+ num)))
    (make-Register :source num)))

(defun make-memop (i)
  (destructuring-bind ((_o . opcode) (_r1 . reg1) (_i . _into) (_r2 . reg2) . _rest) i
    (declare (ignore _o _r1 _i _r2 _into _rest))
    (make-IR :opcode opcode
             :category :memop
             :r1 (register->int reg1)
             :r3 (register->int reg2))))

(defun make-loadi (i)
  (destructuring-bind ((_o . opcode) (_r1 . constant) (_i . _into) (_r2 . reg2) . _rest) i
    (declare (ignore _o _r1 _i _r2 _into _rest))
    (make-IR :opcode opcode
             :category :loadi
             :constant (str->int constant)
             :r3 (register->int reg2))))

(defun make-arithop (i)
  (destructuring-bind ((_o . opcode) (_r1 . r1) (_c . _comma) (_r2 . r2) (_i . _into) (_r3 . reg3) . _rest) i
    (declare (ignore _o _r1 _i _r2 _r3 _c _into _comma _rest))
    (make-IR :opcode opcode
             :category :arithop
             :r1 (register->int r1)
             :r2 (register->int r2)
             :r3 (register->int reg3))))

(defun make-output (i)
  (destructuring-bind ((_o . opcode) (_c . constant) . _rest) i
    (declare (ignore _o _c _rest))
    (make-IR :opcode opcode
             :category :output
             :constant (str->int constant))))

(defun make-internal (i)
  (case (lookup (caar i))
    (:memop (make-memop i))
    (:loadi (make-loadi i))
    (:arithop (make-arithop i))
    (:output (make-output i))
    (:nop (make-IR :opcode "nop"
                   :category :nop))))

;; (pprint-IR (make-internal '((0 . "loadI") (a . "r1") (b . "=>") (c . "r2") (d . ""))))

(defun pprint-IR (i)
  (format t "Opcode: ~a, Constant: ~a, Source: ~a, Source-aux: ~a, Destination: ~a~%"
          (opcode i)
          (constant i)
          (r1 i)
          (r2 i)
          (r3 i)))

(defun output-instruction (data f)
  (declare (function f))
  (case (category data)
         (:memop (format t "~a r~a => r~a~%" (opcode data) (funcall f (r1 data)) (funcall f (r3 data))))
         (:loadi (format t "~a ~a => r~a~%" (opcode data) (constant data) (funcall f (r3 data))))
         (:arithop (format t "~a r~a, r~a => r~a~%" (opcode data) (funcall f (r1 data)) (funcall f (r2 data)) (funcall f (r3 data))))
         (:output (format t "output ~a~%" (constant data)))
         (:nop (format t "nop~%"))))

(defun output-ir (ir f)
  (loop for node = (ll::head ir) then (ll::next node)
     while node
     for data = (ll::data node)
     do
       (output-instruction data f)))

(defun z (n)
  (declare (fixnum n))
  (if (< n 0)
      "-"
      n))

(defun output-ir-table (ir)
  (format t "|opcode|VR|PR|NU||VR|PR|NU||VR|PR|NU|~%")
  (format t "|-~%")
  (loop for node = (ll::head ir) then (ll::next node)
     while node
     for data = (ll::data node)
     do
       (let ((r1 (r1 data))
             (r2 (r2 data))
             (r3 (r3 data)))
         (format t "|~a|~a|~a|~a||~a|~a|~a||~a|~a|~a|~%"
                 (opcode data)
                 (z (virtual r1)) (z (physical r1)) (z (next-use r1))
                 (z (virtual r2)) (z (physical r2)) (z (next-use r2))
                 (z (virtual r3)) (z (physical r3)) (z (next-use r3))))))
