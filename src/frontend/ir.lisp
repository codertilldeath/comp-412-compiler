(defpackage :412fe.ir
  (:use :cl)
  (:import-from :412fe.table
                :lookup)
  (:export
   :make-internal :pprint-ir))

(in-package :412fe.ir)

(defstruct IR
  (opcode "" :type string)
  (constant -1 :type fixnum)
  (source -1 :type fixnum)
  (source-aux -1 :type fixnum)
  (dest -1 :type fixnum))

;; :memop :loadi :arithop :output :nop

(make-IR :opcode "loadI" :constant 10 :dest 2)

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
  (declare ((vector character *) r))
  (str->int (subseq r 1)))

(defun make-memop (i)
  (destructuring-bind ((_o . opcode) (_r1 . reg1) (_i . _into) (_r2 . reg2) . _rest) i
    (declare (ignore _o _r1 _i _r2 _into _rest))
    (make-IR :opcode opcode
             :source (register->int reg1)
             :dest (register->int reg2))))

(defun make-loadi (i)
  (destructuring-bind ((_o . opcode) (_r1 . constant) (_i . _into) (_r2 . reg2) . _rest) i
    (declare (ignore _o _r1 _i _r2 _into _rest))
    (make-IR :opcode opcode
             :constant (str->int constant)
             :dest (register->int reg2))))

(defun make-arithop (i)
  (destructuring-bind ((_o . opcode) (_r1 . r1) (_c . _comma) (_r2 . r2) (_i . _into) (_r3 . reg3) . _rest) i
    (declare (ignore _o _r1 _i _r2 _r3 _c _into _comma _rest))
    (make-IR :opcode opcode
             :source (register->int r1)
             :source-aux (register->int r2)
             :dest (register->int reg3))))

(defun make-output (i)
  (destructuring-bind ((_o . opcode) (_c . constant) . _rest) i
    (declare (ignore _o _c _rest))
    (make-IR :opcode opcode
             :constant (str->int constant))))

(defun make-internal (i)
  (case (lookup (caar i))
    (:memop (make-memop i))
    (:loadi (make-loadi i))
    (:arithop (make-arithop i))
    (:output (make-output i))
    (:nop (make-IR :opcode "nop"))))

(IR-opcode (make-internal '((0 . "loadI") (a . "r1") (b . "=>") (c . "r2") (d . ""))))

(defun pprint-IR (i)
  (format t "Opcode: ~a, Constant: ~a, Source: ~a, Source-aux: ~a, Destination: ~a~%"
          (IR-opcode i)
          (IR-constant i)
          (IR-source i)
          (IR-source-aux i)
          (IR-dest i)))
