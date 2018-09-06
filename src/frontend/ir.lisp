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

(defun parse-register (r)
  (parse-integer (subseq r 1)))

(defun make-memop (i)
  (destructuring-bind ((_o . opcode) (_r1 . reg1) (_i . _into) (_r2 . reg2) . _rest) i
    (declare (ignore _o _r1 _i _r2 _into _rest))
    (make-IR :opcode opcode
             :source (parse-register reg1)
             :dest (parse-register reg2))))

(defun make-loadi (i)
  (destructuring-bind ((_o . opcode) (_r1 . constant) (_i . _into) (_r2 . reg2) . _rest) i
    (declare (ignore _o _r1 _i _r2 _into _rest))
    (make-IR :opcode opcode
             :constant (parse-integer constant)
             :dest (parse-register reg2))))

(defun make-arithop (i)
  (destructuring-bind ((_o . opcode) (_r1 . r1) (_c . _comma) (_r2 . r2) (_i . _into) (_r3 . reg3) . _rest) i
    (declare (ignore _o _r1 _i _r2 _r3 _c _into _comma _rest))
    (make-IR :opcode opcode
             :source (parse-register r1)
             :source-aux (parse-register r2)
             :dest (parse-register reg3))))

(defun make-output (i)
  (destructuring-bind ((_o . opcode) (_c . constant) . _rest) i
    (declare (ignore _o _c _rest))
    (make-IR :opcode opcode
             :constant (parse-integer constant))))

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
