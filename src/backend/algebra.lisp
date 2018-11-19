(in-package :scheduler)

(defstruct (algebraic-expr :conc-name
                           (:print-function
                            (lambda (struct stream z)
                              (declare (ignore z))
                              (format stream "Known? ~a, Vars: ~a, Const: ~a"
                                      (if (unknown struct)
                                          "No"
                                          "Yes")
                                      (hash-to-list (vars struct))
                                      (const struct)))))
  (vars (make-hash-table) :type hash-table)
  (const 0 :type fixnum)
  (unknown t :type boolean))

(defun is-const (alg)
  (zerop (hash-table-count (vars alg))))

(defun hash-to-list (hash)
  (loop for key being the hash-keys of hash
        collect (cons key (gethash key hash))))

(defun add-hash (expr1 expr2)
  (let ((map (make-hash-table)))
    (maphash (lambda (key value)
               (add-element map key value))
             expr1)
    (maphash (lambda (key value)
               (add-element map key value))
             expr2)
    map))

(defun mult-hash (expr1 val)
  (let ((map (make-hash-table)))
    (maphash (lambda (key value)
               (loop for i from 1 to val
                  do (add-element map key value)))
             expr1)
    map))

(defun sub-hash (expr1 expr2)
  (let ((map (make-hash-table)))
    (maphash (lambda (key value)
               (add-element map key value))
             expr1)
    (maphash (lambda (key value)
               (add-element map key (- value)))
             expr2)
    map))

(defun hash-eq? (expr1 expr2)
  (and (= (hash-table-count expr1) (hash-table-count expr2))
       (loop for key1 being the hash-keys of expr1
          always (eq (gethash key1 expr1)
                     (gethash key1 expr2)))))

(defun add-new-element (map key)
  (multiple-value-bind (ov succ) (gethash key map)
    (setf (gethash key map)
          (if succ
              (1+ ov)
              1))))

(defun add-element (map key val)
  (multiple-value-bind (ov succ) (gethash key map)
    (setf (gethash key map)
          (if succ
              (+ ov val)
              val))))

(defun make-variable (val)
  (let ((expr (make-algebraic-expr :unknown nil)))
    (setf (gethash val (vars expr))
          1)
    expr))

(defun make-value (var)
  (make-algebraic-expr :unknown nil
                       :const var))

(defun make-unknown ()
  (make-algebraic-expr))

(defun add (alg1 alg2)
  (make-algebraic-expr :unknown nil
                       :vars (add-hash (vars alg1)
                                       (vars alg2))
                       :const (+ (const alg1)
                                 (const alg2))))

(defun safe-mult (alg1 alg2)
  (make-algebraic-expr :unknown nil
                       :vars (mult-hash (vars alg2)
                                        (const alg1))
                       :const (* (const alg1)
                                 (const alg2))))

(defun mult (alg1 alg2 reg)
  (cond ((is-const alg1)
         (safe-mult alg1 alg2))
        ((is-const alg2)
         (safe-mult alg2 alg1))
        (t (make-variable reg))
        ))

(defun sub (alg1 alg2)
  (make-algebraic-expr :unknown nil
                       :vars (sub-hash (vars alg1)
                                       (vars alg2))
                       :const (- (const alg1)
                                 (const alg2))))

(defun alg-eq? (alg1 alg2)
  (and (not (unknown alg1))
       (not (unknown alg2))
       (= (const alg2) (const alg1))
       (hash-eq? (vars alg1) (vars alg2))))

(let* ((alg1 (make-value 3))
       (alg2 (make-variable 19))
       (alg3 (add alg1 alg2))
       (alg4 (add alg3 alg2))
       (alg5 (add alg4 alg3))
       (alg6 (sub alg5 alg4))
       (alg7 (mult alg6 alg1)))
  (print alg1)
  (print alg2)
  (print alg3)
  (print alg4)
  (print alg5)
  (print alg6)
  (print (alg-eq? alg6 alg3))
  (print alg7))
