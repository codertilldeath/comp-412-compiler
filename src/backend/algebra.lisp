(in-package :scheduler)

(defun make-var-expr ()
  (make-hash-table))

(defun var-add (expr1 expr2)
  (let ((map (make-var-expr)))
    (maphash (lambda (key value)
               (add-element map key value))
             expr1)
    (maphash (lambda (key value)
               (add-element map key value))
             expr2)
    map))

(defun var-eq? (expr1 expr2)
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

(defun make-val-alg (val)
  (cons (make-var-expr) val))

(defun make-var-alg (var)
  (let ((map (make-var-expr)))
    (add-new-element map var)
    (cons map var)))

(defun add (alg1 alg2)
  (destructuring-bind (var1 . val1) alg1
    (destructuring-bind (var2 . val2) alg2
        (cons (var-add var1 var2)
              (+ val1 val2)))))

(defun alg-eq? (alg1 alg2)
  (destructuring-bind (var1 . val1) alg1
    (destructuring-bind (var2 . val2) alg2
        (and (var-eq? var1 var2)
             (= val1 val2)))))
