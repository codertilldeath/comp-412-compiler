(defpackage :ll
  (:use :cl :alexandria)
  (:export :make-LL
           :to-list
           :insert-back
           :insert-before
           :insert-after))

(in-package :ll)

(defstruct (LL :conc-name
               (:print-function
                (lambda (struct stream z)
                  (declare (ignore stream z))
                  (print (to-list struct)))))
  head tail (size 0 :type fixnum))

(defstruct (ll-node :conc-name
                    (:print-function
                     (lambda (struct stream z)
                       (declare (ignore stream z))
                       (print (data struct)))))
  data prev next)

(defun to-list (ll)
  (loop for i = (head ll) then (next i)
     while i
     collect (data i)))

(defun link-nodes (n1 n2)
  (setf (next n1) n2)
  (setf (prev n2) n1))

(defun insert-back (LL data)
  (let ((new-node (make-ll-node :data data)))
    (if (null (head LL))
        (setf (head LL) new-node)
        (link-nodes (tail LL) new-node))
    (setf (tail LL) new-node)
    (incf (size LL))
    LL))

(defun insert-before (ll1 ll-node ll2)
  (let ((hd (head ll2))
        (tl (tail ll2))
        (bf (prev ll-node)))
    (link-nodes tl ll-node)
    (if bf
        (link-nodes bf hd)
        (setf (head ll1) hd))
    (setf (size ll1) (+ (size ll1)
                        (size ll2)))))

(defun insert-after (ll1 ll-node ll2)
  (let ((hd (head ll2))
        (tl (tail ll2))
        (af (next ll-node)))
    (link-nodes ll-node hd)
    (if af
        (link-nodes tl af)
        (setf (tail ll1) tl))
    (setf (size ll1) (+ (size ll1)
                        (size ll2)))))

;; Testing
;; (let ((ll (make-LL))
;;       (ll2 (make-LL))
;;       (ll3 (make-LL)))
;;   (insert-back ll 10)
;;   (insert-back ll 1)
;;   (insert-back ll 2)
;;   (insert-back ll2 3)
;;   (insert-back ll2 4)
;;   (insert-back ll2 5)
;;   (insert-back ll3 6)
;;   (insert-back ll3 7)
;;   (insert-back ll3 8)
;;   (insert-before ll (head ll) ll2)
;;   (insert-after ll (tail ll) ll3)
;;   (size ll))
