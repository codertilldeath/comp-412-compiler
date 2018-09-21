(in-package :scanner-table)

(defparameter *map-to*
  (make-array '(129) :element-type 'fixnum
              :fill-pointer nil :adjustable nil :displaced-to nil))

;; Every characters maps to itself
(loop for i from 0 to 128
      do (setf (aref *map-to* i) i))

;; Except
(loop for i from 0 to 128
      when (= i (aref *map-to* i))
        do (loop for j from (1+ i) to 127
                 when (= j (aref *map-to* j))
                   do (let ((same t))
                        ;; When both characters trigger the exact same transitions on every state
                        (loop for k from 0 to 43
                              while same
                              when (not (= (aref *table* k i)
                                           (aref *table* k j)))
                                do (setf same nil))
                        (when same
                          (setf (aref *map-to* j) i)))))

(defparameter *table-min*
  (make-array `(44 ,(length (remove-duplicates *map-to*)))
              :element-type 'fixnum
              :initial-element *error-state*))

;; Copy over the necessary table entries
(let ((sorted (sort (remove-duplicates *map-to*) #'<)))
  (loop for i from 0 to 43
        do (loop for it from 0 to (1- (length sorted))
                 for k = (aref sorted it)
                 do
                 (setf (aref *table-min* i it) (aref *table* i k)))))

;; Normalize the indexes 
(let ((sorted (sort (remove-duplicates *map-to*) #'<)))
  (loop for i from 0 to 128
        do
           (setf (aref *map-to* i) (position (aref *map-to* i) sorted))))
