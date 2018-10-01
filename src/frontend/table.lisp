(defpackage scanner-table
  (:use :cl)
  (:export :lookup
           :*parts-of-speech*
           :*start-state*
           :*valid-lexemes*
           :*valid-terminators*
           :*table*))

(in-package :scanner-table)

;; Parts of speech table
(defparameter *parts-of-speech*
  (make-array 14
              :initial-contents
              '(:memop :loadi :arithop :output :nop
                :constant :register :comma :into :newline :comment
                :error :error-register
                :start)
              :element-type 'symbol
              :adjustable nil :displaced-to nil :fill-pointer nil))

;; Expand before compiling
(defun index-of (s)
  (declare ((simple-array symbol (14)) *parts-of-speech*))
  (position s *parts-of-speech*))

;; Useful variables 
(defparameter *error-state* (index-of :error))
(defparameter *error-register-state* (index-of :error-register))
(defparameter *start-state* (index-of :start))
(defparameter *next-state* (car (array-dimensions *parts-of-speech*)))

;; Scanner table
;; 129, the extra character being for non-ascii characters
(defparameter *table* (make-array '(44 129)
                                  :initial-element *error-state*
                                  :element-type 'fixnum
                                  :adjustable nil :displaced-to nil :fill-pointer nil))
(defparameter *valid-lexemes* (index-of :comment))
(defparameter *valid-terminators* (make-array `(,*next-state* 129)
                                              :element-type 'boolean
                                              :initial-element nil))

(defun build-number-dfa (start-state success-state)
  (loop for i from 48 to 57 do
       (progn
         (link-states start-state
                      i
                      success-state)
         (link-states success-state
                      i
                      success-state))))

;; NOTE - Leading zero's are allowed on registers
(defun build-register-dfa ()
  (let ((re-state (index-of :error-register))
        (r-state (index-of :register)))
    (link-states *start-state*
                 (char-code #\r)
                 re-state)
    (build-number-dfa re-state r-state)))


(defun num-p (c)
  (char<= #\0 c #\9))

(defun alpha-p (c)
  (or (char<= #\A c #\Z)
      (char<= #\a c #\z)))

(defun alphanum-p (c)
  (or (num-p c)
      (alpha-p c))) 

(defun build-comments-dfa ()
  (fill-path "//" :comment)
  (let ((index (index-of :comment)))
    ;; Non-ascii characters (128) are allowed in comments
    (loop for i from 0 to 128
       when (not (= i 10))
       do
         (link-states index i index))))

(defun fill-path (word final-state)
  (declare ((simple-array fixnum (44 129)) *table*)
           (fixnum *error-state*))
  (with-input-from-string (stream word)
    (let ((state *start-state*))
      (loop for ch = (read-char stream nil)
            while (peek-char nil stream nil)
            do (let* ((c (char-code ch))
                      (next-state (aref *table* state c)))
                 (declare (fixnum next-state))
                 (if (not (= next-state *error-state*))
                     (setf state next-state)
                     (let ((new-state (make-new-state)))
                       (link-states state c new-state)
                       (setf state new-state))))
            finally (link-states state
                                 (char-code ch)
                                 (index-of final-state))))))


(defun make-new-state ()
  (declare (fixnum *next-state*))
  (let ((new-state-number *next-state*))
    (incf *next-state*)
    new-state-number))

(defun link-states (initial-state char next-state)
  (declare ((simple-array fixnum (44 129)) *table*))
  (setf (aref *table* initial-state char)
        next-state))
