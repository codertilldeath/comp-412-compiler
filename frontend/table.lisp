(defpackage 412fe.table
  (:use :cl)
  (:export :lookup
           :*parts-of-speech*
           :*start-state*
           :*valid-lexemes*
           :*valid-terminators*
           :*table*
           :get-example))

(in-package :412fe.table)

;; Parts of speech table
(defparameter *parts-of-speech*
  (make-array 14
              :initial-contents
              '(:memop :loadi :arithop :output :nop :constant :register :comma :into :newline :comment :error :error-register :start)))

;; Expand before compiling
(defun index-of (s)
  (position s *parts-of-speech*))

;; Useful variables 
(defparameter *error-state* (index-of :error))
(defparameter *error-register-state* (index-of :error-register))
(defparameter *start-state* (index-of :start))
(defparameter *next-state* (car (array-dimensions *parts-of-speech*)))

;; Scanner table
(defparameter *table* (make-array `(,*next-state* 128)
                                  :initial-element *error-state*))

(defparameter *valid-lexemes* (index-of :comment))
(defparameter *valid-terminators* (make-array `(,*next-state* 128)
                                              :initial-element nil))

;; How do I know when a return carriage is legal?
;; Shouldn't a scanner not have to worry about grammar strictly?
;; What would a scanner for a language like Java look like?
;; The terminating character at the end is context sensitive right?
;; One way I can see that this can be avoided is to make these terminating characters parts of speech.
;; Is this correct?
;; (Probably I have to add newline as a new part of speech. But then how do I denote the end of a word?)
;; (It seems ridiculous to have a space at the end of each line before a newline)
;; (Maybe some hackery needs to be employed)
(defun fill-end-characters (state final)
  (link-states state
               (char-code #\space)
               (index-of final))
  (link-states state
               (char-code #\linefeed)
               (index-of final)))

;; Lenient number. [0-9]+
;; Q: Lenient number approved for registers. But what about constants?
;; (Posted)
;; Maybe have it where this uses an intermediate state
;; which makes the transition after a space is scanned
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
    (loop for i from 1 to 127
       when (not (= i 10))
       do
         (link-states index i index))))

;; Or just check the next character?
(defun fill-path (word final-state)
  (with-input-from-string (stream word)
    (let ((state *start-state*))
      (loop for ch = (read-char stream nil)
         while (peek-char nil stream nil) do
           (let* ((c (char-code ch))
                  (next-state (aref *table* state c)))
             (if (not (= next-state *error-state*))
                 (setf state next-state)
                 (let ((new-state (make-new-state)))
                   (link-states state c new-state)
                   (setf state new-state))))
         finally (link-states state
                              (char-code ch)
                              (index-of final-state))))))

(defun make-new-state ()
  (let ((new-state-number *next-state*))
    (setf *table*
          (adjust-array *table*
                        `(,(incf *next-state*) 128)
                        :initial-element *error-state*))
    new-state-number))

(defun link-states (initial-state char next-state)
  (setf (aref *table* initial-state char)
        next-state))

(defmacro append-symbol (symbol &rest body)
  `(progn
     ,@(mapcar (lambda (x)
                 (append x
                         (list symbol)))
               body)))


(progn
  
  ;; Leading spaces don't affect anything
  (link-states *start-state*
               (char-code #\space)
               *start-state*)
  
  ;; Lexemes
  ;; Build complicated DFA's, number and register recognition
  (build-register-dfa)
  (build-number-dfa *start-state* (index-of :constant))
  
  ;; MEMOPS
  (fill-path "load" :memop)
  (fill-path "store" :memop)

  ;; LOADI
  (fill-path "loadI" :loadi)

  ;; ARITHOP
  (fill-path "add" :arithop)
  (fill-path "sub" :arithop)
  (fill-path "mult" :arithop)
  (fill-path "lshift" :arithop)
  (fill-path "rshift" :arithop)

  ;; OUTPUT
  (fill-path "output" :output)

  ;; NOP
  (fill-path "nop" :nop)

  ;; The above terminators are any non-alphanum characters
  (loop for i from 1 to 127
     when (not (alphanum-p (code-char i)))
     do
       (loop for type from (index-of :memop) to (index-of :register) do
            (setf (aref *valid-terminators* type i)
                  t)))

  ;; COMMA
  (fill-path "," :comma)

  ;; INTO
  (fill-path "=>" :into)

  ;; Newline
   (fill-path "
" :newline)

   ;; In the context of these two, anything is a terminator
  (loop for i from 1 to 127 do
       (loop for type from (index-of :comma) to (index-of :newline) do
            (setf (aref *valid-terminators* type i)
                  t)))

   ;; Comments
  (build-comments-dfa)

  ;; A comment ignores the rest of the line, so only newline is a terminator
  (setf (aref *valid-terminators*
              (index-of :comment)
              (char-code #\newline))
        t)

  ;; Errors can end with any non-alphanumeric
  
  (loop for i from 1 to 127
     when (not (alphanum-p (code-char i)))
     do
       (setf (aref *valid-terminators*
                   (index-of :error)
                   i)
             t))

  (defun lookup (s)
    (when (< s (car (array-dimensions *parts-of-speech*) ))
      (aref *parts-of-speech* s))))



;; (follow "add r1,r2 => r3
;; sub r1, r2=>r3")


;; (with-input-from-string (stream "hello")
;;   (loop for ch = (read-char stream nil)
;;      while (peek-char t stream nil) do
;;        (print ch)))


(defun choose-elt (l)
  (let ((len (length l)))
    (nth (random len) l)))

(defun get-example (s)
  (case s
    (:memop (choose-elt '("load " "store ")))
    (:loadi "loadI ")
    (:arithop (choose-elt '("sub " "add " "mult " "lshift " "rshift ")))
    (:output "output ")
    (:constant (concatenate 'string (write-to-string (random 100)) " "))
    (:register (concatenate 'string "r" (write-to-string (random 100)) " "))
    (:comma ", ")
    (:into "=> ")
    (:newline "
")))
