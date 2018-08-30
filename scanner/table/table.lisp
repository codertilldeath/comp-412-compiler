(defpackage 412fe.scanner.table
  (:use :cl)
  (:export :*parts-of-speech*
           :*table*
           :lookup))

(in-package :412fe.scanner.table)

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
  (let ((re-state (index-of 'error-register))
        (r-state (index-of 'register)))
    (link-states *start-state*
                 (char-code #\r)
                 re-state)
    (build-number-dfa re-state r-state)))


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

(with-input-from-string (stream "hello")
  (loop for ch = (read-char stream nil)
     while (peek-char t stream nil) do
       (print ch)))

(defun make-new-state ()
  (let ((new-state-number *next-state*))
    (setf *table*
          (adjust-array *table*
                        `(,(incf *next-state*) 256)
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

(defun index-of (s)
  (position s *parts-of-speech*))


(progn
  ;; Parts of speech table
  (defparameter *parts-of-speech*
    (make-array 13
                :initial-contents
                '(memop loadi arithop output nop constant register comma into newline error error-register start)))
  
  ;; Useful variables 
  (defparameter *error-state* (index-of 'error))
  (defparameter *error-register-state* (index-of 'error-register))
  (defparameter *start-state* (index-of 'start))
  (defparameter *next-state* (car (array-dimensions *parts-of-speech*)))

  ;; Scanner table
  (defparameter *table* (make-array `(,*next-state* 256)
                                    :initial-element *error-state*))
  
  ;; Leading spaces don't affect anything
  (link-states *start-state*
               (char-code #\space)
               *start-state*)

  ;; Lexemes
  ;; Build complicated DFA's, number and register recognition
  (build-register-dfa)
  (build-number-dfa *start-state* (index-of 'constant))
  
  ;; MEMOPS
  (fill-path "load" 'memop)
  (fill-path "store" 'memop)

  ;; LOADI
  (fill-path "loadI" 'loadi)

  ;; ARITHOP
  (fill-path "add" 'arithop)
  (fill-path "sub" 'arithop)
  (fill-path "mult" 'arithop)
  (fill-path "lshift" 'arithop)
  (fill-path "rshift" 'arithop)

  ;; OUTPUT
  (fill-path "output" 'output)

  ;; NOP
  (fill-path "nop" 'nop)

  ;; COMMA
  (fill-path "," 'comma)

  ;; INTO
  (fill-path "=>" 'into)

  ;; Newline
   (fill-path "
" 'newline)
   )

(defun num-p (c)
  (char<= #\0 c #\9))

(defun alpha-p (c)
  (or (char<= #\A c #\Z)
      (char<= #\a c #\z)))

(defun alphanum-p (c)
  (or (num-p c)
      (alpha-p c)))

(defun punctuation-p (type)
  (or (= type 7)
      (= type 8)
      (= type 9)))

(defun valid-terminator (current-termination next-char)
  (or (null next-char)
      (cond ((punctuation-p current-termination) (or (alphanum-p next-char)
                                                     (char= next-char #\space)))
            (t (not (alphanum-p next-char))))))

(defun follow-word (stream)
  (let ((state *start-state*))
    (loop for ch = (when (not (and (< state *start-state*)
                                   (valid-terminator state
                                                     (peek-char nil stream nil))))
                     (read-char stream nil))
       while ch
       do
         (let ((c (char-code ch)))
           (setf state
                 (aref *table* state c)))
         )
    (lookup state)))

(follow-word (make-string-input-stream "0"))

(defun follow (word)
  (with-input-from-string (stream word)
    (loop for ch = (follow-word stream)
       while (and (not (eq ch 'error))
                  (not (eq ch 'start)))
       do
         (format t "~a~%" ch))))

(defun follow-file (file)
  (with-open-file (stream file)
    (loop for ch = (follow-word stream)
       while (and (not (eq ch 'error))
                  (not (eq ch 'start)))
       do
         (progn
           (format t "~a " ch)
           (when (eq ch 'newline)
             (format t "~%"))))))

(defun lookup (s)
  (aref *parts-of-speech* s))

(follow "add r1,r2 => r3
sub r1, r2=>r3")
