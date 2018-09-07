(in-package :412fe.table)

;; Leading spaces don't affect anything
(link-states *start-state*
             (char-code #\space)
             *start-state*)

;; Neither do leading tabs
(link-states *start-state*
             (char-code #\tab)
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
    (aref *parts-of-speech* s)))
