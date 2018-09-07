(defpackage 412fe.scanner
  (:use :cl)
  (:import-from :412fe.table
                :lookup
                :*parts-of-speech*
                :*start-state*
                :*valid-terminators*
                :*valid-lexemes*
                :*table*
   :*table-min*
   :*map-to*)
  (:export :scan))

(in-package :412fe.scanner)

(defun valid-terminator (current-termination next-char)
  (declare ((simple-array t (44 129)) *valid-terminators*)
           (fixnum current-termination)
           (fixnum *start-state*))
  (or (not next-char)
      (when (< current-termination *start-state*)
        (aref *valid-terminators* current-termination (char-code next-char)))))

(defun safe-char (i)
  (if (or (null i) (< (char-code i) 128))
    i
    (code-char 128)))

;; (defun follow-word (stream)
;;   (let ((state *start-state*)
;;         (fstr (make-array '(0) :element-type 'character
;;                           :fill-pointer 0 :adjustable t)))
;;     (with-output-to-string (s fstr)
;;       (loop for ch = (when (not (valid-terminator state
;;                                                   (safe-char (peek-char nil stream nil))))
;;                        (safe-char (read-char stream nil)))
;;          while ch
;;          do
;;             (let ((c (char-code ch)))
;;               (unless (eq ch #\return)
;;                 (unless (member ch '(#\space #\tab))
;;                   (format s "~a" ch))
;;                 (setf state
;;                       (aref *table* state c))))))
;;     (cons state fstr)))

(defun follow-word (stream)
  (declare ((simple-array fixnum (129)) *map-to*)
           ((simple-array fixnum (44 26)) *table-min*))
  (let ((state *start-state*)
        (fstr (make-array '(10)
                          :element-type 'character :fill-pointer 0 :adjustable t)))
    (loop for ch = (let* ((char (read-char stream nil))
                          (char-s (safe-char char)))
                     (if (not (valid-terminator state char-s))
                         char-s
                         (unless (null char)
                           (unread-char char stream))))
          while ch
          do
             (let ((c (char-code ch)))
               (unless (eq ch #\return)
                 (unless (member ch '(#\space #\tab))
                   (vector-push-extend ch fstr))
                 (setf state
                       (aref *table-min*
                             state
                             (aref *map-to* c))))))
    (cons state fstr)))
