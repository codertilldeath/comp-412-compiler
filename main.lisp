(defpackage 412fe
  (:use cl)
  (:import-from :412fe.scanner
                :scan)
  (:export :entry))

(in-package :412fe)

(defun cli-flag-p (f)
  (eql #\-
       (char f 0)))

(defun get-command-type (f)
  (when (cli-flag-p f)
    (char f 1)))

;; (defun parse-args (args)
;;   (let ((flags (remove-if-not #'cli-flag-p
;;                               args)))
;;     (if (> (list-length flags) 1)
;;         )))

(defun higher (new old)
  (let ((pl (list '|-h| '|-s| '|-p| '|-r|)))
    (< (position old pl)
       (position new pl))))

(defun parse-args (args)
  (let (;; help scan-tokens parse-errors parse-repr
             filename option too-many)
    
    ;; Determine present flags
    (loop for arg in (cdr args) do
         (let ((arg-s (intern arg)))
           (cond ((not (cli-flag-p arg))
                  (setq filename arg))
                 ((null option)
                  (setq option arg-s))
                 (t
                  (setq too-many t)
                  (when (higher arg-s option)
                    (setq option arg-s)))))
         ;; (let ((type (get-command-type arg)))
         ;;   (cond ((null type) (setq filename arg))
         ;;         ((eql type #\h) (setq help t))
         ;;         ((eql type #\s) (setq scan-tokens t))
         ;;         ((eql type #\p) (setq parse-errors t))
         ;;         ((eql type #\r) (setq parse-repr t))))
         )

    (when )
    (when too-many
      (format t "Too many arguments passed, defaulting to ~a~%" option))

    ;; ;; Decide on which flag to use, deal with too many flags
    ;; (cond (parse-repr
    ;;        (setq option '|-r|))
    ;;       (parse-errors
    ;;        (setq option '|-p|))
    ;;       (scan-tokens
    ;;        (setq option '|-s|))
    ;;       (help
    ;;        (setq option '|-h|))
    ;;       (t
    ;;        (format t "No options selected.~%")
    ;;        (setq option '|-h|)))

    ;; ;; Check for too many args
    ;; (when (< 1 (list-length
    ;;             (remove-if-not #'identity
    ;;                            (list help scan-tokens parse-errors parse-repr))))
    ;;   (format t "Too many arguments passed, defaulting to ~a~%" option))

    (list option filename)))

(defun output-help ()
  (format t "This is the help message!~%"))

(defun main (argl)
  (destructuring-bind (option file) (parse-args argl)
    (if (eq option '|-h|)
      (output-help))))

(defun entry ()
  (main sb-ext:*posix-argv*))
