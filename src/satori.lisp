(in-package #:satori)

(defvar *builder*)
(defvar *module*)
(defvar *execution-engine*)

;; For uniquely identifying types
(defvar *next-serial*)
(defvar *types*)

(define-condition satori-error (error)
  ((message :initarg :message :reader message))
  (:report (lambda (condition stream)
             (format stream "~a" (message condition)))))

(defun repl ()
  (llvm:with-objects ((*module* llvm:module "<unknown>")
                      (*builder* llvm:builder)
                      (*execution-engine* llvm:execution-engine *module*))
    (format *error-output* "? ")
    (let ((env nil)
          (tenv nil)
          (defs nil))
      (setf *next-serial* 0
            *types* (make-hash-table :test #'equal))
      (loop for x = (read *standard-input* nil 'eof nil)
            while (not (equal x 'eof)) do
              (let ((result (%eval x env tenv defs)))
                (setf env (second result)
                      tenv (third result)
                      defs (fourth result))
                (format *error-output* "~a~%" (first result))
                (format *error-output* "? ")))
      (llvm:dump-module *module*))))
