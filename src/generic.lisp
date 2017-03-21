(in-package #:satori)

(defun genericp (type)
  (find-anywhere 'type-variable type))

(define-condition define-in-body (error)
  ((argument :initarg :argument :reader argument))
  (:report (lambda (condition stream)
             (format stream "definition not at the top-level: ~a" (argument condition)))))

(defun substitute-definitions (x defs)
  (let* ((def (and (definep x) x))
         (defs* (or (and def `((,(second def) . ,(third def))  . ,defs))
                    defs))
         (x* (substitute* defs* x)))
    `(,x* ,defs*)))
