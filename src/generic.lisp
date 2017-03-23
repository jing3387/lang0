(in-package #:satori)

(defun genericp (type)
  (and (listp type) (eq (first type) 'lambda) (find-anywhere 'type-variable type)))

(defun definep (x)
  (and (listp x) (= (length x) 3) (eq (first x) 'define)))

(defun recursive-definition-p (x)
  (and (listp x) (= (length x) 3) (listp (third x)) (eq (first (third x)) 'lambda)
       (find-anywhere (second x) (third x)) (second (third x))))

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
