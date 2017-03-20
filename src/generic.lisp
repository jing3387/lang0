(in-package #:satori)

(defun definep (x)
  (and (listp x) (= (length x) 3) (eq (first x) 'define)))

(defun recp (x)
  (and (definep x) (listp (third x)) (eq (first (third x)) 'lambda)
       (find-anywhere (second x) (rest (rest (third x))))
       (second (third x))))

(define-condition define-in-body (error)
  ((argument :initarg :argument :reader argument))
  (:report (lambda (condition stream)
             (format stream "definition not at the top-level: ~a" (argument condition)))))

(defun substitute-definitions (x defs)
  (let* ((def (and (definep x) x))
         (defs* (or (and def `((,(second def) . ,(third def))  . ,defs))
                    defs))
         (x* (substitute* defs* x)))
    (if (not (find-anywhere 'define (third def)))
        (if (and def (isval (third def)))
            `(nil ,defs*)
            `(,x* ,defs*))
        (error 'define-in-body :argument x*))))
