(in-package #:satori)

(defun sort-symbols< (list)
  (assert (every #'symbolp list))
  (let ((strings (map 'list #'string list)))
    (map 'list #'intern (sort strings #'string<))))

(defun flatten (structure)
  (cond
    ((null structure) nil)
    ((atom structure) (list structure))
    (t (mapcan #'flatten structure))))

(define-condition unknown-variable-name (error)
  ((argument :initarg :argument :reader argument))
  (:report (lambda (condition stream)
             (format stream "unknown variable ~a" (argument condition)))))

(defun lookup (x env)
  (let ((var (gethash x env)))
    (or var
        (error 'unknown-variable-name :argument x))))

(defun mappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lsts)))

(defun find-or-insert (k v env)
  (let ((x (gethash k env)))
    (or x
        (setf (gethash k env) v))
    x))

(defun remove-assoc (k alist)
  (let ((alist* (remove k alist :key #'car)))
    alist*))

(define-condition satori-error (error)
  ((message :initarg :message :reader message))
  (:report (lambda (condition stream)
             (format stream "~a" (message condition)))))

(defun unwrap (x)
  (cond
    ((not (listp x)) x)
    ((and (= (length x) 1) (atom (first x))) (first x))
    (t x)))

(defun remove-nil (x)
  (cond ((listp x) (map 'list #'remove-nil (remove nil x)))
        (t x)))

(defun ir1 (x ctx)
  (third (infer x ctx '())))

(defun ir2 (x ctx)
  (flat-closure-convert (ir1 x ctx)))
