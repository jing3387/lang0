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

(defun mappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lsts)))

(defun unwrap (x)
  (cond
    ((not (listp x)) x)
    ((and (= (length x) 1) (atom (first x))) (first x))
    (t x)))

(defun remove-nil (x)
  (cond ((listp x) (map 'list #'remove-nil (remove nil x)))
        (t x)))
