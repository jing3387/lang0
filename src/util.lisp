(in-package #:satori)

(defun isval (x)
  (cond
    ((integerp x) t)
    ((and (listp x)
          (case (first x)
            (lambda t)
            (lambda% t))))
    (t nil)))

(defun find-anywhere (item tree)
  (cond ((eql item tree) tree)
        ((atom tree) nil)
        ((find-anywhere item (first tree)))
        ((find-anywhere item (rest tree)))))

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
