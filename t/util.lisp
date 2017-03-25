(in-package :satori)

(setf prove:*enable-colors* nil
      prove:*default-reporter* :dot)

(defun evlis (xs)
  (setf *next-serial* 0
        *types* (make-hash-table :test #'equal))
  (cond ((null xs) nil)
        (t (car (reduce
                 #'(lambda (ctx x)
                     (let ((env (second ctx))
                           (tenv (third ctx))
                           (defs (fourth ctx)))
                       (let* ((x* (%eval x env tenv defs))
                              (env* (second x*))
                              (tenv* (third x*))
                              (defs* (fourth x*)))
                         `(,(first x*) ,env* ,tenv* ,defs*))))
                 xs :initial-value `(nil nil nil nil))))))
