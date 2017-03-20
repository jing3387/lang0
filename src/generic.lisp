(in-package #:satori)

(defun collect-definitions (xs)
  (labels
      ((rec (x acc)
         (cond
           ((atom x) acc)
           ((case (first x)
              (lambda (mappend #'(lambda (x) (rec x acc)) (rest (rest x))))
              (let (mappend #'(lambda (x) (rec x acc)) (rest (rest x))))
              (define (rec (third x) `((,(second x) . ,(third x)) . ,acc))))))))
    (delete-duplicates (mappend #'(lambda (x) (rec x '())) xs) :test #'equal)))

(defun remove-definitions (xs)
  (labels
      ((rec (x)
         (cond
           ((atom x) x)
           ((case (first x)
              (lambda (let* ((params (second x))
                             (body (rest (rest x)))
                             (body* (map 'list #'rec body)))
                        `(lambda ,params ,@body*)))
              (let (let* ((bindings (second x))
                          (body (rest (rest x)))
                          (body* (map 'list #'rec body)))
                     `(let ,bindings ,@body*)))
              (define (let* ((var (second x))
                             (exp (third x)))
                        (if (isval exp)
                            nil
                            `(define ,var ,(rec exp))))))))))
    (remove-nil (map 'list #'rec xs))))

(defun substitute-definitions (xs)
  (let* ((sub (collect-definitions xs))
         ;; Definitions can appear in definitions so substitute appropriately.
         (sub* (pairlis (map 'list #'car sub)
                        (map 'list #'(lambda (x) (substitute* sub x))
                             (map 'list #'cdr sub))))
         (xs* (remove-definitions (map 'list #'(lambda (x) (substitute* sub* x)) xs))))
    (if (not sub)
        xs*
        (substitute-definitions xs*))))
