(in-package #:satori)

(defun collect-definitions (x)
  (labels
      ((rec (x acc)
         (cond
           ((atom x) acc)
           ((case (first x)
              (lambda (mappend #'(lambda (x) (rec x acc)) (rest (rest x))))
              (let (mappend #'(lambda (x) (rec x acc)) (rest (rest x))))
              (define (rec (third x) `((,(second x) . ,(third x)) . ,acc))))))))
    (rec x nil)))

(defun remove-definitions (x)
  (cond
    ((atom x) x)
    ((case (first x)
       (lambda (let* ((params (second x))
                      (body (rest (rest x)))
                      (body* (map 'list #'remove-definitions body)))
                 `(lambda ,params ,@body*)))
       (let (let* ((bindings (second x))
                   (body (rest (rest x)))
                   (body* (map 'list #'remove-definitions body)))
              `(let ,bindings ,@body*)))
       (define (let* ((var (second x))
                      (exp (third x)))
                 (if (isval exp)
                     nil
                     `(define ,var ,(remove-definitions exp)))))
       (t (let ((f (first x))
                (args (rest x)))
            `(,(remove-definitions f) ,@(map 'list #'remove-definitions args))))))))

(defun substitute-definitions (x defs)
  (let* ((x-defs (collect-definitions x))
         (sub (append x-defs defs))
         ;; Definitions can appear in definitions so substitute appropriately.
         (sub* (pairlis (map 'list #'car sub)
                        (map 'list #'(lambda (y) (substitute* sub y))
                             (map 'list #'cdr sub))))
         (x* (remove-nil (remove-definitions (substitute* sub* x)))))
    `(,x* ,sub*)))
