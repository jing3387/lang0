(in-package #:satori)

;; See: http://www.cis.upenn.edu/~bcpierce/tapl/checkers/fullrecon/core.ml

(defun recon (x ctx)
  (cond
    ((integerp x) `(<integer> () (<integer> ,x)))
    ((symbolp x) (let ((type (unwrap (flatten (rest (assoc x ctx))))))
                   (if type
                       `(,type () (variable ,x ,type))
                       (error 'unknown-variable-name :argument x))))
    ((case (first x)
       (lambda (let ((params `(,(gensym) . ,(second x)))
                     (body (rest (rest x))))
                 (let* ((param-syms (alexandria:make-gensym-list (length params) "T"))
                        (param-types (map 'list
                                          #'list
                                          (make-list (length param-syms)
                                                     :initial-element 'type-variable)
                                          param-syms))
                        (ctx* (append (pairlis params param-types) ctx))
                        (body-recon (map 'list
                                         #'(lambda (x)
                                             (recon x ctx*))
                                         body))
                        (body-last (first (last body-recon)))
                        (body-type (first body-last))
                        (body-constr (second body-last))
                        (params* (map 'list
                                      #'(lambda (param type)
                                          `(variable ,param ,type))
                                      params
                                      param-types))
                        (body* (map 'list #'third body-recon)))
                   `((lambda ,param-types ,body-type)
                     ,body-constr
                     (lambda ,params* ,body-type ,@body*)))))
       (let (let* ((vars (map 'list #'first (second x)))
                   (exps (map 'list #'second (second x)))
                   (body (rest (rest x)))
                   (sub (map 'list
                             #'(lambda (var exp)
                                 (when (isval exp)
                                   `(,var . ,exp)))
                             vars
                             exps))
                   (body* (substitute* sub body))
                   (binding-constr '())
                   (annotated-bindings '())
                   (ctx1 (reverse
                          (reduce
                           #'(lambda (binding ctx)
                               (let* ((var (car binding))
                                      (exp (cdr binding))
                                      (recon1 (recon exp ctx))
                                      (exp-type (first recon1))
                                      (constr1 (second recon1))
                                      (exp* (third recon1)))
                                 (if (not (isval exp*))
                                     (progn
                                       (setf annotated-bindings
                                             `(((variable ,var ,exp-type)
                                                ,exp*)
                                               . ,annotated-bindings))
                                       (setf binding-constr `(,constr1
                                                              . ,binding-constr))
                                       `((,var ,exp-type) . ,ctx))
                                     ctx)))
                           (pairlis vars exps)
                           :initial-value ctx
                           :from-end t)))
                   (annotated-bindings* (reverse annotated-bindings))
                   (ctx* (remove nil ctx1))
                   (recon2 (map 'list #'(lambda (x) (recon x ctx*)) body*))
                   (body-last (first (last recon2)))
                   (body-type (first body-last))
                   (body-constr (second body-last))
                   (annotated-body (map 'list #'third recon2)))
              (if annotated-bindings*
                  `(,body-type
                    ,(append binding-constr body-constr)
                    (let ,annotated-bindings* ,@annotated-body))
                  `(,body-type
                    ,(append binding-constr body-constr)
                    ,annotated-body))))
       (t (let* ((f (first x))
                 (xs (rest x))
                 (recon-f (recon f ctx))
                 (recon-xs (map 'list #'(lambda (x) (recon x ctx)) xs))
                 (type-f (first recon-f))
                 (type-xs `((type-variable ,(gensym "T"))
                            . ,(map 'list #'first recon-xs)))
                 (constr-f (second recon-f))
                 (constr-xs (map 'list #'second recon-xs))
                 (exp-f (third recon-f))
                 (exp-xs (map 'list #'third recon-xs))
                 (type-ret `(type-variable ,(gensym "T")))
                 (new-constr `((,type-f (lambda ,type-xs ,type-ret))))
                 (constr (concatenate 'list new-constr constr-f constr-xs)))
            `(,type-ret ,constr (,exp-f ,@exp-xs))))))))

(defun isval (x)
  (cond
    ((integerp x) t)
    ((and (listp x)
          (case (first x)
            (lambda t))))
    (t nil)))

(defun subst-type (tyX tyT tyS)
  (defun f (tyS)
    (cond
      ((equal tyS '<integer>) '<integer>)
      ((case (first tyS)
         (type-variable (let ((s (second tyS)))
               (if (equal s tyX)
                   tyT
                   `(type-variable ,s))))
         (lambda (let ((tyS1 (second tyS))
                       (tyS2 (third tyS)))
                   `(lambda ,(map 'list #'f tyS1) ,(f tyS2))))))))
  (f tyS))

(defun apply-subst (constr tyT)
  (reduce #'(lambda (tyS x)
              (let ((tyX (second (first x)))
                    (tyC2 (second x)))
                (subst-type tyX tyC2 tyS)))
          (reverse constr)
          :initial-value tyT))

(defun subst-constr (tyX tyT constr)
  (map 'list
       #'(lambda (x)
           (let ((tyS1 (first x))
                 (tyS2 (second x)))
             `(,(subst-type tyX tyT tyS1) ,(subst-type tyX tyT tyS2))))
       constr))

(defun occurs-in (tyX tyT)
  (defun o (tyT)
    (cond
      ((equal tyT '<integer>) nil)
      ((case (first tyT)
         (type-variable (equal (second tyT) tyX))
         (f (let ((tyT1 (second tyT))
                  (tyT2 (third tyT)))
              (or (o tyT1) (o tyT2))))))))
  (o tyT))

(defun unify (constr)
  (defun u (constr)
    (let ((fst (first (first constr)))
          (snd (second (first constr))))
      (cond
        ((null constr) nil)
        ((equal fst snd) (u (rest constr)))
        ((and (listp snd)
              (case (first snd)
                (type-variable (let ((tyS fst)
                          (tyX (second snd)))
                      (cond
                        ((equal tyS `(type-variable ,tyX)) (u (rest constr)))
                        ((occurs-in tyX tyS)
                         (error 'satori-error :message "circular constraints"))
                        (t (append (u (subst-constr tyX tyS (rest constr)))
                                   `(((type-variable ,tyX) ,tyS))))))))))
        ((and (listp fst)
              (case (first fst)
                (type-variable (let ((tyX (second fst))
                          (tyT snd))
                      (cond
                        ((equal tyT `(type-variable ,tyX)) (u (rest constr)))
                        ((occurs-in tyX tyT) (error 'satori-error
                                                    :message "circular constraints"))
                        (t (append (u (subst-constr tyX tyT (rest constr)))
                                   `(((type-variable ,tyX) ,tyT)))))))
                (lambda (let* ((f1 fst)
                               (f2 snd)
                               (tyS1 (second f1))
                               (tyS2 (third f1))
                               (tyT1 (second f2))
                               (tyT2 (third f2))
                               (type-1s (map 'list
                                             #'(lambda (tyS1 tyT1)
                                                 `(,tyS1 ,tyT1))
                                             tyS1
                                             tyT1)))
                          (u `(,@type-1s (,tyS2 ,tyT2) ,@(rest constr))))))))
        (t (error 'satori-error :message "unsolvable constraints")))))
  (u constr))

(defun infer (x ctx constr)
  (let* ((recon (recon x ctx))
         (type (first recon))
         (exp (third recon))
         (constr* (unify (append constr (second recon)))))
    `(,(apply-subst constr* type) ,constr* ,(substitute-type
                                             (substitute-type exp constr*)
                                             constr))))


(defvar *example1* '((lambda (x) (let ((f (lambda (x) x)) (y x)) (f y))) 1))

(defvar *example2* '((lambda (x) (let ((y x) (z y)) z)) 1))

(defvar *example3* '(lambda () x))

(defvar *example4* '((lambda (f x) (f x)) (lambda (x) x) 1))
