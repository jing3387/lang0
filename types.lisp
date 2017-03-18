(in-package #:satori)

;; See: http://www.cis.upenn.edu/~bcpierce/tapl/checkers/fullrecon/core.ml

(defun recon (x ctx)
  (cond
    ((integerp x) `(<integer> () (,x <integer>)))
    ((symbolp x) (let ((type (flatten (rest (assoc x ctx)))))
                   `(,type () (,x ,type))))
    ((case (first x)
       (lambda (let ((params (second x))
                     (body (rest (rest x))))
                 (let* ((param-syms (alexandria:make-gensym-list (length params) "T"))
                        (param-types (map 'list
                                          #'list
                                          (make-list (length param-syms)
                                                     :initial-element 'id)
                                          param-syms))
                        (ctx* (append (pairlis params param-types) ctx))
                        (body-recon (recon-progn body ctx*))
                        (body-type (first body-recon))
                        (body-constr (second body-recon))
                        (params* (map 'list
                                      #'(lambda (param type)
                                          `(,param ,type))
                                      params
                                      param-types))
                        (body* (map 'list #'(lambda (x) (third (recon x ctx*))) body)))
                   `((lambda ,param-types ,body-type)
                     ,body-constr
                     (lambda ,params* ,@body*)))))
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
                   (ctx1 (map 'list
                              #'(lambda (var exp)
                                  (let* ((recon1 (recon exp ctx))
                                         (exp-type (first recon1))
                                         (constr1 (second recon1)))
                                    (setf binding-constr (cons constr1 binding-constr))
                                    `(,var ,exp-type)))
                              vars
                              exps))
                   (recon2 (recon-progn body* ctx1))
                   (body-type (first recon2))
                   (body-constr (second recon2))
                   (annotated-bindings (map 'list
                                            #'(lambda (var exp)
                                                `(,var ,(third (recon exp ctx))))
                                            ctx1
                                            exps))
                   (ctx* (append ctx1 ctx))
                   (annotated-body (map 'list
                                        #'(lambda (x)
                                            (third (recon x ctx*)))
                                        body*)))
              `(,body-type
                ,(append binding-constr body-constr)
                (let ,annotated-bindings ,@annotated-body))))
       (t (let* ((f (first x))
                 (xs (rest x))
                 (recon-f (recon f ctx))
                 (recon-xs (map 'list #'(lambda (x) (recon x ctx)) xs))
                 (type-f (first recon-f))
                 (type-xs (map 'list #'first recon-xs))
                 (constr-f (second recon-f))
                 (constr-xs (map 'list #'second recon-xs))
                 (exp-f (third recon-f))
                 (exp-xs (map 'list #'third recon-xs))
                 (type-ret `(id ,(gensym "T")))
                 (new-constr `((,type-f (lambda ,type-xs ,type-ret))))
                 (constr (concatenate 'list new-constr constr-f (flatten constr-xs))))
            `(,type-ret ,constr (,exp-f ,@exp-xs))))))))

(defun recon-progn (xs ctx)
  (cond ((= (length xs) 1) (recon (first xs) ctx))
        (t (first (last (map 'list #'(lambda (x) (recon x ctx)) xs) 1)))))

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
         (id (let ((s (second tyS)))
               (if (equal s tyX)
                   tyT
                   `(id ,s))))
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
         (id (equal (second tyT) tyX))
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
                (id (let ((tyS fst)
                          (tyX (second snd)))
                      (cond
                        ((equal tyS `(id ,tyX)) (u (rest constr)))
                        ((occurs-in tyX tyS)
                         (error 'satori-error :message "circular constraints"))
                        (t (append (u (subst-constr tyX tyS (rest constr)))
                                   `(((id ,tyX) ,tyS))))))))))
        ((and (listp fst)
              (case (first fst)
                (id (let ((tyX (second fst))
                          (tyT snd))
                      (cond
                        ((equal tyT `(id ,tyX)) (u (rest constr)))
                        ((occurs-in tyX tyT) (error 'satori-error
                                                    :message "circular constraints"))
                        (t (append (u (subst-constr tyX tyT (rest constr)))
                                   `(((id ,tyX) ,tyT)))))))
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
    `(,(apply-subst constr* type) ,constr* ,(substitute-type exp constr*))))
