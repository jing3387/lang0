(in-package #:satori)

(define-condition unknown-variable-name (error)
  ((argument :initarg :argument :reader argument))
  (:report (lambda (condition stream)
             (format stream "unknown variable ~a" (argument condition)))))

(defun comp (x env tenv)
  (cond
   ((symbolp x) (let ((var (second (assoc x env))))
                  (if var
                      `(,var env tenv)
                      (error 'unknown-variable-name :argument x))))
   ((case (first x)
      (null `(nil env tenv))
      (i32 `(,(comp-int x) ,env ,tenv))
      (variable `(,(comp-var x env tenv) ,env ,tenv))
      (make-closure (let* ((lambda* (second x))
                           (make-env (third x))
                           (tenv* (comp-tenv (rest (second lambda*)) tenv))
                           (c-make-env (first (comp make-env env tenv*)))
                           (envptr (first c-make-env))
                           (tenv** (second c-make-env))
                           (env* (third c-make-env))
                           (clambda* (first (comp lambda* env* tenv**))))
                      `(,(comp-make-closure envptr clambda*) ,env ,tenv)))
      (lambda* (let* ((params (second x))
                      (retty (third x))
                      (body (rest (rest (rest x)))))
                 `(,(comp-lambda* retty params body env tenv) ,env ,tenv)))
      (make-env (let ((env-var (second x))
                      (vs (map 'list #'car (rest (rest x)))))
                  `(,(comp-make-env env-var vs env tenv) ,env ,tenv)))
      (env-ref (let ((env-var (second x))
                     (v (third x))
                     (idx (fourth x)))
                 `(,(comp-env-ref env-var v idx env) ,env ,tenv)))
      (apply-closure (let ((f (second x))
                           (args (rest (rest x))))
                       `(,(comp-apply-closure f args env tenv) ,env ,tenv)))
      (let* (let* ((bindings (second x))
                   (body (rest (rest x)))
                   (env* (reduce
                          #'(lambda (env x)
                              (let* ((var (first x))
                                     (exp (second x))
                                     (alloca (comp-binding var exp env tenv)))
                                `((,(second var) ,alloca) . ,env)))
                          bindings :initial-value env))
                   (tenv* (reduce
                           #'(lambda (tenv x)
                               (let* ((var (first x))
                                      (type (llvm-type (third var) tenv)))
                                 `((,(second var) ,type) . ,tenv)))
                           bindings :initial-value tenv)))
              `(,(comp-progn body env* tenv*) ,env ,tenv)))
      (define* (let* ((var (second x))
                      (exp (third x)))
                 (comp-define var exp env tenv)))
      (if* (let* ((pred (second x))
                  (pred-type (third x))
                  (body-type (fourth x))
                  (true (fifth x))
                  (false (sixth x)))
             `(,(comp-if pred pred-type body-type true false env tenv) ,env ,tenv)))
      (eq* (let* ((type (second x))
                  (lhs (third x))
                  (rhs (fourth x)))
             `(,(comp-eq type lhs rhs env tenv) ,env ,tenv)))
      ((add* sub* mul* sdiv* srem*)
       (let* ((op (first x))
              (lhs (second x))
              (rhs (third x)))
         `(,(comp-int-op op lhs rhs env tenv) ,env ,tenv)))
      (cons* (let ((elements (second x))
                   (types (third x)))
               (comp-cons elements types env tenv)))
      (arity* (let ((type (second x)))
                (comp-arity type env tenv)))
      (nth* (let ((idx (second x))
                  (cons (third x)))
              (comp-index idx cons env tenv)))
      (quote* (let ((const (second x))
                    (type (third x)))
                (comp-quote const type env tenv)))
      (cast* (let ((rettype (second x))
                   (union (third x))
                   (variable (fourth x))
                   (clauses (fifth x)))
               (comp-cast rettype union variable clauses env tenv)))))))

(defun comp-cast (rettype union variable clauses env tenv)
  (let* ((union-exp (first union))
         (cunion (first (comp union-exp env tenv)))
         (tag-indices (vector (llvm:const-int (llvm:int32-type) 0)
                              (llvm:const-int (llvm:int32-type) 0)))
         (tag-ptr (llvm:build-gep *builder* cunion tag-indices ""))
         (tag (llvm:build-load *builder* tag-ptr ""))
         (data-indices (vector (llvm:const-int (llvm:int32-type) 0)
                               (llvm:const-int (llvm:int32-type) 1)))
         (data-ptr (llvm:build-gep *builder* cunion data-indices ""))
         (rettype-ptr (llvm:build-malloc *builder* (llvm-type rettype tenv) ""))
         (rettype-data-indices (if (unionp rettype)
                                   (vector (llvm:const-int (llvm:int32-type) 0)
                                           (llvm:const-int (llvm:int32-type) 1))
                                   (vector (llvm:const-int (llvm:int32-type) 0))))
         (rettype-data-ptr (llvm:build-gep *builder* rettype-ptr rettype-data-indices ""))
         (tagged-types (map 'list #'first clauses))
         (tags (map 'list #'first tagged-types))
         (types (map 'list #'second tagged-types))
         (bodies (map 'list #'second clauses))
         (function (llvm:basic-block-parent (llvm:insertion-block *builder*)))
         (basic-blocks (loop for i from 0 to (1- (length tags))
                             collect (llvm:append-basic-block function "")))
         (merge-bb (llvm:append-basic-block function ""))
         (switch (llvm:build-switch *builder* tag merge-bb (length tags))))
    (map nil
         #'(lambda (tag type body basic-block)
             (llvm:add-case switch
                            (llvm:const-int (llvm:int32-type) tag)
                            basic-block)
             (llvm:position-builder *builder* basic-block)
             (let* ((llvm-type (llvm-type type tenv))
                    (data-bit-cast (llvm:build-bit-cast *builder* data-ptr llvm-type ""))
                    (env* `((,variable ,data-bit-cast) . ,env))
                    (tenv* `((,variable ,llvm-type) . ,tenv))
                    (cbody (first (comp body env* tenv*))))
               (if (unionp rettype)
                   (let* ((rettype-tag-indices (vector (llvm:const-int (llvm:int32-type) 0)
                                                       (llvm:const-int (llvm:int32-type) 0)))
                          (rettype-tag-ptr (llvm:build-gep *builder* rettype-ptr
                                                           rettype-tag-indices "")))
                     (multiple-value-bind (rettype-tag in-table) (gethash rettype *types*)
                       (if in-table
                           (llvm:build-store *builder*
                                             (llvm:const-int (llvm:int32-type) rettype-tag)
                                             rettype-tag-ptr)
                           (let ((new-tag *next-serial*))
                             (llvm:build-store *builder*
                                               (llvm:const-int (llvm:int32-type) new-tag)
                                               rettype-tag-ptr)
                             (setf *next-serial* (1+ *next-serial*))))
                       (let* ((dest-ty (llvm:pointer-type (llvm:struct-type #() nil)))
                              (bit-cast (llvm:build-bit-cast *builder* cbody dest-ty "")))
                         (llvm:build-store *builder* bit-cast rettype-data-ptr))))
                   (let* ((dest-ty (llvm-type rettype tenv))
                          (bit-cast (llvm:build-bit-cast *builder* cbody dest-ty "")))
                     (llvm:build-store *builder* bit-cast rettype-data-ptr)))
               (llvm:build-br *builder* merge-bb)
               (setf basic-block (llvm:insertion-block *builder*))))
         tags types bodies basic-blocks)
    (llvm:position-builder *builder* merge-bb)
    (if (unboxed rettype)
        (let ((ret (llvm:build-load *builder* rettype-ptr "")))
          `(,ret ,env ,tenv))
        `(,rettype-ptr ,env ,tenv))))

(defun unboxed (x)
  (case x
    ((i1 i32) t)
    (t nil)))

(defun comp-quote (const type env tenv)
  (case (first type)
    (i32 `(,(llvm:const-int (llvm:int32-type) const) ,env ,tenv))
    (symbol `(,(llvm:build-global-string-pointer *builder* (string const) "") ,env ,tenv))
    (structure
     (let* ((elements (rest const))
            (types (rest type))
            (types* (map 'list
                         #'(lambda (x)
                             (llvm:pointer-type (llvm-type x tenv)))
                         types))
            (struct-type (llvm:struct-type (coerce types* 'vector) nil))
            (ptr (llvm:build-malloc *builder* struct-type ""))
            (idx 0))
       (map nil
            #'(lambda (const type)
                (let* ((cconst (first (comp-quote const type env tenv)))
                       (type* (llvm-type type tenv))
                       (const-ptr (llvm:build-malloc *builder* type* ""))
                       (indices (vector (llvm:const-int (llvm:int32-type) 0)
                                        (llvm:const-int (llvm:int32-type) idx)))
                       (element-ptr (llvm:build-gep *builder* ptr indices "")))
                  (llvm:build-store *builder* cconst const-ptr)
                  (llvm:build-store *builder* const-ptr element-ptr)
                  (setf idx (1+ idx))))
            elements types)
       `(,ptr ,env ,tenv)))))

(defun comp-arity (type env tenv)
  (let* ((type* (satori-type type tenv))
         (arity (length (rest type*))))
    `(,(llvm:const-int (llvm:int32-type) arity) ,env ,tenv)))

(defun comp-index (idx cons env tenv)
  (let* ((ccons (first (comp cons env tenv)))
         (indices (vector (llvm:const-int (llvm:int32-type) 0)
                          (llvm:const-int (llvm:int32-type) idx)))
         (ptr (llvm:build-gep *builder* ccons indices ""))
         (load (llvm:build-load *builder* ptr ""))
         (element-indices (vector (llvm:const-int (llvm:int32-type) 0)))
         (element-ptr (llvm:build-gep *builder* load element-indices ""))
         (element-load (llvm:build-load *builder* element-ptr "")))
    `(,element-load ,env ,tenv)))

(defun comp-cons (elements types env tenv)
  (let* ((types* (map 'list
                      #'(lambda (x)
                          (llvm:pointer-type (llvm-type x tenv)))
                      types))
         (struct-type (llvm:struct-type (coerce types* 'vector) nil))
         (ptr (llvm:build-malloc *builder* struct-type ""))
         (idx 0))
    (map nil
         #'(lambda (exp type)
             (let* ((cexp (first (comp exp env tenv)))
                    (type* (llvm-type type tenv))
                    (exp-ptr (llvm:build-malloc *builder* type* ""))
                    (indices (vector (llvm:const-int (llvm:int32-type) 0)
                                     (llvm:const-int (llvm:int32-type) idx)))
                    (element-ptr (llvm:build-gep *builder* ptr indices "")))
               (llvm:build-store *builder* cexp exp-ptr)
               (llvm:build-store *builder* exp-ptr element-ptr)
               (setf idx (1+ idx))))
         elements types)
    `(,ptr ,env ,tenv)))

(defun comp-int-op (op lhs rhs env tenv)
  (let* ((clhs (first (comp lhs env tenv)))
         (crhs (first (comp rhs env tenv))))
    (case op
      (add* (llvm:build-add *builder* clhs crhs ""))
      (sub* (llvm:build-sub *builder* clhs crhs ""))
      (mul* (llvm:build-mul *builder* clhs crhs ""))
      (sdiv* (llvm:build-s-div *builder* clhs crhs ""))
      (srem* (llvm:build-s-rem *builder* clhs crhs "")))))

(defun comp-eq (type lhs rhs env tenv)
  (let* ((clhs (first (comp lhs env tenv)))
         (crhs (first (comp rhs env tenv))))
    (case (satori-type type tenv)
      (i32 (llvm:build-i-cmp *builder* := clhs crhs "")))))

(defun comp-bool (x)
  (if x
      (llvm:const-int (llvm:int1-type) 1)
      (llvm:const-int (llvm:int1-type) 0)))

(defun comp-if (pred pred-type body-type true false env tenv)
  (let* ((cond* (comp-cond pred pred-type env tenv))
         (element-types (vector (llvm:int32-type)
                                (llvm:pointer-type (llvm:struct-type #() nil))))
         (union-type (when (unionp body-type)
                       (llvm:struct-type element-types nil)))
         (union-mem (when union-type
                        (llvm:build-malloc *builder* union-type "")))
         (union-body-indices (vector (llvm:const-int (llvm:int32-type) 0)
                                     (llvm:const-int (llvm:int32-type) 1)))
         (union-body-ptr (when union-type
                           (llvm:build-gep *builder* union-mem union-body-indices "")))
         (union-body-type (llvm:pointer-type (llvm:struct-type #() nil)))
         (union-tag-indices (vector (llvm:const-int (llvm:int32-type) 0)
                                    (llvm:const-int (llvm:int32-type) 0)))
         (union-tag-ptr (when union-type
                          (llvm:build-gep *builder* union-mem union-tag-indices "")))
         (function (llvm:basic-block-parent (llvm:insertion-block *builder*)))
         (then-bb (llvm:append-basic-block function ""))
         (else-bb (llvm:append-basic-block function ""))
         (merge-bb (llvm:append-basic-block function "")))
    (llvm:build-cond-br *builder* cond* then-bb else-bb)
    (llvm:position-builder *builder* then-bb)
    (let* ((then (first (comp true env tenv)))
           (then-ptr (when union-type
                       (llvm:build-bit-cast *builder* then union-body-type ""))))
      (when union-type
        (llvm:build-store *builder* (llvm:const-int (llvm:int32-type) 0) union-tag-ptr)
        (llvm:build-store *builder* then-ptr union-body-ptr))
      (llvm:build-br *builder* merge-bb)
      (setf then-bb (llvm:insertion-block *builder*))
      (llvm:position-builder *builder* else-bb)
      (let* ((else (first (comp false env tenv)))
             (else-ptr (when union-type
                         (llvm:build-bit-cast *builder* else union-body-type ""))))
        (when union-type
          (llvm:build-store *builder* (llvm:const-int (llvm:int32-type) 1) union-tag-ptr)
          (llvm:build-store *builder* else-ptr union-body-ptr))
        (llvm:build-br *builder* merge-bb)
        (setf else-bb (llvm:insertion-block *builder*))
        (llvm:position-builder *builder* merge-bb)
        (let ((phi (llvm:build-phi *builder* (llvm-type body-type tenv) "")))
          (if union-type
              (llvm:add-incoming phi (list union-mem union-mem) (list then-bb else-bb))
              (llvm:add-incoming phi (list then else) (list then-bb else-bb)))
          phi)))))

(defun varp (x)
  (and (listp x) (= (length x) 3) (eq (first x) 'variable)))

(defun comp-cond (pred pred-type env tenv)
  (let* ((cpred (comp pred env tenv)))
    (case (satori-type pred-type tenv)
      (i1 (llvm:build-i-cmp *builder* :/= (first cpred)
                            (llvm:const-int (llvm:int1-type) 0)
                            ""))
      (i32 (llvm:build-i-cmp *builder* :/= (first cpred) (comp-int '(i32 0)) "")))))

(defun comp-define (var exp env tenv)
  (let ((name (second var))
        (type (third var)))
    (cond
      ((eq type 'i32)
       (let* ((global (llvm:add-global *module* (llvm-type type tenv) ""))
              (env* `((,name ,global) . ,env))
              (tenv* `((,name ,type) . ,tenv)))
         (setf (llvm:initializer global) (comp-int '(i32 0)))
         (let ((cexp (first (comp exp env tenv))))
           (llvm:build-store *builder* cexp global)
           `(,global ,env* ,tenv*))))
      ((case (first type)
         ((structure union)
          (let* ((ctype (llvm-type type tenv))
                 (global (llvm:add-global *module* ctype ""))
                 (cexp (first (comp exp env tenv)))
                 (env* `((,name ,global) . ,env))
                 (tenv* `((,name ,type) . ,tenv)))
            (setf (llvm:initializer global) (llvm:const-pointer-null ctype))
            (llvm:build-store *builder* cexp global)
            `(,global ,env* ,tenv*)))
         (lambda
             (let* ((ctype (llvm-type type tenv))
                    (global (llvm:add-global *module* ctype ""))
                    (env* `((,name ,global) . ,env))
                    (tenv* `((,name ,type) . ,tenv))
                    (cexp (first (comp exp env* tenv*))))
               (setf (llvm:initializer global) (llvm:const-pointer-null ctype))
               (llvm:build-store *builder* cexp global)
               `(nil ,env* ,tenv*))))))))

(defun comp-tenv (vars tenv)
  (let* ((var-types (map 'list
                         #'(lambda (x) (llvm-type x tenv))
                         (map 'list #'third vars)))
         (var-names (map 'list #'second vars)))
    (if vars
        `(,@(map 'list #'list var-names var-types) . ,tenv)
      tenv)))

(define-condition unable-to-allocate-memory (error)
  ((argument :initarg :argument :reader argument))
  (:report (lambda (condition stream)
             (format stream
                     "unable to allocate memory for type ~a"
                     (argument condition)))))

(define-condition unknown-type (error)
  ((ty :initarg :ty :reader ty))
  (:report (lambda (condition stream)
             (format stream "unknown type ~a" (ty condition)))))

(defun satori-type (ty tenv)
  (cond
   ((null ty) nil)
   ((eq ty 'void) ty)
   ((eq ty 'i1) ty)
   ((eq ty 'i32) ty)
   ((case (first ty)
      (symbol ty)
      (nth (let* ((idx (second ty))
                  (struct-type (satori-type (third ty) tenv)))
             (nth (1+ idx) struct-type)))
      (structure (let* ((element-types (map 'list
                                            #'(lambda (x)
                                                (satori-type x tenv))
                                            (rest ty))))
                   `(structure ,@element-types)))
      (union (let* ((element-types (map 'list
                                        #'(lambda (x)
                                            (satori-type x tenv))
                                        (rest ty))))
               `(union ,@(remove-duplicates element-types :test #'equal))))
      (lambda (let* ((retty (satori-type (third ty) tenv))
                     (param-types (map 'list
                                       #'(lambda (x) (satori-type x tenv))
                                       (second ty))))
                `(lambda ,param-types ,retty)))
      (type-variable (let ((ty* (second (assoc ty tenv :test #'equal))))
                       (or ty*
                           (error 'unknown-type :ty ty))))
      (t (error 'unknown-type :ty ty))))))

(defun llvm-type (ty tenv)
  (cond
   ((null ty) nil)
   ((eq ty 'void) (llvm:void-type))
   ((eq ty 'i1) (llvm:int1-type))
   ((eq ty 'i32) (llvm:int32-type))
   ((case (first ty)
      (symbol (llvm:pointer-type (llvm:int8-type)))
      (nth (llvm-type (satori-type (third ty) tenv) tenv))
      (structure (let* ((element-types (map 'list
                                            #'(lambda (x)
                                                (llvm:pointer-type (llvm-type x tenv)))
                                            (rest ty)))
                        (element-types* (coerce element-types 'vector)))
                   (llvm:pointer-type (llvm:struct-type element-types* nil))))
      (union (let* ((element-types (vector (llvm:int32-type)
                                           (llvm:pointer-type (llvm:struct-type #() nil)))))
               (llvm:pointer-type (llvm:struct-type element-types nil))))
      (lambda (let* ((retty (llvm-type (third ty) tenv))
                     (param-types (map 'list
                                       #'(lambda (x) (llvm-type x tenv))
                                       (second ty)))
                     (param-types* (coerce param-types 'vector))
                     (element-types (vector
                                     (llvm:pointer-type (llvm:function-type
                                                         retty param-types*))
                                     (first param-types))))
                (llvm:pointer-type (llvm:struct-type element-types nil))))
      (type-variable (let ((ty* (second (assoc ty tenv :test #'equal))))
                       (if (cffi:pointerp ty*)
                           ty*
                         (let ((ty** (llvm-type ty* tenv)))
                           (or ty**
                               (error 'unknown-type :ty ty))))))))
   (t (error 'unknown-type :ty ty))))

(defun comp-binding (var exp env tenv)
  (let* ((name (second var))
         (type (llvm-type (third var) tenv))
         (alloca (llvm:build-alloca *builder* type ""))
         (env* `((,name ,alloca) . ,env))
         (tenv* `((,name ,(llvm-type (third var) tenv)) . ,tenv))
         (code (first (comp exp env* tenv*))))
    (llvm:build-store *builder* code alloca)
    alloca))

(defun comp-make-closure (c-make-env clambda*)
  (if (and c-make-env clambda*)
      (let* ((element-types (vector (llvm:type-of clambda*) (llvm:type-of c-make-env)))
             (closure-type (llvm:struct-type element-types nil)))
        (let* ((ptr (llvm:build-malloc *builder* closure-type ""))
               (idx 0))
          (if ptr
              (progn
                (map nil
                     #'(lambda (x)
                         (let* ((indices (vector (llvm:const-int (llvm:int32-type)
                                                                 0)
                                                 (llvm:const-int (llvm:int32-type)
                                                                 idx)))
                                (var-ptr (llvm:build-gep *builder* ptr indices "")))
                           (llvm:build-store *builder* x var-ptr)
                           (setf idx (1+ idx))))
                     (list clambda* c-make-env))
                ptr)
              (error 'unable-to-allocate-memory
                     :argument (llvm:get-type-by-name *module* closure-type)))))
      (error 'satori-error :message "unable to create closure")))

(defun comp-var (var env tenv)
  (let ((name (second var)))
    (cond
      ((eq name '%callee)
       (let* ((lam (llvm:basic-block-parent (llvm:insertion-block *builder*)))
              (envptr (car (llvm:params lam))))
         (comp-make-closure envptr lam)))
      ((and (listp name) (eq (first name) 'env-ref)) (first (comp name env tenv)))
      ((assoc name env)
       (llvm:build-load *builder* (second (assoc name env)) ""))
      (t (error 'unknown-variable-name :argument name)))))

(defun comp-int (x)
  (llvm:const-int (llvm:int32-type) (second x)))

(defun comp-progn (xs env tenv)
  (cond ((= (length xs) 1) (first (comp (first xs) env tenv)))
        (t (first (last (map 'list #'(lambda (x) (first (comp x env tenv))) xs) 1)))))

(defun comp-lambda* (retty params body env tenv)
  (llvm:with-objects ((*builder* llvm:builder))
    (let* ((param-types (map 'list #'(lambda (x) (llvm-type x tenv))
                             (map 'list #'third params)))
           (param-types* (coerce param-types 'vector))
           (ftype (llvm:function-type (llvm-type retty tenv) param-types*))
           (function (llvm:add-function *module* "" ftype)))
      (if function
          (progn
            (llvm:position-builder-at-end *builder*
                                          (llvm:append-basic-block function "entry"))
            (map nil
                 #'(lambda (argument var)
                     (setf (llvm:value-name argument) (string (second var))))
                 (llvm:params function) params)
            (let* ((env* (reduce
                          #'(lambda (env param)
                              (let* ((var (car param))
                                     (arg (cdr param))
                                     (type (llvm-type (third var) tenv))
                                     (alloca (llvm:build-alloca *builder* type "")))
                                (llvm:build-store *builder* arg alloca)
                                `((,(second var) ,alloca) . ,env)))
                          (pairlis params (llvm:params function)) :initial-value env))
                   (retval (comp-progn body env* tenv)))
              (if retval
                  (progn
                    (llvm:build-ret *builder* retval)
                    function)
                (progn
                  (llvm:delete-function function)
                  (error 'satori-error :message "failed to compile lambda body")))))
          (error 'satori-error :message "failed to compile lambda")))))

(defun comp-make-env (env-var vs env tenv)
  (let* ((types (map 'list
                     #'(lambda (x)
                         (let ((type (second (assoc x tenv))))
                           (if (cffi:pointerp type)
                               type
                             (llvm-type type tenv))))
                     vs))
         (types* (remove-nil types))
         (env-type (llvm:struct-type (coerce types* 'vector) nil))
         (tenv* `((,(second env-var) ,env-type) . ,tenv))
         (ptr (llvm:build-malloc *builder* env-type ""))
         (env* `((,(second env-var) ,ptr) . ,env))
         (idx 0))
    (map nil
         #'(lambda (x)
             (let* ((indices (vector (llvm:const-int (llvm:int32-type) 0)
                                     (llvm:const-int (llvm:int32-type) idx)))
                    (var-ptr (llvm:build-gep *builder* ptr indices "")))
               (llvm:build-store *builder* (first (comp x env tenv*)) var-ptr)
               (setf idx (1+ idx))))
         vs)
    `(,ptr ,tenv* ,env*)))

(defun comp-env-ref (env-var v idx env)
  (let* ((envptr (second (assoc (second env-var) env)))
         (indices (vector (llvm:const-int (llvm:int32-type) 0)
                          (llvm:const-int (llvm:int32-type) idx)))
         (ptr (llvm:build-gep *builder* envptr indices "")))
    (llvm:build-load *builder* ptr (string v))))

(define-condition incorrect-number-of-arguments (error)
  ((expected :initarg :expected :reader expected)
   (actual :initarg :actual :reader actual))
  (:report (lambda (condition stream)
             (format stream
                     "incorrect number of arguments: expected ~a but got ~a"
                     (expected condition)
                     (actual condition)))))

(defun comp-apply-closure (f args env tenv)
  (let* ((cargs (map 'vector #'(lambda (x) (first (comp x env tenv))) args))
         (closure (first (comp f env tenv)))
         (f-indices (vector (llvm:const-int (llvm:int32-type) 0)
                            (llvm:const-int (llvm:int32-type) 0)))
         (fptrptr (llvm:build-gep *builder* closure f-indices ""))
         (fptr (llvm:build-load *builder* fptrptr ""))
         (env-indices (vector (llvm:const-int (llvm:int32-type) 0)
                              (llvm:const-int (llvm:int32-type) 1)))
         (env-ptr (llvm:build-gep *builder* closure env-indices ""))
         (env (llvm:build-load *builder* env-ptr "")))
    (llvm:build-call *builder* fptr (concatenate 'vector (vector env) cargs) "")))

(defun comp-in-main (x env tenv)
  (let ((code (comp x env tenv)))
    (or code
        (error 'satori-error :message "failed to compile top-level expression"))))
