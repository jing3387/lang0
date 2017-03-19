(in-package #:satori)

(defun comp (x env tenv)
  (cond
   ((symbolp x) (let ((var (second (assoc x env))))
                  (or var
                      (progn
                        (llvm:dump-module *module*)
                        (error 'unknown-variable-name :argument x)))))
   ((case (first x)
      (i32 (comp-int x))
      (variable (comp-var x env tenv))
      (make-closure (let* ((lambda* (second x))
                           (make-env (third x))
                           (tenv* (comp-tenv (rest (second lambda*)) tenv))
                           (c-make-env (comp make-env env tenv*))
                           (envptr (first c-make-env))
                           (tenv** (second c-make-env))
                           (env* (third c-make-env))
                           (clambda* (comp lambda* env* tenv**)))
                      (comp-make-closure envptr clambda*)))
      (lambda* (let* ((params (second x))
                      (retty (third x))
                      (body (rest (rest (rest x)))))
                 (comp-lambda* retty params body env tenv)))
      (make-env (let ((env-var (second x))
                      (vs (map 'list #'car (rest (rest x)))))
                  (comp-make-env env-var vs env tenv)))
      (env-ref (let ((env-var (second x))
                     (v (third x))
                     (idx (fourth x)))
                 (comp-env-ref env-var v idx env)))
      (apply-closure (let ((f (second x))
                           (args (rest (rest x))))
                       (comp-apply-closure f args env tenv)))
      (let* (let* ((bindings (second x))
                   (body (rest (rest x)))
                   (env* (reduce
                          #'(lambda (env x)
                              (let* ((var (first x))
                                     (exp (second x))
                                     (load (comp-binding var exp env tenv)))
                                `((,(second var) ,load) . ,env)))
                          bindings :initial-value env))
                   (tenv* (reduce
                           #'(lambda (tenv x)
                               (let* ((var (first x))
                                      (type (llvm-type (third var) tenv)))
                                 `((,(second var) ,type) . ,tenv)))
                           bindings :initial-value tenv)))
              (comp-progn body env* tenv*)))))))

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

(defun llvm-type (ty tenv)
  (cond
   ((null ty) nil)
   ((eq ty 'i32) (llvm:int32-type))
   ((case (first ty)
      (structure (let* ((element-types (map 'list
                                            #'(lambda (x)
                                                (llvm-type x tenv))
                                            (rest ty)))
                        (element-types* (coerce element-types 'vector)))
                   (llvm:pointer-type (llvm:struct-type element-types* nil))))
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
                               (progn
                                 (llvm:dump-module *module*)
                                 (error 'unknown-type :ty ty)))))))))
   (t (llvm:dump-module *module*)
      (error 'unknown-type :ty ty))))

(defun comp-binding (var exp env tenv)
  (let* ((type (llvm-type (third var) tenv))
         (alloca (llvm:build-alloca *builder* type ""))
         (indices (vector (llvm:const-int (llvm:int32-type) 0)))
         (ptr (llvm:build-gep *builder* alloca indices ""))
         (code (comp exp env tenv)))
    (llvm:build-store *builder* code ptr)
    (llvm:build-load *builder* ptr "")))

(defun comp-make-closure (c-make-env clambda*)
  (if (and c-make-env clambda*)
      (let* ((element-types (vector (llvm:type-of clambda*) (llvm:type-of c-make-env)))
             (closure-type (llvm:struct-type element-types nil)))
        (let* ((ptr (llvm:build-alloca *builder* closure-type ""))
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
              (progn
                (llvm:dump-module *module*)
                (error 'unable-to-allocate-memory
                       :argument (llvm:get-type-by-name *module* closure-type))))))
      (progn
        (llvm:dump-module *module*)
        (error 'satori-error :message "unable to create closure"))))

(defun comp-var (var env tenv)
  (let ((name (second var)))
    (cond
     ((and (listp name) (eq (first name) 'env-ref)) (comp name env tenv))
     (t (let ((var (second (assoc name env))))
          (or var
              (progn
                (llvm:dump-module *module*)
                (error 'unknown-variable-name :argument name))))))))

(defun comp-int (x)
  (llvm:const-int (llvm:int32-type) (second x)))

(defun comp-progn (xs env tenv)
  (cond ((= (length xs) 1) (comp (first xs) env tenv))
        (t (first (last (map 'list #'(lambda (x) (comp x env tenv)) xs) 1)))))

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
                                `((,(second var)
                                   ,(llvm:build-load *builder* alloca ""))
                                  . ,env)))
                          (pairlis params (llvm:params function)) :initial-value env))
                   (retval (comp-progn body env* tenv)))
              (if retval
                  (progn
                    (llvm:build-ret *builder* retval)
                    function)
                (progn
                  (llvm:dump-module *module*)
                  (llvm:delete-function function)
                  (error 'satori-error :message "failed to compile lambda body")))))
        (progn
          (llvm:dump-module *module*)
          (error 'satori-error :message "failed to compile lambda"))))))

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
         (ptr (llvm:build-alloca *builder* env-type ""))
         (env* `((,(second env-var) ,ptr) . ,env))
         (idx 0))
    (map nil
         #'(lambda (x)
             (let* ((indices (vector (llvm:const-int (llvm:int32-type) 0)
                                     (llvm:const-int (llvm:int32-type) idx)))
                    (var-ptr (llvm:build-gep *builder* ptr indices "")))
               (llvm:build-store *builder* (comp x env tenv*) var-ptr)
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
  (let* ((cargs (map 'vector #'(lambda (x) (comp x env tenv)) args))
         (closure (comp f env tenv))
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
        (progn
          (llvm:dump-module *module*)
          (error 'satori-error "failed to compile top-level expression")))))
