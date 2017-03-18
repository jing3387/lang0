(in-package #:satori)

(defun comp (x env tenv)
  (cond
    ((case (first x)
       (<integer> (comp-int x))
       (variable (comp-var x env))
       (make-closure (let* ((lambda* (second x))
                            (make-env (third x))
                            (c-make-env (comp make-env env tenv))
                            (clambda* (comp lambda* env tenv)))
                       (comp-make-closure c-make-env clambda*)))
       (lambda* (let* ((params (second x))
                       (retty (third x))
                       (body (rest (rest (rest x)))))
                  (comp-lambda* retty params body env tenv)))
       (make-env (let ((id (second x))
                       (vs (map 'list #'car (rest (rest x)))))
                   (comp-make-env id vs env tenv)))
       (env-ref (let ((e (second x))
                      (v (third x))
                      (idx (fourth x)))
                  (comp-env-ref e v idx)))
       (apply-closure (let ((f (second x))
                            (args (rest (rest x))))
                        (comp-apply-closure f args env tenv)))
       (let* (let* ((bindings (second x))
                    (body (rest (rest x)))
                    (tenv* (reduce
                            #'(lambda (tenv x)
                                (let ((var (first x))
                                      (exp (second x)))
                                  (comp-binding var exp env tenv)
                                  `(,var . ,tenv)))
                            bindings
                            :initial-value tenv)))
               (comp-progn body env tenv*)))))))

(define-condition unable-to-allocate-memory (error)
  ((argument :initarg :argument :reader argument))
  (:report (lambda (condition stream)
             (format stream
                     "unable to allocate memory for type ~a"
                     (argument condition)))))

(define-condition unknown-type (error)
  ((ty :initarg :ty :reader ty)
   (tenv :initarg :tenv :reader tenv))
  (:report (lambda (condition stream)
             (format stream "unknown type ~a in ~a" (ty condition) (tenv condition)))))

(defun llvm-type (ty tenv)
  (cond
   ((eq ty '<integer>) (llvm:int32-type))
   ((case (first ty)
      ;; We don't know the exact type of the struct so mimic its structure and
      ;; hope for the best.
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
      (t (let ((ty* (second (assoc ty tenv :test #'equal))))
           (if (cffi:pointerp ty*)
               (llvm:pointer-type ty*)
             (let ((ty** (llvm-type ty* tenv)))
               (or ty**
                   (error 'unknown-type :ty ty :tenv tenv))))))))))

(defun comp-binding (var exp env tenv)
  (let* ((type (llvm-type (third var) tenv))
         (alloca (llvm:build-alloca *builder* type ""))
         (indices (vector (llvm:const-int (llvm:int32-type) 0)))
         (ptr (llvm:build-gep *builder* alloca indices ""))
         (code (comp exp env tenv)))
    (llvm:build-store *builder* code ptr)
    (let ((load (llvm:build-load *builder* ptr "")))
      (setf (gethash (string (second var)) env) load))))

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

(defun comp-var (var env)
  (let* ((name (string (second var)))
         (var (gethash name env)))
    (or var
        (progn
          (llvm:dump-module *module*)
          (error 'unknown-variable-name :argument name)))))

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
            (setf (llvm:value-name (car (llvm:params function))) (string (gensym)))
            (map nil
                  #'(lambda (argument var)
                      (let ((alloca (llvm:build-alloca *builder*
                                                      (llvm-type (third var) tenv)
                                                      "")))
                        (llvm:build-store *builder* argument alloca)
                        (setf (llvm:value-name argument) (string (second var))
                              (gethash (string (second var)) env)
                              (llvm:build-load *builder* alloca ""))))
                  (rest (llvm:params function))
                  (rest params))
            (let ((retval (comp-progn body env tenv)))
              (if retval
                  (progn
                    (llvm:build-ret *builder* retval)
                    function)
                  (progn
                    (llvm:dump-module *module*)
                    (llvm:delete-function function)
                    (error 'satori-error :message "failed to compile lambda")))))
          (progn
            (llvm:dump-module *module*)
            (error 'satori-error :message "failed to compile lambda"))))))

(defun comp-make-env (id vs env tenv)
  (let* ((types (map 'list #'(lambda (var) (llvm-type (third var) tenv)) vs))
         (env-type (llvm:struct-create-named (llvm:global-context) "")))
    (llvm:struct-set-body env-type (coerce types 'vector))
    (nconc tenv `(((type-variable ,id) ,env-type)))
    (let ((ptr (llvm:build-alloca *builder* env-type ""))
          (idx 0))
      (map nil
           #'(lambda (x)
               (let* ((indices (vector (llvm:const-int (llvm:int32-type) 0)
                                       (llvm:const-int (llvm:int32-type) idx)))
                      (var-ptr (llvm:build-gep *builder* ptr indices "")))
                 (llvm:build-store *builder* (comp x env tenv) var-ptr)
                 (setf idx (1+ idx))))
           (sort-symbols< vs))
      ptr)))

(defun comp-env-ref (e v idx)
  (let* ((env (gethash e *environment-parameters*))
         (indices (vector (llvm:const-int (llvm:int32-type) 0)
                          (llvm:const-int (llvm:int32-type) idx)))
         (ptr (llvm:build-gep *builder* env indices "")))
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
          (error 'failed-to-compile-function :argument (llvm:value-name code))))))
