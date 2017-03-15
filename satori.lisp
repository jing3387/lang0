(in-package #:satori)

(defun closure-convert (x)
  (cond
    ((symbolp x) x)
    ((case (first x)
       (lambda (let* ((id (gensym))
                      (params (cons id (second x)))
                      (fv (free x))
                      (env (pairlis fv fv))
                      (sub (map 'list
                                #'(lambda (x)
                                    `(,x . (env-ref ,id ,x)))
                                fv))
                      (body (substitute sub (rest (rest x)))))
                 `(make-closure (lambda* ,params ,body)
                                (make-env ,id ,@env))))
       (lambda* x)
       (make-closure x)
       (make-env x)
       (env-ref x)
       (apply-closure x)
       (t (if (= (length x) 1)
              (first x)
              (let ((f (first x))
                    (args (rest x)))
                `(apply-closure ,f . ,args))))))))

(defun flatten (structure)
  (cond
    ((null structure) nil)
    ((atom structure) (list structure))
    (t (mapcan #'flatten structure))))

(defun free (x)
  (cond
    ((symbolp x) (list x))
    ((case (first x)
       (lambda (let ((params (second x))
                     (body (rest (rest x))))
                 (set-difference (free body) params)))
       (lambda* (let ((params (second x))
                      (body (rest (rest x))))
                  (set-difference (free body) params)))
       (make-closure (let ((proc (second x))
                           (env (third x)))
                       (union (free proc) (free env))))
       (make-env (let ((es (mapcar #'cdr (rest (rest x)))))
                   (delete-duplicates (flatten (map 'list #'free es)))))
       (env-ref (let ((env (second x)))
                  (free env)))
       (apply-closure (let ((f (second x))
                            (args (rest (rest x))))
                        (delete-duplicates (flatten (map 'list #'free `(,f . ,args))))))
       (t (let ((f (first x))
                (args (rest x)))
            (delete-duplicates (flatten (map 'list #'free `(,f . ,args))))))))))

(defun substitute (sub x)
  (cond
    ((symbolp x) (if (assoc x sub)
                     (cdr (assoc x sub))
                     x))
    ((case (first x)
       (lambda (let* ((params (second x))
                      (body (rest (rest x)))
                      (sub* (map 'list
                                 #'(lambda (x)
                                     (let ((k (car x)))
                                       (when (not (member k params))
                                         x)))
                                 sub)))
                 `(lambda ,params ,@(substitute sub* body))))
       (lambda* (let* ((params (second x))
                       (body (rest (rest x)))
                       (sub* (map 'list
                                  #'(lambda (x)
                                      (let ((k (car x)))
                                        (when (not (member k params))
                                          x)))
                                  sub)))
                  `(lambda* ,params ,@(substitute sub* body))))
       (make-closure (let ((lam (second x))
                           (env (third x)))
                       `(make-closure ,(substitute sub lam) ,(substitute sub env))))
       (make-env (let ((id (second x))
                       (vs (map 'list #'car (rest (rest x))))
                       (es (map 'list #'cdr (rest (rest x)))))
                   `(make-env ,id ,@(pairlis vs (map 'list
                                                     #'(lambda (x)
                                                         (substitute sub x))
                                                     es)))))
       (env-ref (let ((env (second x))
                      (v (third x)))
                  `(env-ref ,(substitute sub env) ,v)))
       (apply-closure (let ((f (second x))
                            (args (rest (rest x))))
                        `(apply-closure ,@(map 'list
                                               #'(lambda (x)
                                                   (substitute sub x))
                                               `(,f . ,args)))))
       (t (let ((f (first x))
                (args (rest x)))
            (map 'list #'(lambda (x) (substitute sub x)) `(,f . ,args))))))))

(defun transform-bottom-up (f x)
  (defun transform (x*) (transform-bottom-up f x*))
  (let ((x* (cond
              ((symbolp x) x)
              ((case (first x)
                 (lambda (let ((params (second x))
                               (body (rest (rest x))))
                           `(lambda ,params ,@(transform body))))
                 (lambda* (let ((params (second x))
                                (body (rest (rest x))))
                            `(lambda* ,params ,@(transform body))))
                 (make-closure (let ((lam (second x))
                                     (env (third x)))
                                 `(make-closure ,(transform lam) ,(transform env))))
                 (make-env (let ((id (second x))
                                 (vs (map 'list #'car (rest (rest x))))
                                 (es (map 'list #'cdr (rest (rest x)))))
                                 `(make-env ,id ,@(pairlis vs
                                                           (map 'list
                                                                #'transform
                                                                es)))))
                 (env-ref (let ((env (second x))
                                (v (third x)))
                            `(env-ref ,(transform env) ,v)))
                 (apply-closure (let ((f (second x))
                                      (args (rest (rest x))))
                                  `(apply-closure ,(transform f)
                                                  ,@(map 'list #'transform args))))
                 (t (if (= (length x) 1)
                        (transform (first x))
                        (let ((f (first x))
                              (args (rest x)))
                          `(,(transform f) ,@(map 'list #'transform args))))))))))
    (funcall f x*)))

(defun flat-closure-convert (x)
  (transform-bottom-up #'closure-convert x))

(defvar *example* '(lambda (f)
                    (lambda (z)
                      (lambda (x)
                        (f x z a)))))

(defvar *builder*)
(defvar *module*)

(defun comp (x env)
  (cond
    ((symbolp x) (comp-var x env))
    ((integerp x) (comp-int x))
    ((case (first x)
       (make-closure (let* ((lambda* (second x))
                            (make-env (third x))
                            (c-make-env (comp make-env env))
                            (clambda* (comp lambda* env)))
                       (comp-make-closure c-make-env clambda*)))
       (lambda* (let ((id (first (second x)))
                      (params (rest (second x)))
                      (body (rest (rest x))))
                  (comp-lambda* id params body env)))
       (make-env (let ((id (second x))
                       (vs (map 'list #'car (rest (rest x))))
                       (es (map 'list #'cdr (rest (rest x)))))
                   (comp-make-env id vs es env)))
       (env-ref (let ((e (second x))
                      (v (third x)))))
       (apply-closure (let ((f (second x))
                            (args (rest (rest x))))))))))

(define-condition unable-to-allocate (error)
  ((argument :initarg :argument :reader argument))
  (:report (lambda (condition stream)
             (format stream
                     "unable to allocate memory for type ~a"
                     (argument condition)))))

(defun comp-make-closure (c-make-env clambda*)
  (let* ((closure-type (llvm:struct-create-named (llvm:global-context) ""))
         (element-types (vector (llvm:type-of clambda*) (llvm:type-of c-make-env) )))
    (llvm:struct-set-body closure-type element-types)
    (let* ((ptr (llvm:build-alloca *builder* closure-type ""))
           (count 0))
      (if ptr
          (map nil
               #'(lambda (x)
                   (let* ((indices (vector (llvm:const-int (llvm:int32-type) 0)
                                           (llvm:const-int (llvm:int32-type) count)))
                          (var-ptr (llvm:build-gep *builder* ptr indices "")))
                     (llvm:build-store *builder* x var-ptr)
                     (setf count (1+ count))))
               (list clambda* c-make-env))
          (progn
            (llvm:dump-module *module*)
            (error 'unable-to-allocate-on-stack
                   :argument (llvm:get-type-by-name *module* closure-type))))
      closure-type)))

(define-condition unknown-variable-name (error)
  ((argument :initarg :argument :reader argument))
  (:report (lambda (condition stream)
             (format stream "unknown variable name ~a" (argument condition)))))

(defun comp-var (sym env)
  (let* ((name (string sym))
         (var (gethash name env)))
    (or var
        (progn
          (llvm:dump-module *module*)
          (error 'unknown-variable-name :argument sym)))))

(defun comp-int (x)
  (llvm:const-int (llvm:int32-type) x))

(defvar *closure-environments*)

(define-condition failed-to-compile-function (error)
  ((argument :initarg :argument :reader argument))
  (:report (lambda (condition stream)
             (format stream
                     "failed to compile function ~a"
                     (argument condition)))))

(define-condition no-closure-environment (error)
  ((argument :initarg :argument :reader argument))
  (:report (lambda (condition stream)
             (format stream
                     "no environment associated with closure ~a"
                     (argument condition)))))

(defun comp-progn (xs env)
  (cond ((= (length xs) 1) (comp (first xs) env))
        (t (last (map 'list #'(lambda (x) (comp x env)) xs) 0))))

(defun comp-lambda* (id params body env)
  (llvm:with-objects ((*builder* llvm:builder))
    (let ((cenv (gethash id *closure-environments*)))
      (unless cenv
        (llvm:dump-module *module*)
        (error 'no-closure-environment :argument id))
      (let* ((param-types (concatenate
                           'vector
                           (make-array 1 :initial-element cenv)
                           (make-array (length params)
                                       :initial-element (llvm:int32-type))))
             (name (string id))
             (ftype (llvm:function-type (llvm:int32-type) param-types))
             (function (llvm:add-function *module* name ftype)))
        (if function
            (progn
              (llvm:position-builder-at-end *builder*
                                            (llvm:append-basic-block function "entry"))
              (setf (llvm:value-name (car (llvm:params function))) (string 'env))
              (map nil
                   #'(lambda (argument name)
                       (setf (llvm:value-name argument) (string name)
                             (gethash (string name) env) argument))
                   (cdr (llvm:params function))
                   params)
              (let ((retval (comp-progn body env)))
                (if retval
                    (progn
                      (llvm:build-ret *builder* retval)
                      function)
                    (progn
                      (llvm:dump-module *module*)
                      (llvm:delete-function function)
                      (error 'failed-to-compile-function :argument id)))))
            (progn
              (llvm:dump-module *module*)
              (error 'failed-to-compile-function :argument id)))))))

(defun comp-make-env (id vs es env)
  (let* ((name (string id))
         (env-type (llvm:struct-create-named (llvm:global-context) name)))
    (llvm:struct-set-body env-type
                          (make-array (length es)
                                      :initial-element (llvm:int32-type)))
    (let ((ptr (llvm:build-alloca *builder* env-type ""))
          (count 0))
      (map nil
           #'(lambda (x)
               (let* ((indices (vector (llvm:const-int (llvm:int32-type) 0)
                                       (llvm:const-int (llvm:int32-type) count)))
                      (var-ptr (llvm:build-gep *builder* ptr indices "")))
                 (llvm:build-store *builder* (comp x env) var-ptr)
                 (setf count (1+ count))))
           es)
      (setf (gethash id *closure-environments*) env-type)
      ptr)))

(defun comp-main (x env)
  (llvm:with-objects ((*builder* llvm:builder))
    (let* ((id (gensym))
           (param-types (make-array 0))
           (ftype (llvm:function-type (llvm:int32-type) param-types))
           (name (string id))
           (function (llvm:add-function *module* name ftype)))
      (llvm:position-builder-at-end *builder*
                                    (llvm:append-basic-block function "entry"))
      (let ((code (comp (flat-closure-convert x) env))
            (retval (comp 0 env)))
        (if (and code retval)
            (progn
              (llvm:build-ret *builder* retval)
              function)
            (progn
              (llvm:dump-module *module*)
              (llvm:delete-function function)
              (error 'failed-to-compile-function
                     :argument (llvm:value-name function))))))))

(defvar *execution-engine*)

(defun compiler ()
  (llvm:with-objects ((*module* llvm:module "satori")
                      (*execution-engine* llvm:execution-engine *module*))
    (setf *closure-environments* (make-hash-table :test #'equal))
    (loop for x = (read) do
      (let* ((env (make-hash-table :test #'equal))
             (main (comp-main x env))
             (fptr (llvm:pointer-to-global *execution-engine* main)))
        (llvm:dump-module *module*)
        (llvm:verify-module *module*)
        (llvm:generic-value-to-int (llvm:run-function *execution-engine* fptr ()) t)))))
