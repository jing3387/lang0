(in-package #:satori)

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
                       (vs (map 'list #'car (rest (rest x)))))
                   (comp-make-env id vs env)))
       (env-ref (let ((e (second x))
                      (v (third x))
                      (idx (fourth x)))
                  (comp-env-ref e v idx)))
       (apply-closure (let ((f (second x))
                            (args (rest (rest x))))
                        (comp-apply-closure f args env)))
       (let* (let ((bindings (second x))
                   (body (rest (rest x))))
               (map nil
                    #'(lambda (x)
                        (let ((var (first x))
                              (exp (second x)))
                          (comp-binding var exp env)))
                    bindings)
               (comp-progn body env)))))))

(define-condition unable-to-allocate-memory (error)
  ((argument :initarg :argument :reader argument))
  (:report (lambda (condition stream)
             (format stream
                     "unable to allocate memory for type ~a"
                     (argument condition)))))

(defun comp-binding (var exp env)
  (let* ((alloca (llvm:build-alloca *builder* (llvm:int32-type) ""))
         (indices (vector (llvm:const-int (llvm:int32-type) 0)))
         (ptr (llvm:build-gep *builder* alloca indices ""))
         (code (comp exp env)))
    (llvm:build-store *builder* code ptr)
    (let ((load (llvm:build-load *builder* ptr (string var))))
      (setf (gethash (string var) env) load))))

(defun comp-make-closure (c-make-env clambda*)
  (if (and c-make-env clambda*)
      (let* ((closure-type (llvm:struct-create-named (llvm:global-context) ""))
             (element-types (vector (llvm:type-of clambda*) (llvm:type-of c-make-env))))
        (llvm:struct-set-body closure-type element-types)
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
        (t (first (last (map 'list #'(lambda (x) (comp x env)) xs) 1)))))

(defun comp-lambda* (id params body env)
  (llvm:with-objects ((*builder* llvm:builder))
    (let ((cenv (gethash id *closure-environments*)))
      (unless cenv
        (llvm:dump-module *module*)
        (error 'no-closure-environment :argument id))
      (let* ((param-types (concatenate
                           'vector
                           (make-array 1 :initial-element (llvm:type-of cenv))
                           (make-array (length params)
                                       :initial-element (llvm:int32-type))))
             (name (string id))
             (ftype (llvm:function-type (llvm:int32-type) param-types))
             (function (llvm:add-function *module* name ftype)))
        (if function
            (progn
              (llvm:position-builder-at-end *builder*
                                            (llvm:append-basic-block function "entry"))
              (let ((env-param (car (llvm:params function))))
                (setf (llvm:value-name (car (llvm:params function))) (string 'env))
                (setf (gethash id *environment-parameters*) env-param))
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

(defun comp-make-env (id vs env)
  (let* ((name (string id))
         (env-type (llvm:struct-create-named (llvm:global-context) name)))
    (llvm:struct-set-body env-type
                          (make-array (length vs)
                                      :initial-element (llvm:int32-type)))
    (let ((ptr (llvm:build-alloca *builder* env-type ""))
          (idx 0))
      (map nil
           #'(lambda (x)
               (let* ((indices (vector (llvm:const-int (llvm:int32-type) 0)
                                       (llvm:const-int (llvm:int32-type) idx)))
                      (var-ptr (llvm:build-gep *builder* ptr indices "")))
                 (llvm:build-store *builder* (comp x env) var-ptr)
                 (setf idx (1+ idx))))
           (sort-symbols< vs))
      (setf (gethash id *closure-environments*) ptr)
      ptr)))

(defvar *environment-parameters*)

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

(defun comp-apply-closure (f args env)
  (let* ((closure (comp f env))
         (f-indices (vector (llvm:const-int (llvm:int32-type) 0)
                            (llvm:const-int (llvm:int32-type) 0)))
         (fptrptr (llvm:build-gep *builder* closure f-indices ""))
         (fptr (llvm:build-load *builder* fptrptr ""))
         (env-indices (vector (llvm:const-int (llvm:int32-type) 0)
                              (llvm:const-int (llvm:int32-type) 1)))
         (env-ptr (llvm:build-gep *builder* closure env-indices ""))
         (env (llvm:build-load *builder* env-ptr "")))
    (llvm:build-call *builder*
                     fptr
                     (concatenate 'vector
                                  (vector env)
                                  (map 'vector
                                       #'(lambda (x)
                                           (comp x env))
                                       args))
                     "")))

(defun comp-in-main (x env)
  (let ((code (comp x env)))
    (or code
        (progn
          (llvm:dump-module *module*)
          (error 'failed-to-compile-function :argument (llvm:value-name code))))))
