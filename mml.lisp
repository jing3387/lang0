(in-package #:mml)

(defvar *builder*)
(defvar *module*)
(defvar *execution-engine*)
(defvar *fpm*)

(define-condition mml-error (error)
  ((message :initarg :message :reader message))
  (:report (lambda (condition stream)
             (write-string (message condition) stream))))

(defun comp (x env)
  (cond
    ((symbolp x) (comp-var x env))
    ((floatp x) (comp-float x))
    ((case (first x)
       (DECLARE (comp-proto (second x) (third x) (fourth x) env))
       (DEFUN (comp-function (second x) (third x) (rest (rest (rest x))) env))
       (t (comp-call x env))))))

(defun comp-var (name env)
  (let ((var (gethash (string-downcase (string name)) env)))
    (or var
        (progn
          (llvm:dump-module *module*)
          (error 'mml-error :message "unknown variable name")))))

(defun comp-float (x)
  (llvm:const-real (llvm:double-type) x))

(defun llvm-type (arg)
  (cond
    ((symbolp arg) (case arg
                     (byte (llvm:int8-type))
                     (integer (llvm:int64-type))
                     (float (llvm:double-type))
                     (string (llvm:pointer-type (llvm:int8-type)))))
    ((case (first arg)
       (LAMBDA (let ((arg-types (coerce (map 'list #'llvm-type (second arg)) 'vector))
                     (retty (llvm-type (third arg))))
                 (llvm:pointer-type (llvm:function-type retty arg-types))))))))

(defun comp-proto (sym arg-types retty env)
  (let* ((name (string-downcase (string sym)))
         (argtys (coerce (mapcar #'llvm-type arg-types) 'vector))
         (f-type (llvm:function-type (llvm-type retty) argtys))
         (function (llvm:add-function *module* name f-type)))
    (when (not (string= (llvm:value-name function) name))
      (llvm:delete-function function)
      (setf function (llvm:named-function *module* name)))
    (if (= (llvm:count-basic-blocks function) 0)
        (unless (= (llvm:count-params function) (length argtys))
          (progn
            (llvm:dump-module *module*)
            (error 'mml-error :message "redefinition of function with different # args")))
        (progn
          (llvm:dump-module *module*)
          (error 'mml-error :message "redefinition of function")))
    (setf (gethash name env) function)
    function))

(defun comp-progn (xs env)
  (cond ((= (length xs) 1) (comp (first xs) env))
        (t (map nil #'(lambda (x) (comp x env))) xs)))

(defun comp-function (name args body env)
  (let ((function (gethash (string-downcase (string name)) env)))
    (if function
        (progn
          (llvm:position-builder-at-end *builder*
                                        (llvm:append-basic-block function "entry"))
          (map nil
               (lambda (argument name)
                 (setf (llvm:value-name argument) (string-downcase (string name))
                       (gethash (string-downcase (string name)) env) argument))
               (llvm:params function)
               args)
          (let ((retval (comp-progn body env)))
            (if retval
                (progn
                  (llvm:build-ret *builder* retval)
                  (unless (llvm:verify-function function)
                    (llvm:dump-module *module*)
                    (error 'mml-error :message "function verification failure"))
                  (llvm:run-function-pass-manager *fpm* function)
                  function)
                (progn
                  (llvm:dump-module *module*)
                  (error 'mml-error :message "failed to compile function body")
                  (llvm:delete-function function)))))
        (progn
          (llvm:dump-module *module*)
          (error 'mml-error :message "no function declaration")))))

(defun comp-call (x env)
  (let ((callee (comp (first x) env))
        (args (rest x)))
    (if callee
        (if (= (llvm:count-params callee) (length args))
            (llvm:build-call *builder*
                             callee
                             (map 'vector #'(lambda (x) (comp x env)) args)
                             "calltmp")
            (progn
              (llvm:dump-module *module*)
              (error 'mml-error :message "incorrect # arguments passed")))
        (progn
          (llvm:dump-module *module*)
          (error 'mml-error :message "unknown function referenced")))))

(defun comp-top-level-expr (x env)
  (let ((function (comp-proto "" 'float '() env)))
    (when function
      (llvm:position-builder-at-end *builder*
                                    (llvm:append-basic-block function "entry"))
      (let ((retval (comp x env)))
        (if retval
            (progn
              (llvm:build-ret *builder* retval)
              (unless (llvm:verify-function function)
                (llvm:dump-module *module*)
                (error 'mml-error :message "function verification failure"))
              function)
            (progn
              (llvm:dump-module *module*)
              (error 'mml-error :message "failed to compile function body")
              (llvm:delete-function function)))))))

(defun compiler ()
  (llvm:with-objects ((*builder* llvm:builder)
                      (*module* llvm:module "repl")
                      (*execution-engine* llvm:execution-engine *module*)
                      (*fpm* llvm:function-pass-manager *module*))
    (llvm:add-instruction-combining-pass *fpm*)
    (llvm:add-reassociate-pass *fpm*)
    (llvm:add-gvn-pass *fpm*)
    (llvm:add-cfg-simplification-pass *fpm*)
    (llvm:initialize-function-pass-manager *fpm*)
    (defvar *object-type* (llvm:struct-create-named (llvm:global-context) ""))
    (llvm:struct-set-body *object-type*
                          (vector (llvm:int-type 64)
                                  (llvm:pointer-type (llvm:int-type 8))
                                  (llvm:array-type *object-type* 2)))
    (let ((env (make-hash-table :test #'equal)))
      (loop
        for x = (read) do
        (case (first x)
          (DECLARE (comp x env))
          (DEFUN (comp x env))
          (t (let* ((code (comp-top-level-expr x env))
                    (ptr (llvm:pointer-to-global *execution-engine* code)))
               (format *error-output* "==> ~f~%~%"
                       (if (cffi:pointer-eq ptr code)
                           (llvm:generic-value-to-pointer
                            *object-type*
                            (llvm:run-function *execution-engine* ptr ()))
                           (cffi:foreign-funcall-pointer ptr () :double))))))
            (llvm:dump-module *module*)))))
