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
       (LAMBDA (comp-lambda (second x) (rest (rest x)) env))
       (t (comp-call x env))))))

(defun comp-var (name env)
  (let ((var (gethash name env)))
    (or var
        (error 'mml-error :message "unknown variable name"))))

(defun comp-float (x)
  (llvm:const-real (llvm:double-type) x))

(defun comp-proto (args env)
  (let* ((doubles (make-array (length args)
                              :initial-element (llvm:double-type)))
         (name "")
         (f-type (llvm:function-type (llvm:double-type) doubles))
         (function (llvm:add-function *module* name f-type)))
    (when (not (string= (llvm:value-name function) name))
      (llvm:delete-function function)
      (setf function (llvm:named-function *module* name)))
    (if (= (llvm:count-basic-blocks function) 0)
        (if (= (llvm:count-params function) (length args))
            (map nil
                 (lambda (argument name)
                   (setf (llvm:value-name argument) (string-downcase (string name))
                         (gethash name env) argument))
                 (llvm:params function)
                 args)
            (error 'mml-error
                   :message "redefinition of function with different # args"))
        (error 'mml-error :message "redefinition of function"))
    function))

(defun comp-progn (xs env)
  (cond ((= (length xs) 1) (comp (first xs) env))))

(defun comp-lambda (args body env)
  (let ((function (comp-proto args env)))
    (llvm:with-objects ((*builder* llvm:builder))
      (when function
        (llvm:position-builder-at-end *builder*
                                      (llvm:append-basic-block function "entry"))
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
                (error 'mml-error :message "failed to compile function body")
                (llvm:delete-function function))))))))

(defun top-level (x env)
  (let ((function (comp-proto '() env)))
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
              (error 'mml-error :message "failed to compile function body")
              (llvm:delete-function function)))))))

(defun comp-call (x env)
  (let ((callee (comp (first x) env))
        (args (rest x)))
    (if callee
        (if (= (llvm:count-params callee) (length args))
            (llvm:build-call *builder*
                             callee
                             (map 'vector #'(lambda (x) (comp x env)) args)
                             "calltmp")
            (error 'mml-error :message "incorrect # arguments passed"))
        (error 'mml-error :message "unknown function referenced"))))

(defun compiler (x)
  (llvm:with-objects ((*builder* llvm:builder)
                      (*module* llvm:module "repl")
                      (*execution-engine* llvm:execution-engine *module*)
                      (*fpm* llvm:function-pass-manager *module*))
    (llvm:add-instruction-combining-pass *fpm*)
    (llvm:add-reassociate-pass *fpm*)
    (llvm:add-gvn-pass *fpm*)
    (llvm:add-cfg-simplification-pass *fpm*)
    (llvm:initialize-function-pass-manager *fpm*)
    (case (first x)
      (LAMBDA (comp x (make-hash-table :test #'equal)))
      (t (let* ((env (make-hash-table :test #'equal))
                (code (top-level x env))
                (ptr (llvm:pointer-to-global *execution-engine* code)))
           (format *error-output* "==> ~f~%~%"
                   (if (cffi:pointer-eq ptr code)
                       (llvm:generic-value-to-float
                        (llvm:double-type)
                        (llvm:run-function *execution-engine* ptr ()))
                       (cffi:foreign-funcall-pointer ptr () :double))))))
    (llvm:dump-module *module*)))
