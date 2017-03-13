(in-package #:satori)

(defvar *builder*)
(defvar *module*)
(defvar *execution-engine*)
(defvar *fpm*)

(define-condition mml-error (error)
  ((message :initarg :message :reader message))
  (:report (lambda (condition stream)
             (write-string (message condition) stream))))

(defun comp (x env fenv)
  (cond
    ((symbolp x) (comp-var x env))
    ((integerp x) (comp-integer x))
    ((floatp x) (comp-float x))
    ((case (first x)
       (FUNCTION (comp-funptr (second x) fenv))
       (DECLARE (comp-prototype (second x) (third x) (fourth x) fenv))
       (DEFUN (comp-function (second x) (third x) (rest (rest x)) env fenv))
       (LAMBDA (comp-lambda (second x) (rest (rest (rest x))) env fenv))
       (t (comp-call x env fenv))))))

(defun comp-var (name env)
  (let ((var (gethash (string-downcase (string name)) env)))
    (or var
        (progn
          (llvm:dump-module *module*)
          (error 'mml-error :message "unknown variable name")))))

(defun comp-integer (x)
  (llvm:const-int (llvm:int64-type) x))

(defun comp-float (x)
  (llvm:const-real (llvm:double-type) x))

(defun comp-funptr (name fenv)
  (let ((fun (gethash (string-downcase (string name)) fenv)))
    (or fun
        (progn
          (llvm:dump-module *module*)
          (error 'mml-error :message "unknown function name")))))

(defun llvm-type (arg)
  (cond
    ((atom arg) (case arg
                  (<byte> (llvm:int8-type))
                  (<integer> (llvm:int64-type))
                  (<float> (llvm:double-type))
                  (<string> (llvm:pointer-type (llvm:int8-type)))))
    ((case (first arg)
       (FUNCTION (let ((arg-types (coerce (map 'list
                                               #'llvm-type
                                               (second arg))
                                          'vector))
                       (retty (llvm-type (third arg))))
                   (llvm:pointer-type (llvm:function-type retty arg-types))))))))

(defun comp-prototype (sym arg-types retty fenv)
  (let* ((name (string-downcase (string sym)))
         (argtys (coerce (map 'list #'llvm-type arg-types) 'vector))
         (f-type (llvm:function-type (llvm-type retty) argtys))
         (function (llvm:add-function *module* name f-type)))
    (when (not (string= (llvm:value-name function) name))
      (llvm:delete-function function)
      (setf function (llvm:named-function *module* name)))
    (if (= (llvm:count-basic-blocks function) 0)
        (unless (= (llvm:count-params function) (length argtys))
          (progn
            (llvm:dump-module *module*)
            (error 'mml-error
                   :message "redefinition of function with different # args")))
        (progn
          (llvm:dump-module *module*)
          (error 'mml-error :message "redefinition of function")))
    (setf (gethash name fenv) function)
    function))

(defun comp-progn (xs env fenv)
  (cond ((= (length xs) 1) (comp (first xs) env fenv))
        (t (last (map 'list #'(lambda (x) (comp x env fenv)) xs) 0))))

(defun comp-function (name args body env fenv)
  (let ((function (gethash (string-downcase (string name)) fenv)))
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
          (let ((retval (comp-progn body env fenv)))
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
                  (llvm:delete-function function)
                  (error 'mml-error :message "failed to compile function body")))))
        (progn
          (llvm:dump-module *module*)
          (error 'mml-error :message "no function declaration")))))

(defun comp-lambda (args body env fenv)
  (llvm:with-objects ((*builder* llvm:builder))
    (let* ((argtys (make-array (length args) :initial-element (llvm:double-type)))
           (f-type (llvm:function-type (llvm:double-type) argtys))
           (function (llvm:add-function *module* "" f-type)))
      (if (= (llvm:count-basic-blocks function) 0)
          (unless (= (llvm:count-params function) (length argtys))
            (progn
              (llvm:dump-module *module*)
              (error 'mml-error
                     :message "redefinition of function with different # args")))
          (progn
            (llvm:dump-module *module*)
            (error 'mml-error :message "redefinition of function")))
      (llvm:position-builder-at-end *builder*
                                    (llvm:append-basic-block function "entry"))
      (map nil
           (lambda (argument name)
             (setf (llvm:value-name argument) (string-downcase (string name))
                   (gethash (string-downcase (string name)) env) argument))
           (llvm:params function)
           args)
      (let ((retval (comp-progn body env fenv)))
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
              (llvm:delete-function function)
              (error 'mml-error :message "failed to compile function body")))))))

(defun alloca-store-load (llvm-val)
  (let ((alloca (llvm:build-alloca *builder* (llvm:type-of llvm-val) "")))
    (llvm:build-store *builder* llvm-val alloca)
    (llvm:build-load *builder* alloca "")))

(defun comp-call (x env fenv)
  (if (symbolp x)
      (let ((callee-fenv (gethash (string-downcase (string (first x))) fenv))
            (args (rest x)))
        (if callee-fenv
            (if (= (llvm:count-params callee-fenv) (length args))
                (llvm:build-call *builder*
                                 callee-fenv
                                 (map 'vector #'(lambda (x) (comp x env fenv)) args)
                                 "")
                (progn
                  (llvm:dump-module *module*)
                  (error 'mml-error :message "incorrect # arguments passed")))
            (let ((callee-env (gethash (string-downcase (string (first x))) env)))
              (if callee-env
                  (let ((callee (alloca-store-load callee-env)))
                    (llvm:build-call *builder*
                                     callee
                                     (map 'vector
                                          #'(lambda (x) (comp x env fenv))
                                          args)
                                     ""))
                  (progn
                    (llvm:dump-module *module*)
                    (error 'mml-error :message "unknown function referenced"))))))
      (let ((function (comp-lambda (second (first x))
                                   (rest (rest (first x)))
                                   env
                                   fenv))
            (args (rest x)))
        (if (= (llvm:count-params function) (length args))
            (llvm:build-call *builder*
                             function
                             (map 'vector #'(lambda (x) (comp x env fenv)) args)
                             "")))))

(defun comp-top-level-prototype (retty fenv)
  (let* ((name "")
         (argtys #())
         (f-type (llvm:function-type retty argtys))
         (function (llvm:add-function *module* name f-type)))
    (when (not (string= (llvm:value-name function) name))
      (llvm:delete-function function)
      (setf function (llvm:named-function *module* name)))
    (if (= (llvm:count-basic-blocks function) 0)
        (unless (= (llvm:count-params function) (length argtys))
          (progn
            (llvm:dump-module *module*)
            (error 'mml-error
                   :message "redefinition of function with different # args")))
        (progn
          (llvm:dump-module *module*)
          (error 'mml-error :message "redefinition of function")))
    (setf (gethash name fenv) function)
    function))

(defun comp-top-level-expr (x env fenv)
  (llvm:with-objects ((*builder* llvm:builder))
    (let ((function (comp-top-level-prototype (llvm:double-type) fenv)))
      (when function
        (llvm:position-builder-at-end *builder*
                                      (llvm:append-basic-block function "entry"))
        (let ((retval (comp x env fenv)))
          (if retval
              (progn
                (llvm:build-ret *builder* retval)
                (unless (llvm:verify-function function)
                  (llvm:dump-module *module*)
                  (error 'mml-error :message "function verification failure"))
                function)
              (progn
                (llvm:dump-module *module*)
                (llvm:delete-function function)
                (error 'mml-error :message "failed to compile function body"))))))))

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
    (let ((fenv (make-hash-table :test #'equal)))
      (loop
        for x = (read) do
        (let ((env (make-hash-table :test #'equal)))
          (cond
            ((floatp x) (let* ((code (comp-top-level-expr x env fenv))
                               (ptr (llvm:pointer-to-global *execution-engine* code)))
                          (format *error-output* "==> ~f~%"
                                  (if (cffi:pointer-eq ptr code)
                                      (llvm:generic-value-to-float
                                       (llvm:double-type)
                                       (llvm:run-function *execution-engine* ptr ()))
                                      (cffi:foreign-funcall-pointer ptr () :double)))))
            ((case (first x)
               (DECLARE (comp x env fenv))
               (DEFUN (comp x env fenv))
               (t (let* ((code (comp-top-level-expr x env fenv))
                         (ptr (llvm:pointer-to-global *execution-engine* code)))
                    (format *error-output* "==> ~f~%"
                            (if (cffi:pointer-eq ptr code)
                                (llvm:generic-value-to-float
                                 (llvm:double-type)
                                 (llvm:run-function *execution-engine* ptr ()))
                                (cffi:foreign-funcall-pointer ptr
                                                              ()
                                                              :double)))
                    (llvm:dump-module *module*)))))))))))
