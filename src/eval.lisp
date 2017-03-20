(in-package #:satori)

(defun foreign-funcall-ptr (ty main)
  (cond
    (t (let ((ptr (llvm:pointer-to-global *execution-engine* main)))
         (case ty
           (void (progn
                   (if (cffi:pointer-eq main ptr)
                       (llvm:run-function *execution-engine* ptr ())
                       (cffi:foreign-funcall-pointer ptr () :void))
                   nil))
           (i32 (if (cffi:pointer-eq main ptr)
                    (llvm:generic-value-to-int
                     (llvm:run-function *execution-engine* ptr ()) t)
                    (cffi:foreign-funcall-pointer ptr () :int32))))))))

(defun %eval (x env tenv defs)
  (let* ((ctx (remove-if-not #'(lambda (x) (symbolp (first x))) tenv))
         (constr (remove-if #'(lambda (x) (symbolp (first x))) tenv))
         (sub (substitute-definitions x defs))
         (x& (first sub))
         (defs* (second sub)))
    (if (not x&)
        `(nil ,env ,tenv ,defs*)
        (let* ((inference (infer x& ctx constr))
               (type (first inference))
               (tenv* (append (second inference) tenv))
               (env* (append (fourth inference) env))
               (ir1 (third inference))
               (ir2 (flat-closure-convert ir1))
               (retty (llvm-type (first inference) tenv))
               (param-types (make-array 0))
               (ftype (llvm:function-type retty param-types))
               (main (llvm:add-function *module* "" ftype)))
          (llvm:position-builder-at-end *builder*
                                        (llvm:append-basic-block main "entry"))
          (let* ((x* (comp-in-main ir2 env* tenv*))
                 (env** (second x*))
                 (tenv** (third x*)))
            (if (cffi:pointer-eq retty (llvm:void-type))
                (llvm:build-ret *builder*)
                (llvm:build-ret *builder* (first x*)))
            (llvm:dump-module *module*)
            (llvm:verify-module *module*)
            (let* ((result (foreign-funcall-ptr type main)))
              `(,result ,env** ,tenv** ,defs*)))))))
