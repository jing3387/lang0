(in-package #:satori)

(defvar *execution-engine*)

(defun compiler ()
  (llvm:with-objects ((*module* llvm:module "satori")
                      (*execution-engine* llvm:execution-engine *module*))
    (setf *closure-environments* (make-hash-table :test #'equal))
    (setf *environment-parameters* (make-hash-table :test #'equal))
    (loop for x = (read) do
      (let* ((env (make-hash-table :test #'equal))
             (ir1 (flat-closure-convert x))
             (main (comp-main ir1 env))
             (fptr (llvm:pointer-to-global *execution-engine* main)))
        (llvm:dump-module *module*)
        (llvm:verify-module *module*)
        (llvm:generic-value-to-int (llvm:run-function *execution-engine* fptr ()) t)))))
