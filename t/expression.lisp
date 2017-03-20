(in-package #:satori)

(setf prove:*default-reporter* :dot
      prove:*enable-colors* nil)

(prove:plan 10)

(llvm:with-objects ((*module* llvm:module "<unknown>")
                    (*builder* llvm:builder)
                    (*execution-engine* llvm:execution-engine *module*))
  ;; Constant maps to a constant.
  (prove:is (satori:evlis '(0)) 0)

  ;; The identity function.
  (prove:is (satori:evlis '(((lambda (x) x) 0))) 0)

  ;; Multiple arguments
  (prove:is (satori:evlis '(((lambda (x y) x) 0 1))) 0)
  (prove:is (satori:evlis '(((lambda (x y) y) 0 1))) 1)

  ;; Pass the identity function as an argument. This involves the unification of
  ;; environment variables for the function variable and function argument.
  (prove:is (satori:evlis '(((lambda (f x) (f x)) (lambda (x) x) 0))) 0)

  ;; Empty `let' expression.
  (prove:is (satori:evlis '((let () 0))) 0)

  ;; Basic `let' expression.
  (prove:is (satori:evlis '((let ((x 0)) x))) 0)

  ;; Sequential `let' expression.
  (prove:is (satori:evlis '((let ((x 0) (y x)) y))) 0)

  ;; Bind the identity function in a `let' expression. This is interesting because
  ;; the `lambda' is substituted into the body of the `let' expression leaving
  ;; empty bindings that have to be handled.
  (prove:is (satori:evlis '(((lambda (x) (let ((f (lambda (x) x))) (f x))) 0))) 0)

  ;; Bind the identity function and a variable leaving just a variable in the
  ;; `let' expression after the identity function is substituted into the body.
  (prove:is (satori:evlis '(((lambda (x) (let ((f (lambda (x) x)) (y x)) (f y))) 0))) 0))

(prove:finalize)
