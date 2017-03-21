(in-package #:satori)

(prove:plan 5)

(llvm:with-objects ((*module* llvm:module "<unknown>")
                    (*builder* llvm:builder)
                    (*execution-engine* llvm:execution-engine *module*))

  ;; Constant definition.
  (prove:is (satori:evlis '((define x 0) x)) 0)

  ;; Function definition, note that the `x' in the body isn't substituted with the
  ;; lambda.
  (prove:is (satori:evlis '((define x (lambda (x) x)) (x 0))) 0)

  ;; An `eq' expression that evaluates a defined variable.
  (prove:is (evlis '((define x 0)
                     (if (eq x 1)
                         1
                         0))) 0)

  ;; An `if' expression used as the basis for a definition.
  (prove:is (evlis '((define x
                      (if (eq 0 1)
                          1
                          0))
                     x)) 0)

  ;; Recursive definitions
  (prove:is (evlis '((define factorial
                      (lambda (x)
                        (if (eq x 1)
                            1
                            (mul x (factorial (sub x 1))))))
                     (factorial 5))) 120))

(prove:finalize)
