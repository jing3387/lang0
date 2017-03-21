(in-package #:satori)

(prove:plan 23)

(llvm:with-objects ((*module* llvm:module "<unknown>")
                    (*builder* llvm:builder)
                    (*execution-engine* llvm:execution-engine *module*))
  ;; Constant maps to a constant.
  (prove:is (satori:evlis '(0)) 0)

  ;; The identity function.
  (prove:is (satori:evlis '(((lambda (x)
                               x)
                             0))) 0)

  ;; Multiple arguments
  (prove:is (satori:evlis '(((lambda (x y)
                               x)
                             0 1))) 0)
  (prove:is (satori:evlis '(((lambda (x y) y) 0 1))) 1)

  ;; Pass the identity function as an argument. This involves the unification of
  ;; environment variables for the function variable and function argument.
  (prove:is (satori:evlis '(((lambda (f x)
                               (f x))
                             (lambda (x) x)
                             0))) 0)

  ;; Empty `let' expression.
  (prove:is (satori:evlis '((let ()
                              0))) 0)

  ;; Basic `let' expression.
  (prove:is (satori:evlis '((let ((x 0))
                              x))) 0)

  ;; Sequential `let' expression.
  (prove:is (satori:evlis '((let ((x 0)
                                  (y x))
                              y))) 0)

  ;; Bind the identity function in a `let' expression. This is interesting because
  ;; the `lambda' is substituted into the body of the `let' expression leaving
  ;; empty bindings that have to be handled.
  (prove:is (satori:evlis '(((lambda (x)
                               (let ((f (lambda (x) x)))
                                 (f x)))
                             0))) 0)

  ;; Bind the identity function and a variable leaving just a variable in the
  ;; `let' expression after the identity function is substituted into the body.
  (prove:is (satori:evlis '(((lambda (x)
                               (let ((f (lambda (x) x))
                                     (y x))
                                 (f y)))
                             0))) 0)

  ;; An `eq' expression where the operands are equal.
  (prove:is (satori:evlis '((if (eq 0 0) 1 0))) 1)

  ;; An `eq' expression where the operands aren't equal.
  (prove:is (satori:evlis '((if (eq 0 1) 1 0))) 0)

  ;; An `eq' expression that evaluates a defined variable.
  (prove:is (satori:evlis '((define x 0) (if (eq x 1) 1 0))) 0)

  ;; An `eq' expression that evaluates a local variable.
  (prove:is (satori:evlis '((let ((x 1)) (if (eq x 1) 1 0)))) 1)

  ;; An `eq' expression that uses a function argument to decide on the `else' branch.
  (prove:is (satori:evlis '(((lambda (x) (if (eq x 1) 1 0)) 0))) 0)

  ;; An `if' expression passed as an argument to a function.
  (prove:is (satori:evlis '(((lambda (x) x) (if (eq 0 1) 1 0)))) 0)

  ;; An `if' expression used as the expression for a local variable.
  (prove:is (satori:evlis '((let ((x (if (eq 0 1) 1 0))) x))) 0)

  ;; An `if' expression used as the basis for a definition.
  (prove:is (satori:evlis '((define x (if (eq 0 1) 1 0)) x)) 0)

  ;; Arithmetic
  (prove:is (satori:evlis '((add 1 2))) 3)
  (prove:is (satori:evlis '((sub 1 2))) -1)
  (prove:is (satori:evlis '((mul 2 2))) 4)
  (prove:is (satori:evlis '((sdiv 4 2))) 2)
  (prove:is (satori:evlis '((srem 5 2))) 1)

  ;; Recursive definitions
  (prove:is (satori:evlis '((define factorial
                             (lambda (x)
                               (if (eq x 1)
                                   1
                                   (mul x (factorial (sub x 1))))))
                            (factorial 5))) 120)

  (prove:is (satori:evlis '(((let ((factorial (lambda (x)
                                                (if (eq x 1)
                                                    1
                                                    (mul x (factorial (sub x 1)))))))
                               (factorial 5))))) 120))

(prove:finalize)
