(in-package #:satori)

(prove:plan 25)

(llvm:with-objects ((*module* llvm:module "<unknown>")
                    (*builder* llvm:builder)
                    (*execution-engine* llvm:execution-engine *module*))
  ;; Constant maps to a constant.
  (prove:is (evlis '(0)) 0)

  ;; The identity function.
  (prove:is (evlis '(((lambda (x)
                        x)
                      0))) 0)

  ;; Multiple arguments
  (prove:is (evlis '(((lambda (x y)
                        x)
                      0 1))) 0)
  (prove:is (evlis '(((lambda (x y)
                        y)
                      0 1))) 1)

  ;; Pass the identity function as an argument. This involves the unification of
  ;; environment variables for the function variable and function argument.
  (prove:is (evlis '(((lambda (f x)
                        (f x))
                      (lambda (x)
                        x)
                      0))) 0)

  ;; Empty `let' expression.
  (prove:is (evlis '((let ()
                       0))) 0)

  ;; Basic `let' expression.
  (prove:is (evlis '((let ((x 0))
                       x))) 0)

  ;; Sequential `let' expression.
  (prove:is (evlis '((let ((x 0)
                           (y x))
                       y))) 0)

  ;; Bind the identity function in a `let' expression. This is interesting because
  ;; the `lambda' is substituted into the body of the `let' expression leaving
  ;; empty bindings that have to be handled.
  (prove:is (evlis '(((lambda (x)
                        (let ((f (lambda (x) x)))
                          (f x)))
                      0))) 0)

  ;; Bind the identity function and a variable leaving just a variable in the
  ;; `let' expression after the identity function is substituted into the body.
  (prove:is (evlis '(((lambda (x)
                        (let ((f (lambda (x) x))
                              (y x))
                          (f y)))
                      0))) 0)

  ;; An `eq' expression where the operands are equal.
  (prove:is (evlis '((if (eq 0 0)
                         1
                         0))) 1)

  ;; An `eq' expression where the operands aren't equal.
  (prove:is (evlis '((if (eq 0 1)
                         1
                         0))) 0)

  ;; An `eq' expression that evaluates a defined variable.
  (prove:is (evlis '((define x 0)
                     (if (eq x 1)
                         1
                         0))) 0)

  ;; An `eq' expression that evaluates a local variable.
  (prove:is (evlis '((let ((x 1))
                       (if (eq x 1)
                           1
                           0)))) 1)

  ;; An `eq' expression that uses a function argument to decide on the
  ;; `else' branch.
  (prove:is (evlis '(((lambda (x)
                        (if (eq x 1)
                            1
                            0))
                      0))) 0)

  ;; An `if' expression passed as an argument to a function.
  (prove:is (evlis '(((lambda (x)
                        x)
                      (if (eq 0 1)
                          1
                          0)))) 0)

  ;; An `if' expression used as the expression for a local variable.
  (prove:is (evlis '((let ((x (if (eq 0 1)
                                  1
                                  0)))
                       x))) 0)

  ;; An `if' expression used as the basis for a definition.
  (prove:is (evlis '((define x
                      (if (eq 0 1)
                          1
                          0))
                     x)) 0)

  ;; Arithmetic
  (prove:is (evlis '((add 1 2))) 3)
  (prove:is (evlis '((sub 3 2))) 1)
  (prove:is (evlis '((mul 2 2))) 4)
  (prove:is (evlis '((sdiv 4 2))) 2)
  (prove:is (evlis '((srem 5 2))) 1)

  ;; Recursive definitions
  (prove:is (evlis '((define factorial
                      (lambda (x)
                        (if (eq x 1)
                            1
                            (mul x (factorial (sub x 1))))))
                     (factorial 5))) 120)

  ;; Anonymous recursion, because factorial has a value as its expression the
  ;; `lambda' gets substituted into the body of the `let' therefore anonymous
  ;; recursion is required; see the `%callee' function.
  (prove:is (evlis '((let ((factorial (lambda (x)
                                        (if (eq x 1)
                                            1
                                            (mul x (factorial (sub x 1)))))))
                       (factorial 5)))) 120))

(prove:finalize)
