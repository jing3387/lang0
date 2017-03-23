(in-package :satori)

(prove:plan 7)

(llvm:with-objects ((*module* llvm:module "<unknown>")
                    (*builder* llvm:builder)
                    (*execution-engine* llvm:execution-engine *module*))

  ;; Index a structure.
  (prove:is (evlis '((0 (cons 0 1)))) 0)

  ;; Index a nested structure.
  (prove:is (evlis '((1 (1 (cons 0 (cons 1 2)))))) 2)

  ;; Index a structure bound to a `let' variable then returned.
  (prove:is (evlis '((1 (let ((x (cons 1 2))) x)))) 2)

  ;; Index a structure created inside a lambda.
  (prove:is (evlis '((1 ((lambda (x) (cons x 2)) 1)))) 1)

  ;; Index a structure passed as an argument then returned.
  (prove:is (evlis '((1 ((lambda (x) x) (cons 1 2))))) 2)

  ;; Index a structure definition.
  (prove:is (evlis '((define x (cons 1 2)) (1 x))) 2))

(prove:finalize)
