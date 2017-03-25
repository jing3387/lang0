(in-package :satori)

(defun cons-test ()
  (prove:plan 10)

  ;; Index a structure.
  (prove:is (evlis '((nth 0 (cons 0 1)))) 0)

  ;; Index a nested structure.
  (prove:is (evlis '((nth 1 (nth 1 (cons 0 (cons 1 2)))))) 2)

  ;; Index a structure bound to a `let' variable then returned.
  (prove:is (evlis '((nth 1 (let ((x (cons 1 2))) x)))) 2)

  ;; Index a structure created inside a lambda.
  (prove:is (evlis '((nth 0 ((lambda (x) (cons x 2)) 1)))) 1)

  ;; Index a structure passed as an argument then returned.
  (prove:is (evlis '((nth 1 ((lambda (x) x) (cons 1 2))))) 2)

  ;; Index a structure definition.
  (prove:is (evlis '((define x (cons 1 2)) (nth 1 x))) 2)

  ;; Get the arity of a simple cons.
  (prove:is (evlis '((arity (cons 1 2)))) 2)

  ;; Get the arity of a cons passed as argument.
  (prove:is (evlis '(((lambda (x) (arity x)) (cons 1 2)))) 2)

  ;; Get the arity of a cons bound to a local variable.
  (prove:is (evlis '((let ((x (cons 1 2))) (arity x)))) 2)

  ;; Get the arity of a cons definition.
  (prove:is (evlis '((define x (cons 1 2)) (arity x))) 2)

  (prove:finalize))
