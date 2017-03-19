(in-package #:satori-test)

(plan 10)

;; Constant maps to a constant.
(is (satori:eval 0) 0)

;; The identity function.
(is (satori:eval '((lambda (x) x) 0)) 0)

;; Multiple arguments
(is (satori:eval '((lambda (x y) x) 0 1)) 0)
(is (satori:eval '((lambda (x y) y) 0 1)) 1)

;; Pass the identity function as an argument. This involves the unification of
;; environment variables for the function variable and function argument.
(is (satori:eval '((lambda (f x) (f x)) (lambda (x) x) 0)) 0)

;; Empty `let' expression.
(is (satori:eval '(let () 0)) 0)

;; Basic `let' expression.
(is (satori:eval '(let ((x 0)) x)) 0)

;; Sequential `let' expression.
(is (satori:eval '(let ((x 0) (y x)) y)) 0)

;; Bind the identity function in a `let' expression. This is interesting because
;; the `lambda' is substituted into the body of the `let' expression leaving
;; empty bindings that have to be handled.
(is (satori:eval '((lambda (x) (let ((f (lambda (x) x))) (f x))) 0)) 0)

;; Bind the identity function and a variable leaving just a variable in the
;; `let' expression after the identity function is substituted into the body.
(is (satori:eval '((lambda (x) (let ((f (lambda (x) x)) (y x)) (f y))) 0)) 0)

(finalize)
