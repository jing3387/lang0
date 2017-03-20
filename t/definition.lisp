(in-package #:satori-test)

(plan 2)

;; Constant definition.
(is (satori:evlis* '((define x 0) x)) 0)

;; Lambda definition.
(is (satori:evlis* '((define x (lambda (x) x)) (x 0))) 0)

(finalize)
