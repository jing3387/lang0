(in-package #:satori)

(prove:plan 2)

;; Constant definition.
(prove:is (satori:evlis '((define x 0) x)) 0)

;; Lambda definition.
(prove:is (satori:evlis '((define x (lambda (x) x)) (x 0))) 0)

(prove:finalize)
