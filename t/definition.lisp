(in-package #:satori)

(prove:plan 2)

;; Constant definition.
(prove:is (satori:evlis '((define x 0) x)) 0)

;; Function definition, note that the `x' in the body isn't substituted with the
;; lambda.
(prove:is (satori:evlis '((define x (lambda (x) x)) (x 0))) 0)

(prove:finalize)
