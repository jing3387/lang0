(in-package #:satori-test)

(plan 1)

;; Constant definition.
(is (satori:evlis '((define x 0) x)) 0)

(finalize)
