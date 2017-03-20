(defpackage #:satori-test
  (:use #:cl #:prove))

(setf prove:*default-reporter* :dot
      prove:*enable-colors* nil)
