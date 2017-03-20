(defpackage #:satori-test
  (:use #:cl #:prove))

(in-package #:satori-test)

(setf prove:*default-reporter* :dot
      prove:*enable-colors* nil)
