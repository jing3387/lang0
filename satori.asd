(in-package #:cl-user)
(defpackage #:satori-asd
  (:use #:cl #:asdf))
(in-package #:satori-asd)

(asdf:defsystem #:satori
  :description "Yet another Lisp on LLVM"
  :author "Jarrod Jeffrey Ingram <jarrod.jeffi@gmail.com>"
  :license "BSD-3-Clause"
  :depends-on (#:alexandria #:llvm)
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "satori")
               (:file "util")
               (:file "eval")
               (:file "closure")
               (:file "compile")
               (:file "type")
               (:file "substitute"))
  :in-order-to ((test-op (test-op satori-test))))

(asdf:defsystem #:satori-test
  :depends-on (#:satori #:prove)
  :pathname "t/"
  :serial t
  :components ((:file "package")
               (:file "expression")
               (:file "definition"))
  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
