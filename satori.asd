(asdf:defsystem #:satori
  :description "Yet another Lisp on LLVM"
  :author "Jarrod Jeffrey Ingram <jarrod.jeffi@gmail.com>"
  :license "BSD-3-Clause"
  :depends-on (:alexandria :llvm :prove)
  :pathname "src/"
  :serial t
  :defsystem-depends-on (:prove-asdf)
  :components ((:file "package")
               (:file "util")
               (:file "substitute")
               (:file "generic")
               (:file "type")
               (:file "closure")
               (:file "compile")
               (:file "eval")
               (:file "satori")
               (:test-file "t")))
