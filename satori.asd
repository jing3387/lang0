(asdf:defsystem #:satori
  :description "Yet another Lisp on LLVM"
  :author "Jarrod Jeffrey Ingram <jarrod.jeffi@gmail.com>"
  :license "BSD-3-Clause"
  :defsystem-depends-on (:prove-asdf)
  :depends-on (:alexandria :llvm :prove)
  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "util")
             (:file "substitute")
             (:file "generic")
             (:file "type")
             (:file "closure")
             (:file "compile")
             (:file "eval")
             (:file "satori")))
   (:module "t"
            :depends-on ("src")
            :components
            ((:test-file "util")
             (:test-file "expression")
             (:test-file "definition")))))
