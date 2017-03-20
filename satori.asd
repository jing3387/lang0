(asdf:defsystem #:satori
  :description "Yet another Lisp on LLVM"
  :author "Jarrod Jeffrey Ingram <jarrod.jeffi@gmail.com>"
  :license "BSD-3-Clause"
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
            ((:file "util")
             (:file "expression")
             (:file "definition")))))
