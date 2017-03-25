(asdf:defsystem #:satori
  :description "Satori is a statically-typed, functional Lisp running on LLVM"
  :author "Jarrod Jeffrey Ingram <jarrod.jeffi@gmail.com>"
  :license "BSD-3-Clause"
  :depends-on (:alexandria :llvm :prove)
  :serial t
  :components
  ((:module "src"
            :serial t
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
            :serial t
            :depends-on ("src")
            :components
            ((:file "util")
             (:file "expression")
             (:file "definition")
             (:file "cons")
             (:file "cast")
             (:file "test")))))
