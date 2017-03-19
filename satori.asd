(asdf:defsystem #:satori
  :description "Describe satori here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:alexandria #:llvm)
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "satori")
               (:file "utils")
               (:file "closure")
               (:file "compile")
               (:file "types")
               (:file "substitute")))

(asdf:defsystem #:satori-test
  :description "Test suite for the Satori project"
  :depends-on (#:satori #:prove)
  :serial t
  :pathname "t/"
  :components ((:file "package")
               (:file "test")))
