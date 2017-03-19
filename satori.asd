(asdf:defsystem #:satori
  :description "Describe satori here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:alexandria #:llvm #:prove)
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "satori")
               (:file "utils")
               (:file "closure")
               (:file "compile")
               (:file "types")
               (:file "substitute")))
