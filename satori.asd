(asdf:defsystem #:satori
  :description "Describe satori here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:alexandria #:llvm)
  :serial t
  :components ((:file "package")
               (:file "satori")
               (:file "utils")
               (:file "transform")
               (:file "compile")
               (:file "types")))
