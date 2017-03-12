;;;; mml.asd

(asdf:defsystem #:mml
  :description "Describe mml here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:llvm)
  :serial t
  :components ((:file "package")
               (:file "mml")))

