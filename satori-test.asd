(asdf:defsystem #:satori-test
  :depends-on (#:satori #:prove)
  :defsystem-depends-on (#:prove-asdf)
  :pathname "t/"
  :serial t
  :components ((:file "package")
               (:test-file "expression")
               (:test-file "definition"))
  :perform (test-op :after (op c)
                               (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                               (asdf:clear-system c)))
