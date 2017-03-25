(in-package :satori)

(defun run-tests ()
  (setf prove:*enable-colors* nil
        prove:*default-reporter* :dot)

  (expression-test)
  (definition-test)
  (cons-test)
  (cast-test))
