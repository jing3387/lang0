(in-package :satori)

(defun cast-test ()
  (prove:plan 2)

  ;; Cast an `if' expression that returns a union then return an integer.
  (prove:is (evlis '((cast
                      (if (eq 0 1)
                          (cons)
                          (cons 1 2))
                      lst
                      ((() 1)
                       ((* *) 2)))))
            2)

  ;; Cast an `if' expression that returns a union then try and access the bound
  ;; variable.
  (prove:is (evlis '((cast
                      (if (eq 0 1)
                          (cons)
                          (cons 1 2))
                      lst
                      ((() 0)
                       ((* *) (nth 1 lst))))))
            2)

  (prove:finalize))
