#!r6rs

#|

   The FACT procedure computes the factorial

   of a non-negative integer.

|#

(define fact

  (lambda (n)

    ;; base case

    (if (= n 0)

        #;(= n 1)

        1       ; identity of *

        (* n (fact (- n 1))))))
