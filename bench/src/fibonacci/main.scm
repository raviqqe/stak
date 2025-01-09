(import (scheme base))

(define (fibonacci x)
  (if (< x 2)
    x
    (+
      (fibonacci (- x 1))
      (fibonacci (- x 2)))))

(fibonacci 32)
