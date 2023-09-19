(import (scheme base))

(define (sum* x y)
  (if (eqv? x 0)
    y
    (sum* (- x 1) (+ x y))))

(define (sum x)
  (sum* x 0))

(sum 100000000)
