(import (scheme base))

(define (sum x)
  (let loop ((x x) (y 0))
    (if (= x 0)
      y
      (loop (- x 1) (+ x y)))))

(sum 10000000)
