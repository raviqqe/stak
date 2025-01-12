(import (scheme base))

(define (sum x)
  (let loop ((x x) (y 0))
    ; TODO Use `=`.
    (if (eq? x 0)
      y
      (loop (- x 1) (+ x y)))))

(sum 10000000)
