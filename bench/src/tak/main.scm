(import (scheme base))

(define (tak x y z)
  (if (< y x)
    (tak
      (tak (- x 1) y z)
      (tak (- y 1) z x)
      (tak (- z 1) x y))
    z))

(tak 32 16 8)
