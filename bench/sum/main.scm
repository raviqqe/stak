(define (sum x y)
  (if (eqv? x 0)
    y
    (sum (- x 1) (+ x y))))

(sum 100000000 0)
