(import (scheme base) (scheme eval) (scheme write))

(define sum
  (eval
    '(lambda (x)
      (let loop ((x x) (y 0))
       (if (zero? x)
        y
        (loop (- x 1) (+ x y)))))
    '((scheme base))))

(write (sum 10000000))
