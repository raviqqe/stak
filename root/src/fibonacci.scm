(import
  (scheme base)
  (scheme read)
  (scheme write))

(define (fibonacci x)
  (if (< x 2)
    x
    (+
      (fibonacci (- x 1))
      (fibonacci (- x 2)))))

(write (fibonacci (read)))
