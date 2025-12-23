(import (scheme base) (srfi 1))

(define count 4096)

(define xs #())

(define xs
  (do ((index 0 (+ index 1))
       (xs #() (vector-append xs index)))
    ((= index count)
      xs)))

(do ((index 0 (+ index 1)))
  ((= index count))
  (write (vector-ref index)))
