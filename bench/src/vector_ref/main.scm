(import (scheme base) (scheme write))

(define count 4096)

(define xs
  (let ((xs (make-vector count)))
    (do ((index 0 (+ index 1)))
      ((= index count))
      (vector-set! xs index index))
    xs))

(do ((index 0 (+ index 1)))
  ((= index count))
  (write (vector-ref xs index))
  (newline))
