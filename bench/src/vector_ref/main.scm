(import (scheme base))

(define count 2048)

(define xs (make-vector count))

(do ((index 0 (+ index 1)))
  ((= index count))
  (vector-set! xs index index))

(do ((index 0 (+ index 1)))
  ((= index count))
  (vector-ref xs index))
