(import (scheme base))

(define count 4096)

(define xs (make-vector count))

(do ((index 0 (+ index 1)))
  ((= index count))
  (vector-set! xs index index))

(do ((index 0 (+ index 1)))
  ((= index count))
  (vector-ref xs index))
