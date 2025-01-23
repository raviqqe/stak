(import (stak base))
(import (scheme base))

(define make-person (primitive 1000))
(define throw-pie (primitive 1001))

(define me (make-person 4 0.2))
(define you (make-person 2 0.6))

(throw-pie me you)
(throw-pie you you)
(throw-pie me you)
(throw-pie you you)
