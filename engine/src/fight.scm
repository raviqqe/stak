(import (stak base))
(import (scheme base))

(define make-person (primitive 1000))
(define person-throw-pie (primitive 1001))
(define person-wasted (primitive 1002))

(define me (make-person 4 0.2))
(define you (make-person 2 0.6))

(person-throw-pie me you)
(person-throw-pie you me)
(person-throw-pie me you)
(person-throw-pie you me)

(display "Me: ")
(write (person-wasted me))
(newline)

(display "You ")
(write (person-wasted you))
(newline)
