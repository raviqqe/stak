(import (scheme base) (stak rust))

(define-rust
  make-person
  person-throw-pie
  person-wasted)

(define me (make-person 4 0.2))
(define you (make-person 2 0.6))

(person-throw-pie me you)
(person-throw-pie you me)
(person-throw-pie me you)
(person-throw-pie you me)

(when (person-wasted me)
  (write-string "Oh, no!"))
