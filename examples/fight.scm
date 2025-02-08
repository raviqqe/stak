(import (scheme base) (stak rust))

(define-rust
  make-person
  person-throw-pie
  person-wasted)

(define me (make-person 4 0.2))
(define friend (make-person 2 0.6))

(person-throw-pie me friend)
(person-throw-pie friend me)
(person-throw-pie me friend)
(person-throw-pie friend me)

(write-string
  (cond
    ((person-wasted friend)
      "Congrats!")
    ((person-wasted me)
       "Oh, no!")
    (else
      "Draw..."))))
