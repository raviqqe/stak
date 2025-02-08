(import (scheme base) (stak rust))

(define-rust
  make-person
  person-pies
  person-wasted
  person-throw-pie)

(define me (make-person 4 0.2))
(define friend (make-person 2 0.6))

(do ()
  ((or
      (person-wasted me)
      (person-wasted friend)
      (and
        (zero? (person-pies me))
        (zero? (person-pies friend)))))
  (person-throw-pie me friend)
  (person-throw-pie friend me))

(write-string
  (cond
    ((person-wasted friend)
      "You won!")
    ((person-wasted me)
      "You lost ;(")
    (else
      "Draw...")))
