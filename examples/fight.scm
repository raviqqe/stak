; Import a base library and the library named `(stak rust)` for Rust integration.
(import (scheme base) (stak rust))

; Use the `define-rust` procedure to import native functions written in Rust.
; The order of the functions should match the ones passed into the `Engine::new()`
; function in Rust.
(define-rust
  make-person
  person-pies
  person-wasted
  person-throw-pie)

; Make two people with a number of pies they have and their dodge rates.
(define me (make-person 4 0.2))
(define friend (make-person 2 0.6))

; The fight begins. Let's throw pies to each other!
(do ()
  ((or
      (person-wasted me)
      (person-wasted friend)
      (and
        (zero? (person-pies me))
        (zero? (person-pies friend)))))
  (person-throw-pie me friend)
  (person-throw-pie friend me))

; Output the winner.
(write-string
  (cond
    ((person-wasted friend)
      "You won!")
    ((person-wasted me)
      "You lost...")
    (else
      "Draw...")))
