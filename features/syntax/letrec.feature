Feature: letrec
  Scenario: Bind a variable
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define (f x)
        (letrec (
            (f
              (lambda (x)
                (if (eqv? x 65)
                  x
                  (f (+ x 1))))))
          (f x)))

      (write-u8 (f 0))
      """
    When I successfully run `stak  main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Bind two variables
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define (f x)
        (letrec (
            (f (lambda (x) (if (eqv? x 65) x (g (+ x 1)))))
            (g (lambda (x) (f x))))
          (f x)))

      (write-u8 (f 0))
      """
    When I successfully run `stak  main.scm`
    Then the stdout should contain exactly "A"
