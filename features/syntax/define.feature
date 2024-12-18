Feature: define
  Scenario: Define a recursive procedure
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define (sum x)
        (if (eq? x 0) 0 (+ x (sum (- x 1)))))

      (write-u8 (sum 11))
      """
    When I successfully run `stak  main.scm`
    Then the stdout should contain exactly "B"

  Scenario: Use a local variable in a definition
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define (f x)
        (let ((y x))
          (define z y)
          z))

      (write-u8 (f 65))
      """
    When I successfully run `stak  main.scm`
    Then the stdout should contain exactly "A"
