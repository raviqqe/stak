Feature: let
  Scenario: Bind a variable
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (let ((x 65)) x))
      """
    When I successfully run `stak  main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Bind two variables
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (let ((x 60) (y 5)) (+ x y)))
      """
    When I successfully run `stak  main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Call a bound procedure
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define (f) 65)

      (define (g)
        (let ((h f))
          (h)))

      (write-u8 (g))
      """
    When I successfully run `stak  main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Cause a side effect in a body
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8
        (let ((x 66))
          (write-u8 65)
          x))
      """
    When I successfully run `stak  main.scm`
    Then the stdout should contain exactly "AB"

  Scenario: Do not corrupt a procedure environment
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define (f)
        (let (
            (g
              (let ((x 65))
                (lambda () x))))
          g))

      (write-u8 ((f)))
      """
    When I successfully run `stak  main.scm`
    Then the stdout should contain exactly "A"
