Feature: Library system
  @chibi @gauche @guile
  Scenario: Import a function
    Given a file named "main.scm" with:
    """scheme
    (define-library (foo)
      (export foo)
      (import (scheme base))

      (begin
        (define (foo x)
          (write-u8 x))))

    (import (foo))

    (foo 65)
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  @chibi @gauche @guile
  Scenario: Import functions
    Given a file named "main.scm" with:
    """scheme
    (define-library (foo)
      (export foo)
      (import (scheme base))

      (begin
        (define (foo x)
          (write-u8 x)))

        (define (bar x)
          (write-u8 (+ x 1))))

    (import (foo bar))

    (foo 65)
    (bar 65)
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "AB"
