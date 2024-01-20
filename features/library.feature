Feature: Library system
  @chibi @gauche @guile
  Scenario: Read a byte
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
