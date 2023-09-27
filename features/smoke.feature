Feature: Smoke
  Scenario: Initialize constants in a correct order
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define a #\A)

    (for-each
      (lambda (x) (write-u8 (if (not x) 65 66)))
      '(#\A #\B))

    (define b #\B)
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "BB"
