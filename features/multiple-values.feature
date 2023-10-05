Feature: Multiple values
  Scenario: Return multiple values to a continuation
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (write-u8
      (call-with-values
        (lambda () (values 1 4 60))
        (lambda (x y z) (+ x y z))))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"
