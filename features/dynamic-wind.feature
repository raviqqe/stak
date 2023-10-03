Feature: Dynamic wind
  Scenario: Run callbacks
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (dynamic-wind
      (lambda () (write-u8 65))
      (lambda () (write-u8 66))
      (lambda () (write-u8 67)))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "ABC"
