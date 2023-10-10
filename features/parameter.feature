Feature: Parameter
  Scenario: Make a parameter
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (write-u8 ((make-parameter 65)))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"
