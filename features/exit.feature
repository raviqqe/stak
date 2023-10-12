Feature: Exit
  Scenario: Exit an interpreter
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (exit)

    (write-u8 65)
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly ""
