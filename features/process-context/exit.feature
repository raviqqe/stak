Feature: Exit
  Scenario: Exit an interpreter
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base) (scheme process-context))

    (exit)

    (write-u8 65)
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly ""

  Scenario: Exit an interpreter with a true value
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base) (scheme process-context))

    (exit #t)
    """
    When I successfully run `scheme main.scm`
    Then the exit status should be 0

  Scenario: Exit an interpreter with a false value
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base) (scheme process-context))

    (exit #f)
    """
    When I run `scheme main.scm`
    Then the exit status should not be 0
