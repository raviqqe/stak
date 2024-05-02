Feature: Eval
  Scenario: Import an `eval` library
    Given a file named "main.scm" with:
      """scheme
      (import (scheme eval))
      """
    When I successfully run `scheme main.scm`
    Then the exit status should be 0

  Scenario: Evaluate a number
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme eval))

      (write-u8 (eval 65 (environment)))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Evaluate a string
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme eval))

      (write-string (eval "foo" (environment)))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "foo"
