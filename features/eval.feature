Feature: Eval
  @self
  Scenario: Import an `eval` library
    Given a file named "main.scm" with:
      """scheme
      (import (scheme eval))
      """
    When I successfully run `scheme main.scm`
    Then the exit status should be 0

  @self
  Scenario: Import an `eval` library
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme eval))

      (write-u8 (eval '42))
      """
    When I successfully run `scheme main.scm`
    Then the exit status should be 0
