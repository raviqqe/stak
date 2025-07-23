@stak
Feature: Stak base library
  Scenario: Raise an error
    Given a file named "main.scm" with:
      """scheme
      (import (stak base))

      (when (procedure? error)
        (error "foo"))
      """
    When I run `stak main.scm`
    Then the exit status should not be 0

  Scenario: Raise an error with an I/O library
    Given a file named "main.scm" with:
      """scheme
      (import (stak base) (stak io))

      (when (procedure? error)
        (error "foo"))
      """
    When I run `stak main.scm`
    Then the exit status should not be 0
    And the stderr should contain "foo"
