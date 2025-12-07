@niche
Feature: Niche functionalities

  Background:
    Given I run the following script:
      """sh
      cp $STAK_ROOT/prelude.scm .
      """
    And the exit status should be 0

  Scenario: Show an unknown irritant of an error
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (error "foo" 42)
      """
    When I run the following script:
      """sh
      cat prelude.scm main.scm | stak-compile > main.bc
      """
    And I run `stak-interpret main.bc`
    Then the exit status should be 1
    And the stderr should contain "foo"
    And the stderr should contain "unknown"
