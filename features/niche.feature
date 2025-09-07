@niche
Feature: Niche functionalities

  Background:
    Given I successfully run `cp $STAK_ROOT/prelude.scm .`

  Scenario: Dump a stack trace on an error
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme write) (stak backtrace))

      (define (foo)
        (error "foo")
        #f)

      (define (bar)
        (foo)
        #f)

      (define (baz)
        (bar)
        #f)

      (let ()
        (baz)
        #f)
      """
    When I run `stak main.scm`
    Then the exit status should be 1
    And the stderr should contain "foo"
    And the stderr should contain "backtrace: error -> foo -> bar -> baz"

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
