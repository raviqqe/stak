@niche
Feature: Niche functionalities

  Scenario: Dump a stack trace on an error
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme write) (stak backtrace))

      (define (foo)
        (error "my error" 42)
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
    And the stderr should contain "backtrace: error -> foo -> bar -> baz"
