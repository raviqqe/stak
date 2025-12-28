@stak
Feature: Optimizer

  Scenario: Match a rule
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme write) (stak base))

      (define-optimizer foo
        (syntax-rules ()
          ((foo)
            (write 42))))

      (foo)
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "42"

  Scenario: Match a placeholder
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme write) (stak base))

      (define-optimizer foo
        (syntax-rules ()
          ((foo x)
            (write x))))

      (foo 42)
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "42"
