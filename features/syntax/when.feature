Feature: when
  Scenario: Evaluate a clause
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (when #t (write-u8 65))
      """
    When I successfully run `stak  main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Do not evaluate a clause
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (when #f (write-u8 65))
      """
    When I successfully run `stak  main.scm`
    Then the stdout should contain exactly ""
