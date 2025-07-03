Feature: SRFI 1
  Scenario Outline: Enumerate numbers
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (srfi 1))

      (write-u8 (if (equal? <lhs> <rhs>) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | lhs      | rhs      |
      | (iota 0) | '()      |
      | (iota 1) | '(0)     |
      | (iota 2) | '(0 1)   |
      | (iota 3) | '(0 1 2) |
