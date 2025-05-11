Feature: case-lambda
  Scenario: Evaluate the first clause
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme case-lambda))

      (define foo
        (case-lambda
          ((x) x)
          ((x y) (+ x y))))

      (write-u8 (foo 65))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"
