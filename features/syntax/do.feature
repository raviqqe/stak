Feature: do
  Scenario: Use a `do` syntax with steps
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define x ’(1 3 5 7 9))

      (do ((x x (cdr x))
           (sum 0 (+ sum (car x))))
        ((null? x) sum)))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "25"

  Scenario: Use a `do` syntax without a step
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (and <values>) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<output>"
