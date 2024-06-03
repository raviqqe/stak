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

      (do ((vec (make-vector 5))
           (i 0 (+ i 1)))
        ((= i 5) vec)
        (vector-set! vec i i))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "#(0 1 2 3 4)"
