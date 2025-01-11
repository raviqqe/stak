Feature: do
  Scenario: Use a `do` syntax with steps
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define xs '(1 2 3 4 5 6 7 8 9 10 11))

      (do ((xs xs (cdr xs))
           (y 0 (+ y (car xs))))
        ((null? xs)
          (write-u8 y)))
      """
    When I successfully run `stak  main.scm`
    Then the stdout should contain exactly "B"

  Scenario: Use a `do` syntax without a step
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define y 0)

      (do ((xs #(1 2 3 4 5 6 7 8 9 10 11))
           (i 0 (+ i 1)))
        ((= i (vector-length xs))
          (write-u8 y))
        (set! y (+ y (vector-ref xs i))))
      """
    When I successfully run `stak  main.scm`
    Then the stdout should contain exactly "B"
