Feature: Quote
  Scenario Outline: Quote a scalar value
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (eq? <value> '<value>) 65 66))
      """
    When I successfully run `stak  main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | value |
      | #f    |
      | #t    |
      | 0     |
      | 1     |
      | 2     |
      | 42    |
      | -1    |
      | -2    |
      | -42   |

  Scenario: Quote a list
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (for-each write-u8 '(65 66 67))
      """
    When I successfully run `stak  main.scm`
    Then the stdout should contain exactly "ABC"
