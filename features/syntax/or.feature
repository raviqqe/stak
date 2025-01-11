Feature: or
  Scenario Outline: Use an `or` operator
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (or <values>) 65 66))
      """
    When I successfully run `stak  main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | values | output |
      |        | B      |
      | #f     | B      |
      | #t     | A      |
      | #f #f  | B      |
      | #f #t  | A      |

  Scenario Outline: Return the first true value
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (or <values>))
      """
    When I successfully run `stak  main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | values |
      | 65     |
      | 65 #f  |
      | #f 65  |
