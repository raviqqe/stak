Feature: and
  Scenario Outline: Use an `and` operator
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (and <values>) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | values | output |
      |        | A      |
      | #t     | A      |
      | #f     | B      |
      | #t #t  | A      |
      | #t #f  | B      |
