Feature: Boolean
  Scenario Outline: Check if a value is a boolean
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (boolean? <value>) 65 66))
      """
    When I successfully run `stak  main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | value | output |
      | #f    | A      |
      | #t    | A      |
      | '()   | B      |

  Scenario Outline: Use a `not` operator
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (not <value>) 65 66))
      """
    When I successfully run `stak  main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | value | output |
      | #f    | A      |
      | #t    | B      |
