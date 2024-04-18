Feature: Lazy
  Scenario: Delay evaluation of an expression
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme lazy))

      (delay (write-u8 65))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly ""

  Scenario Outline: Check if a value is a promise
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme lazy))

      (write-u8 (if (promise? <value>) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | value      | output |
      | #f         | B      |
      | (delay #f) | A      |

  Scenario: Force a promise
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme lazy))

      (force (delay (write-u8 65)))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"
