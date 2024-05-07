Feature: Character
  Scenario Outline: Check if a value is a character
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (char? <expression>) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | expression         |
      | #\\A               |
      | #\\newline         |
      | (integer->char 65) |

  Scenario Outline: Check if a value is a whitespace
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme char))

      (write-u8 (if (char-whitespace? <value>) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | value      | output |
      | #\\A       | B      |
      | #\\newline | A      |
      | #\\return  | A      |
      | #\\space   | A      |
      | #\\tab     | A      |

  Scenario: Write a character
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-char #\A)
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Write a newline character
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-char #\A)
      (newline)
      (write-char #\B)
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly:
      """
      A
      B
      """

  Scenario Outline: Compare characters
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (<predicate> <characters>) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | predicate | characters     | output |
      | char=?    | #\\A #\\A      | A      |
      | char=?    | #\\A #\\B      | B      |
      | char=?    | #\\A #\\A #\\A | A      |
      | char=?    | #\\A #\\A #\\B | B      |
      | char<?    | #\\A #\\B      | A      |
      | char<?    | #\\A #\\A      | B      |
      | char<?    | #\\A #\\B #\\C | A      |
      | char<?    | #\\A #\\B #\\B | B      |
