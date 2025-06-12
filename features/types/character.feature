Feature: Character
  Scenario Outline: Check if a value is a character
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (char? <expression>) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | expression         |
      | #\\A               |
      | #\\newline         |
      | (integer->char 65) |

  Scenario Outline: Check a character category
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme char))

      (write-u8 (if (<predicate> #\<value>) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | predicate        | value   | output |
      | char-alphabetic? | a       | A      |
      | char-alphabetic? | A       | A      |
      | char-alphabetic? | z       | A      |
      | char-alphabetic? | Z       | A      |
      | char-alphabetic? | 0       | B      |
      | char-numeric?    | 0       | A      |
      | char-numeric?    | 9       | A      |
      | char-numeric?    | A       | B      |
      | char-whitespace? | newline | A      |
      | char-whitespace? | return  | A      |
      | char-whitespace? | space   | A      |
      | char-whitespace? | tab     | A      |
      | char-whitespace? | A       | B      |

  Scenario: Write a character
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-char #\A)
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Write a newline character
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-char #\A)
      (newline)
      (write-char #\B)
      """
    When I successfully run `stak main.scm`
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
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | predicate | characters     | output |
      | char=?    | #\\A #\\A      | A      |
      | char=?    | #\\A #\\B      | B      |
      | char=?    | #\\A #\\A #\\A | A      |
      | char=?    | #\\A #\\A #\\B | B      |
      | char<?    | #\\A #\\B      | A      |
      | char<?    | #\\A #\\A      | B      |
      | char<?    | #\\B #\\A      | B      |
      | char<?    | #\\A #\\B #\\C | A      |
      | char<?    | #\\A #\\B #\\B | B      |
      | char<=?   | #\\A #\\B      | A      |
      | char<=?   | #\\A #\\A      | A      |
      | char<=?   | #\\B #\\A      | B      |
      | char<=?   | #\\A #\\B #\\C | A      |
      | char<=?   | #\\A #\\B #\\B | A      |

  Scenario Outline: Compare case-insensitive characters
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme char))

      (write-u8 (if (<predicate> <characters>) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | predicate | characters | output |
      | char-ci=? | #\\A #\\A  | A      |
      | char-ci=? | #\\a #\\A  | A      |
      | char-ci=? | #\\A #\\B  | B      |

  Scenario Outline: Convert a character case
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme char))

      (write-u8 (if (eqv? (<predicate> <input>) <output>) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | predicate     | input | output |
      | char-downcase | #\\A  | #\\a   |
      | char-downcase | #\\a  | #\\a   |
      | char-upcase   | #\\a  | #\\A   |
      | char-upcase   | #\\A  | #\\A   |
