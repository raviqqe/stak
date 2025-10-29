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

  Scenario Outline: Check a character property
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme char))

      (write-u8 (if (<predicate> #\<value>) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | predicate        | value | output |
      | char-alphabetic? | a     | A      |
      | char-alphabetic? | A     | A      |
      | char-alphabetic? | z     | A      |
      | char-alphabetic? | Z     | A      |
      | char-alphabetic? | 0     | B      |
      | char-numeric?    | 0     | A      |
      | char-numeric?    | 9     | A      |
      | char-numeric?    | A     | B      |

  Scenario Outline: Check a lower case character
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme char))

      (write-u8 (if (char-lower-case? #\<value>) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | value | output |
      | a     | A      |
      | A     | B      |
      | z     | A      |
      | Z     | B      |
      | 0     | B      |
      | 9     | B      |
      | @     | B      |

  Scenario Outline: Check a lower case character
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme char))

      (write-u8 (if (char-upper-case? #\<value>) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | value | output |
      | a     | B      |
      | A     | A      |
      | z     | B      |
      | Z     | A      |
      | 0     | B      |
      | 9     | B      |
      | @     | B      |

  Scenario Outline: Check a whitespace character
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme char))

      (write-u8 (if (char-whitespace? <expression>) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | expression            | output |
      | #\\newline            | A      |
      | #\\return             | A      |
      | #\\space              | A      |
      | #\\tab                | A      |
      | #\\@                  | B      |
      | #\\A                  | B      |
      | (integer->char 12288) | A      |

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

  Scenario Outline: Convert a character to its lower case
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme char))

      (write-u8 (if (eqv? (char-downcase <input>) <output>) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | input  | output |
      | #\\A   | #\\a   |
      | #\\a   | #\\a   |
      | #\\Α   | #\\α   |
      | #\\ẞ   | #\\ß   |

  Scenario Outline: Convert a character to its upper case
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme char))

      (write-u8 (if (eqv? (char-upcase <input>) <output>) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | input | output |
      | #\\a  | #\\A   |
      | #\\A  | #\\A   |
      | #\\α  | #\\Α   |

    @gauche @guile @stak
    Examples:
      | input | output |
      | #\\ß  | #\\ß   |

  Scenario Outline: Fold a character
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme char))

      (write-u8 (if (equal? (char-foldcase <input>) <output>) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | input  | output |
      | #\\@   | #\\@   |
      | #\\A   | #\\a   |
      | #\\Z   | #\\z   |
      | #\\[   | #\\[   |
      | #\\`   | #\\`   |
      | #\\a   | #\\a   |
      | #\\z   | #\\z   |
      | #\\{   | #\\{   |
      | #\\À   | #\\à   |
      | #\\Ý   | #\\ý   |
      | #\\Α   | #\\α   |
      | #\\ß   | #\\ß   |
      | #\\Ꟶ   | #\\ꟶ   |

    @guile @stak
    Examples:
      | input   | output  |
      | #\\𞤀    | #\\𞤢    |
      | #\\𞤡    | #\\𞥃    |

  Scenario Outline: Extract a digit value
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme char))

      (write-u8 (if (= (digit-value #\<character>) <output>) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | character | output |
      | 0         | 0      |
      | 1         | 1      |
      | 6         | 6      |
      | 9         | 9      |
