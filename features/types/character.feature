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

  Scenario Outline: Check an alphabetic character
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme char))

      (write-u8 (if (char-alphabetic? #\<value>) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | value | output |
      | a     | A      |
      | A     | A      |
      | z     | A      |
      | Z     | A      |
      | À     | A      |
      | Ý     | A      |
      | ß     | A      |
      | ß     | A      |
      | à     | A      |
      | ý     | A      |
      | Α     | A      |
      | α     | A      |
      | あ     | A      |
      | を     | A      |
      | @     | B      |
      | 0     | B      |

  Scenario Outline: Check a numeric character
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme char))

      (write-u8 (if (char-numeric? #\<value>) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | value | output |
      | 0     | A      |
      | 9     | A      |
      | @     | B      |
      | A     | B      |

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
      | 0     | B      |
      | 9     | B      |
      | @     | B      |
      | a     | A      |
      | A     | B      |
      | z     | A      |
      | Z     | B      |
      | ẞ     | B      |
      | À     | B      |
      | Ý     | B      |
      | ß     | A      |
      | à     | A      |
      | ý     | A      |
      | α     | A      |

  Scenario Outline: Check a upper case character
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme char))

      (write-u8 (if (char-upper-case? #\<value>) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | value | output |
      | 0     | B      |
      | 9     | B      |
      | @     | B      |
      | A     | A      |
      | a     | B      |
      | Z     | A      |
      | z     | B      |
      | ẞ     | A      |
      | À     | A      |
      | Ý     | A      |
      | ß     | B      |
      | à     | B      |
      | ý     | B      |
      | α     | B      |

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
      | #\\@   | #\\@   |
      | #\\A   | #\\a   |
      | #\\a   | #\\a   |
      | #\\Z   | #\\z   |
      | #\\z   | #\\z   |
      | #\\ẞ   | #\\ß   |
      | #\\µ   | #\\µ   |
      | #\\Α   | #\\α   |
      | #\\ﬅ   | #\\ﬅ   |

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
      | #\\@  | #\\@   |
      | #\\a  | #\\A   |
      | #\\A  | #\\A   |
      | #\\z  | #\\Z   |
      | #\\Z  | #\\Z   |
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
      | #\\[   | #\\[   |
      | #\\`   | #\\`   |
      | #\\A   | #\\a   |
      | #\\a   | #\\a   |
      | #\\Z   | #\\z   |
      | #\\z   | #\\z   |
      | #\\{   | #\\{   |
      | #\\µ   | #\\μ   |
      | #\\À   | #\\à   |
      | #\\Ý   | #\\ý   |
      | #\\ß   | #\\ß   |
      | #\\Ꟶ   | #\\ꟶ   |
      | #\\Α   | #\\α   |
      | #\\ﬅ   | #\\ﬆ   |

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
