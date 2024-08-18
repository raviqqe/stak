Feature: Number
  Scenario Outline: Check if a value is a number
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (number? <value>) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | value | output |
      | #f    | B      |
      | '()   | B      |
      | 0     | A      |
      | 42    | A      |
      | -2045 | A      |

  Scenario Outline: Use literals
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define x <value>)
      """
    When I successfully run `scheme main.scm`
    Then the exit status should be 0

    Examples:
      | value |
      | 0     |
      | 1     |
      | 42    |
      | -1    |
      | -42   |

  Scenario: Use a negative integer
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (+ 66 -1))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Use large (but not big) integers
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (- 1065 1000))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario Outline: Use integers around the encoding base
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (- <value> 60))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | value | output |
      | 127   | C      |
      | 128   | D      |
      | 129   | E      |

  Scenario Outline: Use arithmetic operators
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (= <expression> <value>) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | expression | value |
      | (+)        | 0     |
      | (+ 1)      | 1     |
      | (+ 1 2)    | 3     |
      | (- 1)      | -1    |
      | (- 0 1)    | -1    |
      | (- 0 1 2)  | -3    |
      | (*)        | 1     |
      | (* 2)      | 2     |
      | (* 2 3)    | 6     |
      | (/ 6 2)    | 3     |
      | (/ 6 2 3)  | 1     |

  Scenario Outline: Use division operators
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (= <expression> <value>) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | expression                 | value |
      | (quotient 6 2)             | 3     |
      | (quotient 6 3)             | 2     |
      | (quotient -6 2)            | -3    |
      | (quotient -6 -2)           | 3     |
      | (truncate-quotient 6 2)    | 3     |
      | (truncate-quotient 6 3)    | 2     |
      | (truncate-quotient -6 2)   | -3    |
      | (truncate-quotient -6 -2)  | 3     |
      | (remainder 8 3)            | 2     |
      | (remainder 8 -3)           | 2     |
      | (remainder -8 3)           | -2    |
      | (remainder -8 -3)          | -2    |
      | (truncate-remainder 8 3)   | 2     |
      | (truncate-remainder 8 -3)  | 2     |
      | (truncate-remainder -8 3)  | -2    |
      | (truncate-remainder -8 -3) | -2    |
      | (modulo 5 1)               | 0     |
      | (modulo 5 2)               | 1     |
      | (modulo 5 3)               | 2     |
      | (modulo -5 2)              | 1     |
      | (modulo -5 -2)             | -1    |
      | (floor-remainder 5 1)      | 0     |
      | (floor-remainder 5 2)      | 1     |
      | (floor-remainder 5 3)      | 2     |
      | (floor-remainder -5 2)     | 1     |
      | (floor-remainder -5 -2)    | -1    |

  Scenario: Calculate a multiplicative inverse
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define (test x y)
        (write-u8 (if (= x y) 65 66)))

      (test (/ 2) (/ 1 2))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario Outline: Use a comparison operator
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if <expression> 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | expression |
      | (< 0 1)    |
      | (< 0 1 2)  |
      | (> 1 0)    |
      | (<= 0 1)   |
      | (<= 0 0)   |
      | (>= 1 0)   |
      | (>= 0 0)   |

  @guile @stak
  Scenario Outline: Use comparison operators with an insufficient number of arguments
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (< <values>) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | values |
      |        |
      | 0      |

  Scenario Outline: Convert a number to a string
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-string (number->string <value> <radix>))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | value | radix | output |
      | 0     |       | 0      |
      | 1     |       | 1      |
      | 2     |       | 2      |
      | 42    |       | 42     |
      | -1    |       | -1     |
      | -2    |       | -2     |
      | -42   |       | -42    |
      | 0     | 16    | 0      |
      | 1     | 16    | 1      |
      | 2     | 16    | 2      |
      | 15    | 16    | f      |
      | 42    | 16    | 2a     |
      | -1    | 16    | -1     |
      | -2    | 16    | -2     |
      | -42   | 16    | -2a    |
      | 0     | 32    | 0      |
      | 1     | 32    | 1      |
      | 2     | 32    | 2      |
      | 31    | 32    | v      |
      | 42    | 32    | 1a     |
      | -1    | 32    | -1     |
      | -2    | 32    | -2     |
      | -42   | 32    | -1a    |

  Scenario Outline: Convert a floating point number to a string
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-string (number->string <value>))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | value | output |
      | 0.5   | 0.5    |
      | 0.125 | 0.125  |
      | 3.14  | 3.14   |
      | -3.14 | -3.14  |

  @gauche @guile @stak
  Scenario Outline: Convert a string to a number
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-string (number->string (string->number "<value>" <radix>)))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | value | radix | output |
      | 0     |       | 0      |
      | 1     |       | 1      |
      | 2     |       | 2      |
      | 42    |       | 42     |
      | -1    |       | -1     |
      | -2    |       | -2     |
      | -42   |       | -42    |
      | 0     | 16    | 0      |
      | 1     | 16    | 1      |
      | 2     | 16    | 2      |
      | f     | 16    | 15     |
      | 2a    | 16    | 42     |
      | 2A    | 16    | 42     |
      | -1    | 16    | -1     |
      | -2    | 16    | -2     |
      | -2a   | 16    | -42    |
      | -2A   | 16    | -42    |
      | 0     | 32    | 0      |
      | 1     | 32    | 1      |
      | 2     | 32    | 2      |
      | v     | 32    | 31     |
      | 1a    | 32    | 42     |
      | 1A    | 32    | 42     |
      | -1    | 32    | -1     |
      | -2    | 32    | -2     |
      | -1a   | 32    | -42    |
      | -1A   | 32    | -42    |

  Scenario Outline: Convert a string to a floating point number
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-string (number->string (string->number "<value>")))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | value | output |
      | 0.5   | 0.5    |
      | 0.125 | 0.125  |
      | 3.14  | 3.14   |
      | -3.14 | -3.14  |

  Scenario Outline: Convert an invalid string to a number
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (string->number "<value>") 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "B"

    Examples:
      | value |
      |       |
      | x     |
      | foo   |
      | -     |
      | 2x    |
      | -x    |
      | -2x   |
