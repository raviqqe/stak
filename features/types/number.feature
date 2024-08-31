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

  Scenario Outline: Check a number class of integers
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (<predicate> <value>) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | predicate | value | output |
      | number?   | 0     | A      |
      | number?   | 1     | A      |
      | number?   | -42   | A      |
      | complex?  | 0     | A      |
      | complex?  | 1     | A      |
      | complex?  | -42   | A      |
      | real?     | 0     | A      |
      | real?     | 1     | A      |
      | real?     | -42   | A      |
      | rational? | 0     | A      |
      | rational? | 1     | A      |
      | rational? | -42   | A      |
      | integer?  | 0     | A      |
      | integer?  | 1     | A      |
      | integer?  | -42   | A      |

  @float
  Scenario Outline: Check a number class of floating point numbers
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme inexact))

      (write-u8 (if (<predicate> <value>) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | predicate | value    | output |
      | number?   | 3.14     | A      |
      | number?   | -42.2045 | A      |
      | complex?  | 3.14     | A      |
      | complex?  | -42.2045 | A      |
      | real?     | 3.14     | A      |
      | real?     | -42.2045 | A      |
      | rational? | 0        | A      |
      | rational? | 1        | A      |
      | rational? | -42      | A      |
      | rational? | 3.14     | A      |
      | rational? | -42.2045 | A      |
      | rational? | (exp 1)  | A      |
      | integer?  | 3.14     | B      |
      | integer?  | -42.2045 | B      |

  Scenario Outline: Check a number class of non-numbers
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (<predicate> #f) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "B"

    Examples:
      | predicate |
      | number?   |
      | complex?  |
      | real?     |
      | rational? |
      | integer?  |

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
      | (modulo 8 3)               | 2     |
      | (modulo 8 -3)              | -1    |
      | (modulo -8 3)              | 1     |
      | (modulo -8 -3)             | -2    |
      | (floor-remainder 5 1)      | 0     |
      | (floor-remainder 5 2)      | 1     |
      | (floor-remainder 5 3)      | 2     |
      | (floor-remainder -5 2)     | 1     |
      | (floor-remainder -5 -2)    | -1    |

  Scenario: Calculate a multiplicative inverse
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (= (/ 2) (/ 1 2)) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  @float
  Scenario: Calculate an exponentiation
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme inexact))

      (write-u8 (if (< (abs (- (expt 2 3) (exp (* (log 2) 3)))) 0.000001) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  @float
  Scenario: Calculate a logarithm
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme inexact))

      (write-u8 (if (< (abs (- (log 2 3) (/ (log 2) (log 3)))) 0.000001) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  @float
  Scenario Outline: Use a truncate function
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme inexact))

      (write-u8 (if (= (truncate <input>) <output>) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | input | output |
      | 0     | 0      |
      | 0.1   | 0      |
      | 0.9   | 0      |
      | 1     | 1      |
      | 1.1   | 1      |
      | 1.9   | 1      |
      | 2     | 2      |
      | -0.9  | 0      |
      | -1    | -1     |
      | -1.9  | -1     |
      | -2    | -2     |
      | -2.1  | -2     |

  @float
  Scenario Outline: Use a floor function
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme inexact))

      (write-u8 (if (= (floor <input>) <output>) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | input | output |
      | 0     | 0      |
      | 0.1   | 0      |
      | 0.9   | 0      |
      | 1     | 1      |
      | 1.1   | 1      |
      | 1.9   | 1      |
      | 2     | 2      |
      | -0.9  | -1     |
      | -1    | -1     |
      | -1.9  | -2     |
      | -2    | -2     |
      | -2.1  | -3     |

  @float
  Scenario Outline: Use a ceiling function
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme inexact))

      (write-u8 (if (= (ceiling <input>) <output>) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | input | output |
      | 0     | 0      |
      | 0.1   | 1      |
      | 0.9   | 1      |
      | 1     | 1      |
      | 1.1   | 2      |
      | 1.9   | 2      |
      | 2     | 2      |
      | -0.9  | 0      |
      | -1    | -1     |
      | -1.9  | -1     |
      | -2    | -2     |
      | -2.1  | -2     |

  @float
  Scenario Outline: Use a round function
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme inexact))

      (write-u8 (if (= (round <input>) <output>) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | input | output |
      | 0     | 0      |
      | 0.1   | 0      |
      | 0.49  | 0      |
      | 0.5   | 0      |
      | 0.9   | 1      |
      | 1     | 1      |
      | 1.1   | 1      |
      | 1.49  | 1      |
      | 1.5   | 2      |
      | 1.9   | 2      |
      | 2     | 2      |
      | 2.5   | 2      |
      | 3.5   | 4      |
      | -1    | -1     |
      | -0.9  | -1     |
      | -1.5  | -2     |
      | -1.51 | -2     |
      | -1.9  | -2     |

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

  @float
  Scenario Outline: Convert a floating point number to a string
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-string (number->string <value>))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<value>"

    Examples:
      | value |
      | 0.5   |
      | 0.125 |
      | 1.2   |
      | 3.14  |
      | -3.14 |

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

  @float
  Scenario Outline: Convert a string to a floating point number
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-string (number->string (string->number "<value>")))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<value>"

    Examples:
      | value |
      | 0.5   |
      | 0.125 |
      | 1.2   |
      | 3.14  |
      | -3.14 |

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
