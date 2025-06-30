@float
Feature: Floating-point number
  Scenario Outline: Check a number class of floating point numbers
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme inexact))

      (write-u8 (if (<predicate> <value>) 65 66))
      """
    When I successfully run `stak main.scm`
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

  Scenario: Calculate an exponentiation
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme inexact))

      (write-u8 (if (< (abs (- (expt 2 3) (exp (* (log 2) 3)))) 0.000001) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Calculate a logarithm
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme inexact))

      (write-u8 (if (< (abs (- (log 2 3) (/ (log 2) (log 3)))) 0.000001) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

  Scenario Outline: Truncate a number
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme inexact))

      (write-u8 (if (= (truncate <input>) <output>) 65 66))
      """
    When I successfully run `stak main.scm`
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

  Scenario Outline: Calculate a floor
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme inexact))

      (write-u8 (if (= (floor <input>) <output>) 65 66))
      """
    When I successfully run `stak main.scm`
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

  Scenario Outline: Calculate a ceiling
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme inexact))

      (write-u8 (if (= (ceiling <input>) <output>) 65 66))
      """
    When I successfully run `stak main.scm`
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

  Scenario Outline: Round a number
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme inexact))

      (write-u8 (if (= (round <input>) <output>) 65 66))
      """
    When I successfully run `stak main.scm`
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

  Scenario Outline: Convert a floating point number to a string
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-string (number->string <value>))
      """
    When I successfully run `stak main.scm`
    # TODO Use an `exactly` adverb.
    Then the stdout should contain "<value>"

    Examples:
      | value |
      | 0.5   |
      | 0.125 |
      | 1.2   |
      | 3.14  |
      | -3.14 |

  Scenario Outline: Convert a string to a floating point number
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-string (number->string (string->number "<value>")))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "<value>"

    Examples:
      | value |
      | 0.5   |
      | 0.125 |
      | 1.2   |
      | 3.14  |
      | -3.14 |

  Scenario Outline: Use trigonometric functions
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme inexact))

      (write-u8
        (if (< (abs (- (/ (sin <value>) (cos <value>)) (tan <value>))) 0.001)
          65
          66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | value |
      | -2    |
      | -1    |
      | 0     |
      | 1     |
      | 2     |

  Scenario Outline: Use inverse trigonometric functions
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme complex) (scheme inexact))

      (write-u8
        (if (= (round (real-part (<normal> (<inverse> 1)))) 1)
          65
          66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | normal | inverse |
      | cos    | acos    |
      | sin    | asin    |
      | tan    | atan    |
