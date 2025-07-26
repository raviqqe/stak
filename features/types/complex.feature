@float
Feature: Complex number

  Scenario Outline: Make a complex number
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme inexact) (scheme complex))

      (write-u8 (if (= (real-part <expression>) <value>) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | expression               | value        |
      | (make-rectangular -42 0) | -42          |
      | (make-rectangular -1 0)  | -1           |
      | (make-rectangular 0 0)   | 0            |
      | (make-rectangular 1 0)   | 1            |
      | (make-rectangular 42 0)  | 42           |
      | (make-polar -1 -1)       | (- (cos -1)) |
      | (make-polar -1 1)        | (- (cos 1))  |
      | (make-polar 1 -1)        | (cos -1)     |
      | (make-polar 1 1)         | (cos 1)      |

  Scenario Outline: Calculate a magnitude
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme inexact) (scheme complex))

      (write-u8 (if (= (magnitude <input>) <output>) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | input | output |
      | -2    | 2      |
      | -1    | 1      |
      | 1     | 1      |
      | 2     | 2      |

  Scenario Outline: Calculate an angle
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme inexact) (scheme complex))

      (write-u8 (if (= (angle <input>) <output>) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | input | output    |
      | -2    | (acos -1) |
      | -1    | (acos -1) |
      | 1     | 0         |
      | 2     | 0         |
