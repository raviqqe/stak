@float
Feature: Complex number
  Scenario Outline: Make a complex number
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme inexact) (scheme complex))

      (write-u8 (if (= <expression> <value>) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | expression               | value    |
      | (make-rectangular -42 0) | -42      |
      | (make-rectangular -1 0)  | -1       |
      | (make-rectangular 0 0)   | 0        |
      | (make-rectangular 1 0)   | 1        |
      | (make-rectangular 42 0)  | 42       |
      | (make-polar -1 -1)       | (cos -1) |
      | (make-polar 1 -1)        | -1       |
      | (make-polar 1 -1)        | 1        |
      | (make-polar 1 1)         | 1        |
