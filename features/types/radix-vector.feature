@stak
Feature: Radix vector

  Scenario Outline: Make a vector
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme write) (stak radix-vector))

      (write (radix-vector->list (make-radix-vector <length> <fill>)))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | length | fill | output |
      | 0      |      | ()     |
      | 1      |      | (#f)   |
      | 1      | 42   | (42)   |

  Scenario Outline: Reference an element
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme write) (stak radix-vector))

      (write (radix-vector-ref (make-radix-vector <length> 42) <index>))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "42"

    Examples:
      | length | index |
      | 1      | 0     |
      | 2      | 0     |
      | 2      | 1     |
