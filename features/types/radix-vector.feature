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
      | length | fill | output                                               |
      | 0      |      | ()                                                   |
      | 1      |      | (#f)                                                 |
      | 1      | 42   | (42)                                                 |
      | 2      | 42   | (42 42)                                              |
      | 3      | 42   | (42 42 42)                                           |
      | 8      | 42   | (42 42 42 42 42 42 42 42)                            |
      | 9      | 42   | (42 42 42 42 42 42 42 42 42)                         |
      | 10     | 42   | (42 42 42 42 42 42 42 42 42 42)                      |
      | 17     | 42   | (42 42 42 42 42 42 42 42 42 42 42 42 42 42 42 42 42) |

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
      | 3      | 0     |
      | 3      | 1     |
      | 3      | 2     |
      | 8      | 0     |
      | 8      | 1     |
      | 8      | 6     |
      | 8      | 7     |
      | 9      | 0     |
      | 9      | 7     |
      | 9      | 8     |

  Scenario Outline: Convert between a list and a vector
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme write) (srfi 1) (stak radix-vector))

      (define xs (iota <count>))

      (write (equal? (radix-vector->list (list->radix-vector xs)) xs))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "#t"

    Examples:
      | count |
      | 0     |
      | 1     |
      | 2     |
      | 3     |
