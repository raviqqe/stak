Feature: SRFI 1

  Scenario Outline: Enumerate numbers
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (srfi 1))

      (write-u8 (if (equal? (iota <arguments>) '(<elements>)) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | arguments | elements |
      | 0         |          |
      | 1         | 0        |
      | 2         | 0 1      |
      | 3         | 0 1 2    |
      | 1 1       | 1        |
      | 1 2       | 2        |
      | 3 5       | 5 6 7    |
      | 3 5 2     | 5 7 9    |

  Scenario Outline: Delete duplicates in a list
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (srfi 1))

      (write-u8 (if (equal? (delete-duplicates '(<input>)) '(<output>)) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | input         | output |
      |               |        |
      | 1             | 1      |
      | 1 1           | 1      |
      | 1 1 2 2       | 1 2    |
      | 1 2 1 2       | 1 2    |
      | 1 2 1 3 3 2 3 | 1 2 3  |
