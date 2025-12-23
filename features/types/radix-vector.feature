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
      | 9      | 1     |
      | 9      | 7     |
      | 9      | 8     |
      | 64     | 63    |
      | 65     | 64    |
      | 512    | 511   |
      | 513    | 512   |
      | 4096   | 4095  |
      | 4097   | 4096  |

  Scenario Outline: Convert values between a list and a vector
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
      | 4     |
      | 5     |
      | 6     |
      | 7     |
      | 8     |
      | 9     |
      | 16    |
      | 17    |
      | 32    |
      | 33    |
      | 64    |
      | 65    |
      | 128   |
      | 129   |
      | 512   |
      | 513   |
      | 4096  |
      | 4097  |

  Scenario Outline: Append vectors
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme write) (stak radix-vector))

      (write
        (equal?
          (radix-vector->list
            (radix-vector-append (radix-vector <lhs>) (radix-vector <rhs>)))
          '(<values>)))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "#t"

    Examples:
      | lhs   | rhs   | values      |
      |       |       |             |
      | 1     | 2     | 1 2         |
      | 1     | 2 3   | 1 2 3       |
      | 1 2   | 3     | 1 2 3       |
      | 1 2 3 | 4 5 6 | 1 2 3 4 5 6 |

  Scenario Outline: Map a function over a vector
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme write) (stak radix-vector))

      (write
        (equal?
          (radix-vector-map
            (lambda (x) (* x x))
            (radix-vector <values>))
          (radix-vector <result>)))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "#t"

    Examples:
      | values | result |
      |        |        |
      | 1      | 1      |
      | 2      | 4      |
      | 1 2 3  | 1 4 9  |

  Scenario Outline: Iterate over a vector
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme write) (stak radix-vector))

      (radix-vector-for-each
        (lambda (x)
          (write x)
          (write-char #\space))
        (radix-vector <values>))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain "<values>"

    Examples:
      | values |
      |        |
      | 1      |
      | 2      |
      | 1 2 3  |
