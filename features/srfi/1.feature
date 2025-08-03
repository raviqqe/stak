Feature: SRFI 1

  Scenario Outline: Map a function on a list and append elements
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (srfi 1))

      (write-u8
        (if (equal?
             (append-map (lambda (x) (list (* 2 x) (* x x))) '(<input>))
             '(<output>))
          65
          66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | input | output      |
      |       |             |
      | 1     | 2 1         |
      | 1 2   | 2 1 4 4     |
      | 1 2 3 | 2 1 4 4 6 9 |

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

  Scenario Outline: Filter a list
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (srfi 1))

      (write-u8 (if (equal? (filter odd? '(<input>)) '(<output>)) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | input       | output |
      |             |        |
      | 1           | 1      |
      | 2           |        |
      | 1 2         | 1      |
      | 1 2 3       | 1 3    |
      | 1 2 3 4     | 1 3    |
      | 1 2 3 4 5 6 | 1 3 5  |

  Scenario Outline: Reduce a list
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (srfi 1))

      (write-u8 (if (equal? (fold + 0 '(<elements>)) <output>) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | elements | output |
      |          | 0      |
      | 1        | 1      |
      | 1 2      | 3      |
      | 1 2 3    | 6      |

  Scenario Outline: Fold a list from right
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (srfi 1))

      (write-u8 (if (equal? (fold-right + 0 '(<elements>)) <output>) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | elements | output |
      |          | 0      |
      | 1        | 1      |
      | 1 2      | 3      |
      | 1 2 3    | 6      |

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

  Scenario Outline: Calculate an index of an element
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (srfi 1))

      (write-u8 (if (equal? (list-index <predicate> <lists>) <index>) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | predicate | lists    | index |
      | even?     | '()      | #f    |
      | even?     | '(1)     | #f    |
      | even?     | '(1 3)   | #f    |
      | even?     | '(2)     | 0     |
      | even?     | '(1 2)   | 1     |
      | even?     | '(1 2 3) | 1     |
      | even?     | '(1 3 2) | 2     |
      | even?     | '(1 3 2) | 2     |

  Scenario Outline: Reduce numbers
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (srfi 1))

      (write-u8 (if (equal? (reduce + 42 '(<elements>)) <output>) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | elements | output |
      |          | 42     |
      | 0        | 0      |
      | 1        | 1      |
      | 1 2      | 3      |
      | 1 2 3    | 6      |

  @gauche @guile @stak
  Scenario Outline: Reduce numbers from right
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (srfi 1))

      (write-u8 (if (equal? (reduce-right + 42 '(<elements>)) <output>) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | elements | output |
      |          | 42     |
      | 0        | 0      |
      | 1        | 1      |
      | 1 2      | 3      |
      | 1 2 3    | 6      |
