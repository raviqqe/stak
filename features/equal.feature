Feature: Equality
  Scenario Outline: Use an `eq?` procedure
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (eq? <lhs> <rhs>) 65 66))
      """
    When I successfully run `stak  main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | lhs | rhs | output |
      | '() | '() | A      |
      | #f  | #f  | A      |
      | #t  | #t  | A      |
      | '() | #f  | B      |
      | #f  | #t  | B      |
      | #t  | '() | B      |
      | 42  | 42  | A      |
      | 42  | 0   | B      |

  Scenario Outline: Use an `eqv?` procedure
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (eqv? <lhs> <rhs>) 65 66))
      """
    When I successfully run `stak  main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | lhs  | rhs  | output |
      | '()  | '()  | A      |
      | #f   | #f   | A      |
      | #t   | #t   | A      |
      | '()  | #f   | B      |
      | #f   | #t   | B      |
      | #t   | '()  | B      |
      | 42   | 42   | A      |
      | 42   | 0    | B      |
      | #\\A | #\\A | A      |
      | #\\A | #\\B | B      |

  Scenario Outline: Use an `equal?` procedure with scalar values
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (equal? <lhs> <rhs>) 65 66))
      """
    When I successfully run `stak  main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | lhs  | rhs  | output |
      | '()  | '()  | A      |
      | #f   | #f   | A      |
      | #t   | #t   | A      |
      | '()  | #f   | B      |
      | #f   | #t   | B      |
      | #t   | '()  | B      |
      | 42   | 42   | A      |
      | 42   | 0    | B      |
      | #\\A | #\\A | A      |
      | #\\A | #\\B | B      |

  Scenario Outline: Use an `equal?` procedure with collections
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (equal? <lhs> <rhs>) 65 66))
      """
    When I successfully run `stak  main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | lhs      | rhs        | output |
      | '()      | '()        | A      |
      | '(1)     | '(1)       | A      |
      | '(1 2)   | '(1 2)     | A      |
      | '(1 2 3) | '(1 2 3)   | A      |
      | '(1 2 3) | '(1 2 3 4) | B      |
      | #()      | #()        | A      |
      | #(1)     | #(1)       | A      |
      | #(1 2)   | #(1 2)     | A      |
      | #(1 2 3) | #(1 2 3)   | A      |
      | #(1 2 3) | #(1 2 3 4) | B      |
