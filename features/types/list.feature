Feature: List
  Scenario Outline: Use literals
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define x '<value>)
      """
    When I successfully run `scheme main.scm`
    Then the exit status should be 0

    Examples:
      | value               |
      | ()                  |
      | (1)                 |
      | (1 2)               |
      | (1 2 3)             |
      | ((1) (2 2) (3 3 3)) |

  Scenario: Create a pair
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (cons 42 '())
      """
    When I successfully run `scheme main.scm`
    Then the exit status should be 0

  Scenario: Create a pair with a non-cons cdr
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (cons 1 2)
      """
    When I successfully run `scheme main.scm`
    Then the exit status should be 0

  Scenario: Create a list
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (list 1 2 3)
      """
    When I successfully run `scheme main.scm`
    Then the exit status should be 0

  Scenario: Use a `for-each` procedure
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (for-each
        (lambda (x) (write-u8 (+ 60 x)))
        '(5 6 7))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "ABC"

  Scenario: Use a `for-each` procedure with two lists
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (for-each
        (lambda (x y) (write-u8 (+ x y)))
        '(65 66 67)
        '(1 2 3))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "BDF"

  Scenario: Use a `for-each` procedure with three lists
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (for-each
        (lambda (x y z) (write-u8 (+ x y z)))
        '(65 66 67)
        '(1 2 3)
        '(4 5 6))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "FIL"

  Scenario: Use a `map` procedure
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (for-each
        write-u8
        (map
          (lambda (x) (+ 60 x))
          '(5 6 7)))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "ABC"

  Scenario: Use a `map` procedure with two lists
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (for-each
        write-u8
        (map
          (lambda (x y) (+ x y))
          '(65 66 67)
          '(1 2 3)))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "BDF"

  Scenario: Use a `map` procedure with three lists
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (for-each
        write-u8
        (map
          (lambda (x y z) (+ x y z))
          '(65 66 67)
          '(1 2 3)
          '(4 5 6)))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "FIL"

  Scenario: Use a `map` procedure with uneven lists
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (for-each
        write-u8
        (map
          (lambda (x y) (+ x y))
          '(65 66 67 68)
          '(1 2 3)))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "BDF"

  Scenario Outline: Use an `append` procedure
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (for-each write-u8 (append <values>))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | values            | output |
      |                   |        |
      | '(65)             | A      |
      | '(65) '(66)       | AB     |
      | '(65) '(66) '(67) | ABC    |
      | '(65 66) '(67 68) | ABCD   |

  Scenario: Share the last argument in an `append` procedure
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define x (list 65))
      (define y (append '(65) x))

      (for-each write-u8 y)

      (set-car! x 66)

      (for-each write-u8 y)
      """
    When I successfully run `scheme main.scm`
    # spell-checker: disable-next-line
    Then the stdout should contain exactly "AAAB"

  Scenario Outline: Use a `memq` procedure
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (memq <value> '(<values>)) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | value | values | output |
      | 1     |        | B      |
      | 1     | 1      | A      |
      | 2     | 1      | B      |
      | 1     | 1 2    | A      |
      | 2     | 1 2    | A      |
      | 3     | 1 2    | B      |
      | 1     | 1 2 3  | A      |
      | 4     | 1 2 3  | B      |

  Scenario Outline: Use a `memv` procedure
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (memv <value> '(<values>)) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | value | values         | output |
      | #\\A  |                | B      |
      | #\\A  | #\\A           | A      |
      | #\\B  | #\\A           | B      |
      | #\\A  | #\\A #\\B      | A      |
      | #\\B  | #\\A #\\B      | A      |
      | #\\C  | #\\A #\\B      | B      |
      | #\\A  | #\\A #\\B #\\C | A      |
      | #\\D  | #\\A #\\B #\\C | B      |

  Scenario Outline: Use a `member` procedure
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (member <value> '(<values>)) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | value | values      | output |
      | '(1)  |             | B      |
      | '(1)  | (1)         | A      |
      | '(2)  | (1)         | B      |
      | '(1)  | (1) (2)     | A      |
      | '(2)  | (1) (2)     | A      |
      | '(3)  | (1) (2)     | B      |
      | '(1)  | (1) (2) (3) | A      |
      | '(4)  | (1) (2) (3) | B      |

  Scenario Outline: Get a value from an association list
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (cdr (<procedure> 42 '((1 . 1) (42 . 65) (3 . 3)))))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | procedure |
      | assq      |
      | assv      |
      | assoc     |

  Scenario Outline: Get a value from an association list of characters
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (cdr (<procedure> #\B '((#\A . 1) (#\B . 65) (#\C . 3)))))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | procedure |
      | assq      |
      | assv      |
      | assoc     |

  Scenario Outline: Check if a value is a pair
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (pair? <value>) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | value      | output |
      | #f         | B      |
      | '()        | B      |
      | '(1)       | A      |
      | '(1 2)     | A      |
      | (cons 1 2) | A      |

  Scenario Outline: Check if a value is null
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (null? <value>) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | value      | output |
      | #f         | B      |
      | '()        | A      |
      | '(1)       | B      |
      | '(1 2)     | B      |
      | (cons 1 2) | B      |

  Scenario Outline: Check if a value is a list
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (list? <value>) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | value      | output |
      | #f         | B      |
      | '()        | A      |
      | '(1)       | A      |
      | '(1 2)     | A      |
      | (cons 1 2) | B      |

  Scenario Outline: Apply a cxr procedure
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme cxr))

      (write-u8 (<procedure> '<value>))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | procedure | value           |
      | car       | (65)            |
      | cdr       | (66 . 65)       |
      | caar      | ((65))          |
      | cadr      | (66 65)         |
      | cdar      | ((66 . 65))     |
      | cddr      | (66 66 . 65)    |
      | caaar     | (((65)))        |
      | caadr     | (66 (65))       |
      | cadar     | ((66 . (65)))   |
      | caddr     | (66 66 65)      |
      | cdaar     | (((66 . 65)))   |
      | cdadr     | (66 (66 . 65))  |
      | cddar     | ((66 66 . 65))  |
      | cdddr     | (66 66 66 . 65) |
      | cadddr    | (66 66 66 65)   |

  Scenario Outline: Copy a list
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (equal? (list-copy <value>) <value>) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | value      |
      | #f         |
      | '()        |
      | '(1)       |
      | '(1 . 2)   |
      | '(1 2)     |
      | '(1 2 . 3) |
