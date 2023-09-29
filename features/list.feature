Feature: List
  Scenario: Use literals
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define x '())
    (define y '(1 2 3))
    (define z '((1) (2 2) (3 3 3)))
    """
    When I successfully run `scheme main.scm`
    Then the exit status should be 0

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

  Scenario: Use a map function
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (map write-u8 '(65 66 67))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "ABC"

  Scenario Outline: Use an append procedure
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (map write-u8 (append <values>))
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

  Scenario: Use a memq function
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (write-u8 (if (memq 2 '(1 2 3)) 65 66))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Use a memv function
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (write-u8 (if (memv 2 '(1 2 3)) 65 66))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  @stak
  Scenario: Get a tag of a pair with a non-cons cdr
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (rib-tag (cons 1 2))
    """
    When I successfully run `scheme main.scm`
    Then the exit status should be 0

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
