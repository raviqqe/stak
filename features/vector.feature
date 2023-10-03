Feature: Vector
  Scenario: Make a vector
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (make-vector 42)
    """
    When I successfully run `scheme main.scm`
    Then the exit status should be 0

  Scenario: Make a vector with a fill value
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (make-vector 42 #t)
    """
    When I successfully run `scheme main.scm`
    Then the exit status should be 0

  Scenario: Convert a vector to a list
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (map write-u8 (vector->list #(65 66 67)))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "ABC"

  Scenario Outline: Get a length of a vector
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (write-u8 (if (= (vector-length <value>) 3) 65 66))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | value              |
      | (make-vector 3)    |
      | (make-vector 3 #t) |
      | (vector 1 2 3)     |

  Scenario Outline: Get an element in a vector
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (write-u8 (vector-ref <vector> <index>))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | vector            | index | output |
      | (vector 65)       | 0     | A      |
      | (vector 65 66)    | 0     | A      |
      | (vector 65 66)    | 1     | B      |
      | (vector 65 66 67) | 0     | A      |
      | (vector 65 66 67) | 1     | B      |
      | (vector 65 66 67) | 2     | C      |
