Feature: Vector
  Scenario: Make a vector
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (make-vector 42)
      """
    When I successfully run `stak main.scm`
    Then the exit status should be 0

  Scenario: Make a vector with a fill value
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (make-vector 42 #t)
      """
    When I successfully run `stak main.scm`
    Then the exit status should be 0

  Scenario: Convert a vector to a list
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (for-each write-u8 (vector->list #(65 66 67)))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "ABC"

  Scenario Outline: Get a length of a vector
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (= (vector-length <value>) <length>) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | value              | length |
      | #()                | 0      |
      | #(1)               | 1      |
      | #(1 2)             | 2      |
      | #(1 2 3)           | 3      |
      | (make-vector 3)    | 3      |
      | (make-vector 3 #t) | 3      |
      | (vector 1 2 3)     | 3      |

  Scenario Outline: Get an element in a vector
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (vector-ref <vector> <index>))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | vector            | index | output |
      | (vector 65)       | 0     | A      |
      | (vector 65 66)    | 0     | A      |
      | (vector 65 66)    | 1     | B      |
      | (vector 65 66 67) | 0     | A      |
      | (vector 65 66 67) | 1     | B      |
      | (vector 65 66 67) | 2     | C      |

  Scenario Outline: Set an element in a vector
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define xs <vector>)

      (vector-set! xs <index> 88)

      (for-each write-u8 (vector->list xs))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | vector            | index | output |
      | (vector 65)       | 0     | X      |
      | (vector 65 66)    | 0     | XB     |
      | (vector 65 66)    | 1     | AX     |
      | (vector 65 66 67) | 0     | XBC    |
      | (vector 65 66 67) | 1     | AXC    |
      | (vector 65 66 67) | 2     | ABX    |

  Scenario Outline: Append vectors
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (for-each write-u8 (vector->list (vector-append <values>)))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | values                     | output |
      | #()                        |        |
      | #() #()                    |        |
      | #(65)                      | A      |
      | #(65) #(66)                | AB     |
      | #(65) #(66) #(67)          | ABC    |
      | #(65) #(66 67) #(68 69 70) | ABCDEF |

  Scenario Outline: Copy a vector
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (for-each write-u8 (vector->list (vector-copy <value>)))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | value       | output |
      | #()         |        |
      | #(65)       | A      |
      | #(65 66)    | AB     |
      | #(65 66 67) | ABC    |

  Scenario Outline: Copy a vector in place
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define xs (vector <values>))

      (vector-copy! xs <arguments>)

      (for-each
        (lambda (x) (write-u8 (+ x 65)))
        (vector->list xs))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "<output>"

    # spell-checker: disable
    Examples:
      | values    | arguments        | output |
      | 0         | 0 #()            | A      |
      | 0 1 2     | 0 #(3 4 5)       | DEF    |
      | 0 1 2     | 1 #(3 4)         | ADE    |
      | 0 1 2     | 2 #(3)           | ABD    |
      | 0 1 2 3 4 | 1 #(5 6 7)       | AFGHE  |
      | 0 1 2 3   | 1 #(4 5 6 7) 1   | AFGH   |
      | 0 1 2 3   | 1 #(4 5 6 7) 1 3 | AFGD   |
