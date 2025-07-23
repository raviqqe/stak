Feature: Bytevector
  Scenario: Write a bytevector
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-bytevector #u8(65 66 67))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "ABC"

  Scenario Outline: Get a length of a bytevector
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (= (bytevector-length <value>) <length>) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | value      | length |
      | #u8()      | 0      |
      | #u8(0)     | 1      |
      | #u8(0 0)   | 2      |
      | #u8(0 0 0) | 3      |

  Scenario Outline: Reference a value in a bytevector
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (bytevector-u8-ref <vector> <index>))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | vector        | index |
      | #u8(65)       | 0     |
      | #u8(65 66)    | 0     |
      | #u8(66 65)    | 1     |
      | #u8(65 66 66) | 0     |
      | #u8(66 65 66) | 1     |
      | #u8(66 66 65) | 2     |

  Scenario Outline: Set a value in a bytevector
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define xs (bytevector <values>))

      (bytevector-u8-set! xs <index> <value>)

      (write-u8 (if (equal? xs #u8(<result>)) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | values | index | value | result |
      | 0      | 0     | 1     | 1      |
      | 0 1    | 0     | 2     | 2 1    |
      | 0 1    | 1     | 2     | 0 2    |
      | 0 1 2  | 0     | 3     | 3 1 2  |
      | 0 1 2  | 1     | 3     | 0 3 2  |
      | 0 1 2  | 2     | 3     | 0 1 3  |

  Scenario Outline: Append bytevectors
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (srfi 1))

      (define xs (bytevector-append <values>))

      (for-each
        (lambda (index)
          (write-u8 (bytevector-u8-ref xs index)))
        (iota (bytevector-length xs)))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | values                           | output |
      | #u8()                            |        |
      | #u8() #u8()                      |        |
      | #u8(65)                          | A      |
      | #u8(65) #u8(66)                  | AB     |
      | #u8(65) #u8(66) #u8(67)          | ABC    |
      | #u8(65) #u8(66 67) #u8(68 69 70) | ABCDEF |

  Scenario Outline: Copy a bytevector
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (srfi 1))

      (define xs (bytevector-copy <value>))

      (for-each
        (lambda (index)
          (write-u8 (bytevector-u8-ref xs index)))
        (iota (bytevector-length xs)))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | value         | output |
      | #u8()         |        |
      | #u8(65)       | A      |
      | #u8(65 66)    | AB     |
      | #u8(65 66 67) | ABC    |

  Scenario Outline: Copy a bytevector in place
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (srfi 1))

      (define xs (bytevector <values>))

      (bytevector-copy! xs <arguments>)

      (for-each
        (lambda (index)
          (write-u8 (+ 65 (bytevector-u8-ref xs index))))
        (iota (bytevector-length xs)))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "<output>"
    # spell-checker: disable

    Examples:
      | values    | arguments          | output |
      |           | 0 #u8()            |        |
      | 0 1 2     | 0 #u8(3 4 5)       | DEF    |
      | 0 1 2     | 1 #u8(3 4)         | ADE    |
      | 0 1 2     | 2 #u8(3)           | ABD    |
      | 0 1 2 3 4 | 1 #u8(5 6 7)       | AFGHE  |
      | 0 1 2 3   | 1 #u8(4 5 6 7) 1   | AFGH   |
      | 0 1 2 3   | 1 #u8(4 5 6 7) 1 3 | AFGD   |
