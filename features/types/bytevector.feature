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

  Scenario Outline: Get a length of a bytevector
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

  Scenario Outline: Append bytevectors
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (srfi 1))

      (for-each write-u8 (bytevector->list (bytevector-append <values>)))
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
