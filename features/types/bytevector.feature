Feature: Bytevector
  Scenario: Write a bytevector
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-bytevector #u8(65 66 67))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "ABC"

  Scenario Outline: Get a length of a bytevector
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (= (bytevector-length <value>) <length>) 65 66))
      """
    When I successfully run `scheme main.scm`
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
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | vector        | index |
      | #u8(65)       | 0     |
      | #u8(65 66)    | 0     |
      | #u8(66 65)    | 1     |
      | #u8(65 66 66) | 0     |
      | #u8(66 65 66) | 1     |
      | #u8(66 66 65) | 2     |
