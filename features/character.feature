Feature: Character
  Scenario Outline: Check if a value is a character
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (write-u8 (if (char? <expression>) 65 66))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | expression         |
      | #\\A               |
      | #\\newline         |
      | (integer->char 65) |

  Scenario Outline: Check if a value is a whitespace
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (write-u8 (if (char? <value>) 65 66))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | value      |
      | #\\newline |
      | #\\return  |
      | #\\space   |
      | #\\tab     |

  Scenario: Write a character
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (write-char #\A)
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Write a newline character
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (write-char #\A)
    (newline)
    (write-char #\B)
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly:
    """
    A
    B
    """
