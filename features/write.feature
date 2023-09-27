Feature: Write
  Scenario: Write a byte
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (write-u8 65)
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Write a character
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (write-char #\A)
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Write an escaped character
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base) (scheme write))

    (write #\A)
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "#\A"

  @stak
  Scenario: Write an escaped special character
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base) (scheme write))

    (write #\newline)
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "#\newline"

  Scenario: Write a string
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (write-string "Hello, world!")
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "Hello, world!"

  Scenario: Write a special character in a string
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (write-string "foo\nbar")
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly:
    """
    foo
    bar
    """

  Scenario Outline: Write a boolean
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base) (scheme write))

    (write <value>)
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<value>"

    Examples:
      | value |
      | #f    |
      | #t    |

  Scenario Outline: Write a list
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base) (scheme write))

    (write '<value>)
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<value>"

    Examples:
      | value             |
      | ()                |
      | (1)               |
      | (1 2)             |
      | (1 2 3)           |
      | (1 (1 2) (3 4 5)) |
      | (1 . 2)           |
      | (1 2 . 3 )        |

  Scenario Outline: Write a number
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base) (scheme write))

    (write <value>)
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<value>"

    Examples:
      | value |
      | 0     |
      | 1     |
      | 42    |
      | -1    |
      | -42   |

  @stak
  Scenario: Write a procedure
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base) (scheme write))

    (write (lambda () #f))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "#procedure"

  Scenario Outline: Write a string
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base) (scheme write))

    (write "<value>")
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "\"<value>\""

    Examples:
      | value         |
      |               |
      | foo           |
      | Hello, world! |

  Scenario: Write a symbol
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base) (scheme write))

    (write 'foo)
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "foo"

  Scenario Outline: Write a vector
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base) (scheme write))

    (write <value>)
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<value>"

    Examples:
      | value                |
      | #()                  |
      | #(1)                 |
      | #(1 2)               |
      | #(1 2 3)             |
      | #(1 #(1 2) #(3 4 5)) |
