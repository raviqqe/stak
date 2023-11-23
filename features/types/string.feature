Feature: String
  Scenario: Write a string
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (write-string "Hello, world!")
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "Hello, world!"

  Scenario: Convert a string to a list
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (for-each write-char (string->list "Hello, world!"))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "Hello, world!"

  Scenario: Convert a list to a string
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (write-string (list->string (string->list "Hello, world!")))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "Hello, world!"

  Scenario Outline: Append strings
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (write-string (string-append <values>))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | values            | output    |
      |                   |           |
      | ""                |           |
      | "foo"             | foo       |
      | "app" "le"        | apple     |
      | "dis" "cov" "ery" | discovery |

  Scenario Outline: Get a character in a string
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (write-char (string-ref "<string>" <index>))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | string | index | output |
      | a      | 0     | a      |
      | ab     | 0     | a      |
      | ab     | 1     | b      |
      | abc    | 0     | a      |
      | abc    | 1     | b      |
      | abc    | 2     | c      |
