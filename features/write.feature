Feature: Write
  Scenario: Write a character integer
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

  Scenario: Write a special character in a string
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (write-char #\newline)
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

    (write-string "\n")
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "\"\\n\""
