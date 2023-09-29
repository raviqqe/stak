Feature: Read
  @stak @chibi @gauche
  Scenario: Read a byte
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (write-u8 (read-u8))
    """
    And a file named "input.txt" with:
    """scheme
    A
    """
    When I run `scheme main.scm` interactively
    And I pipe in the file "input.txt"
    Then the exit status should be 0
    And the stdout should contain exactly "A"

  @stak @chibi @gauche
  Scenario: Read bytes
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (write-u8 (read-u8))
    (write-u8 (read-u8))
    (write-u8 (read-u8))
    """
    And a file named "input.txt" with:
    """scheme
    ABC
    """
    When I run `scheme main.scm` interactively
    And I pipe in the file "input.txt"
    Then the exit status should be 0
    And the stdout should contain exactly "ABC"

  Scenario: Read a character
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (write-char (read-char))
    """
    And a file named "input.txt" with:
    """scheme
    A
    """
    When I run `scheme main.scm` interactively
    And I pipe in the file "input.txt"
    Then the exit status should be 0
    And the stdout should contain exactly "A"

  Scenario: Peek a character
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (write-char (peek-char))
    """
    And a file named "input.txt" with:
    """scheme
    A
    """
    When I run `scheme main.scm` interactively
    And I pipe in the file "input.txt"
    Then the exit status should be 0
    And the stdout should contain exactly "A"

  Scenario: Peek a character multiple times
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (write-char (peek-char))
    (write-char (peek-char))
    """
    And a file named "input.txt" with:
    """scheme
    A
    """
    When I run `scheme main.scm` interactively
    And I pipe in the file "input.txt"
    Then the exit status should be 0
    And the stdout should contain exactly "AA"

  Scenario: Peek and read characters
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (write-char (peek-char))
    (write-char (read-char))
    (write-char (read-char))
    """
    And a file named "input.txt" with:
    """scheme
    AB
    """
    When I run `scheme main.scm` interactively
    And I pipe in the file "input.txt"
    Then the exit status should be 0
    And the stdout should contain exactly "AAB"
