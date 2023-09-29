Feature: Read
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
