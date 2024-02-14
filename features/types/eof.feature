Feature: EOF object
  Scenario: Check if a value is an EOF object
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (eof-object? (eof-object)) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"
