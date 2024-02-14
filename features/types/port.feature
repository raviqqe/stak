Feature: Port
  Scenario Outline: Check if a value is a port
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (port? <expression>) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | expression            |
      | (current-input-port)  |
      | (current-output-port) |
