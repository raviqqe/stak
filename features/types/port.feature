Feature: Port
  Scenario Outline: Check if a value is a port
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (port? <expression>) 65 66))
      """
    When I successfully run `stak  main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | expression            |
      | (current-input-port)  |
      | (current-output-port) |

  Scenario: Check if a value is an input port
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (input-port? (current-input-port)) 65 66))
      """
    When I successfully run `stak  main.scm`
    Then the stdout should contain exactly "A"

  Scenario Outline: Check if a value is an output port
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (output-port? <expression>) 65 66))
      """
    When I successfully run `stak  main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | expression            |
      | (current-output-port) |
      | (current-error-port)  |
