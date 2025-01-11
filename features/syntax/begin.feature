Feature: begin
  Scenario: Use a `begin` expression
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (begin 65))
      (write-u8 (begin 65 66))
      (write-u8 (begin 65 66 67))
      """
    When I successfully run `stak  main.scm`
    Then the stdout should contain exactly "ABC"

  Scenario: Use a `begin` expression with no value
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (begin))
      """
    When I run `stak  main.scm`
    Then the exit status should not be 0
