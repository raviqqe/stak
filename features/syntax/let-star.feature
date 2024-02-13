Feature: let*
  Scenario: Bind a variable
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (let* ((x 65)) x))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Bind two variables
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (let* ((x 65) (y x)) y))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Bind three variables
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (let* ((x 65) (y x) (z y)) z))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"
