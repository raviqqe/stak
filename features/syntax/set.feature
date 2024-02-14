Feature: set!
  Scenario: Set a global variable
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define x #f)
      (set! x 65)
      (write-u8 x)
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Set a local variable
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (let ((x #f))
        (set! x 65)
        (write-u8 x))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"
