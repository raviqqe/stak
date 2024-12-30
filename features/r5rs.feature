Feature: The R5RS library
  Scenario: Add numbers
    Given a file named "main.scm" with:
      """scheme
      (import (scheme r5rs))

      (write-u8 (+ 60 5))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Force a promise
    Given a file named "main.scm" with:
      """scheme
      (import (scheme r5rs))

      (write-u8 (force (delay 65)))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"
