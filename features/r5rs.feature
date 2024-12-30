Feature: The R5RS library
  Scenario: Force a promise
    Given a file named "main.scm" with:
      """scheme
      (import (scheme r5rs))

      (write-u8 (force (delay 65)))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"
