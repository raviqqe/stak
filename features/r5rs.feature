Feature: The R5RS library
  Scenario: Add numbers
    Given a file named "main.scm" with:
      """scheme
      (import (scheme r5rs))

      (write-u8 (+ 60 5))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Read an S-expression
    Given a file named "main.scm" with:
      """scheme
      (import (scheme r5rs))

      (write (read))
      """
    And a file named "input.txt" with:
      """text
      (1 2 3)
      """
    When I run `scheme main.scm` interactively
    And I pipe in the file "input.txt"
    Then the exit status should be 0
    And the stdout should contain exactly "(1 2 3)"

  Scenario: Write an S-expression
    Given a file named "main.scm" with:
      """scheme
      (import (scheme r5rs))

      (write '(1 2 3))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "(1 2 3)"

  Scenario: Force a promise
    Given a file named "main.scm" with:
      """scheme
      (import (scheme r5rs))

      (write-u8 (force (delay 65)))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Evaluate an S-expression
    Given a file named "main.scm" with:
      """scheme
      (import (scheme r5rs))

      (write-u8
        (eval
          '(+ 60 5)
          (environment '(scheme base))))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"
