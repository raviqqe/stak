Feature: case
  Scenario: Evaluate the first clause
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8
        (case 2
          ((1 2 3)
            65)
          (else
            66)))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Evaluate the second clause
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8
        (case 5
          ((1 2 3)
            65)
          ((4 5 6)
            66)
          (else
            67)))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "B"

  Scenario: Evaluate an `else` clause
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8
        (case 7
          ((1 2 3)
            65)
          ((4 5 6)
            66)
          (else
            67)))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "C"
