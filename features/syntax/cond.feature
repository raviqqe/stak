Feature: cond
  Scenario: Evaluate the first clause
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8
        (cond
          (#t
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
        (cond
          (#f
            65)
          (#t
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
        (cond
          (#f
            65)
          (#f
            66)
          (else
            67)))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "C"
