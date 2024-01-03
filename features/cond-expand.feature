Feature: cond-expand
  Scenario: Expand an `else` clause
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (cond-expand
      (else
        (write-u8 65)))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Rule: `and`
    Scenario: Expand no requirement
      Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (cond-expand
        ((and)
          (write-u8 65)))
      """
      When I successfully run `scheme main.scm`
      Then the stdout should contain exactly "A"

    Scenario: Expand a requirement
      Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (cond-expand
        ((and r7rs)
          (write-u8 65)))
      """
      When I successfully run `scheme main.scm`
      Then the stdout should contain exactly "A"

    Scenario: Expand two requirements
      Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (cond-expand
        ((and r7rs r7rs)
          (write-u8 65)))
      """
      When I successfully run `scheme main.scm`
      Then the stdout should contain exactly "A"

  Rule: `or`
    Scenario: Expand no requirement
      Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (cond-expand
        ((or)
          (write-u8 65))
        (else
          (write-u8 66)))
      """
      When I successfully run `scheme main.scm`
      Then the stdout should contain exactly "B"

    Scenario: Expand a requirement
      Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (cond-expand
        ((or r7rs)
          (write-u8 65)))
      """
      When I successfully run `scheme main.scm`
      Then the stdout should contain exactly "A"

    Scenario: Expand two requirements
      Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (cond-expand
        ((or foo r7rs)
          (write-u8 65)))
      """
      When I successfully run `scheme main.scm`
      Then the stdout should contain exactly "A"
