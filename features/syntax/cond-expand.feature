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

  Scenario: Match an implemented feature
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (cond-expand
        (r7rs
          (write-u8 65)))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Match a missing feature
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (cond-expand
        (foo
          (write-u8 65))
        (else
          (write-u8 66)))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "B"

  Scenario: Match an implemented library
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (cond-expand
        ((library (scheme base))
          (write-u8 65)))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  @stak @chibi @gauche
  Scenario: Match a missing library
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (cond-expand
        ((library (scheme miracle))
          (write-u8 65))
        (else
          (write-u8 66)))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "B"

  Scenario: Expand an empty clause
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (cond-expand (r7rs))
      """
    When I successfully run `scheme main.scm`
    Then the exit status should be 0

  Scenario: Expand an empty `else` clause
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (cond-expand (else))
      """
    When I successfully run `scheme main.scm`
    Then the exit status should be 0

  Scenario: Use a `not` requirement
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (cond-expand
        ((not foo)
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
