Feature: cond-expand

  Scenario: Expand an `else` clause
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (cond-expand
        (else
          (write-u8 65)))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Match an implemented feature
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (cond-expand
        (r7rs
          (write-u8 65)))
      """
    When I successfully run `stak main.scm`
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
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "B"

  Scenario: Match an implemented library
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (cond-expand
        ((library (scheme base))
          (write-u8 65)))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

  Scenario Outline: Match a built-in library
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (cond-expand
        ((library <library>)
          (write-u8 65))
        (else
          (write-u8 66)))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | library       |
      | (scheme char) |
      | (scheme lazy) |

    @chibi @gauche @stak
    Examples:
      | library  |
      | (srfi 1) |

  Scenario: Match an imported library
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme read))

      (cond-expand
        ((library (scheme read))
          (write-u8 65))
        (else
          (write-u8 66)))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

  @gauche @stak
  Scenario: Match a library defined in a program
    Given a file named "main.scm" with:
      """scheme
      (define-library (foo)
        (export foo)

        (import (scheme base))

        (begin
          (define foo 65)))

      (import (scheme base))

      (cond-expand
        ((library (foo))
          (write-u8 65))
        (else
          (write-u8 66)))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

  @chibi @guile @stak
  Scenario: Match a library in a load path
    Given a file named "library/foo/bar.sld" with:
      """scheme
      (define-library (foo bar)
        (export bar)

        (import (scheme base))

        (begin
          (define bar 65)))
      """
    And a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (cond-expand
        ((library (foo bar))
          (write-u8 65))
        (else
          (write-u8 66)))
      """
    When I successfully run `stak -I library main.scm`
    Then the stdout should contain exactly "A"

  @chibi @gauche @stak
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
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "B"

  @chibi @gauche @stak
  Scenario: Match a library missing in a load path
    Given a directory named "library"
    And a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (cond-expand
        ((library (foo bar))
          (write-u8 65))
        (else
          (write-u8 66)))
      """
    When I successfully run `stak -I library main.scm`
    Then the stdout should contain exactly "B"

  @chibi @gauche @stak
  Scenario: Expand only a matched library clause
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (cond-expand
        ((library (scheme miracle))
          (syntax-error "unexpected expansion"))
        (else
          (write-u8 65)))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Match an invalid feature
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (cond-expand
        ((lib (scheme base))
          (write-u8 65))
        (else
          (write-u8 66)))
      """
    When I run `stak main.scm`
    Then the exit status should not be 0

  Scenario: Expand an empty clause
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (cond-expand (r7rs))
      """
    When I successfully run `stak main.scm`
    Then the exit status should be 0

  Scenario: Expand an empty `else` clause
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (cond-expand (else))
      """
    When I successfully run `stak main.scm`
    Then the exit status should be 0

  Scenario: Use a `not` requirement
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (cond-expand
        ((not foo)
          (write-u8 65)))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

  @chibi @gauche @stak
  Scenario: Use a `not` requirement with a library
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (cond-expand
        ((not (library (scheme miracle)))
          (write-u8 65))
        (else
          (write-u8 66)))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Examine features
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (memq 'r7rs (features)) 65 66))
      """
    When I successfully run `stak main.scm`
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
      When I successfully run `stak main.scm`
      Then the stdout should contain exactly "A"

    Scenario: Expand a requirement
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base))

        (cond-expand
          ((and r7rs)
            (write-u8 65)))
        """
      When I successfully run `stak main.scm`
      Then the stdout should contain exactly "A"

    Scenario: Expand two requirements
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base))

        (cond-expand
          ((and r7rs r7rs)
            (write-u8 65)))
        """
      When I successfully run `stak main.scm`
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
      When I successfully run `stak main.scm`
      Then the stdout should contain exactly "B"

    Scenario: Expand a requirement
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base))

        (cond-expand
          ((or r7rs)
            (write-u8 65)))
        """
      When I successfully run `stak main.scm`
      Then the stdout should contain exactly "A"

    Scenario: Expand two requirements
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base))

        (cond-expand
          ((or foo r7rs)
            (write-u8 65)))
        """
      When I successfully run `stak main.scm`
      Then the stdout should contain exactly "A"
