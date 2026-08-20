@stak
Feature: Optimization

  Scenario: Match a rule
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (stak base))

      (define-optimizer foo
        (syntax-rules ()
          ((foo)
            (write-u8 65))))

      (foo)
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Match a variable pattern
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (stak base))

      (define-optimizer foo
        (syntax-rules ()
          ((foo x)
            (write-u8 x))))

      (foo 65)
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Match an ellipsis pattern
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (stak base))

      (define-optimizer foo
        (syntax-rules ()
          ((foo x ...)
            (for-each write-u8 (list x ...)))))

      (foo 65 66 67)
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "ABC"

  Scenario: Expand a template calling another optimizer
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (stak base))

      (define-optimizer foo
        (syntax-rules ()
          ((foo x)
            (write-u8 x))))

      (define-optimizer bar
        (syntax-rules ()
          ((bar x)
            (foo x))))

      (bar 65)
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Expand an ellipsis template calling a variadic optimizer
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (stak base))

      (define-optimizer foo
        (syntax-rules ()
          ((foo x)
            (write-u8 x))

          ((foo x y ...)
            (begin (write-u8 x) (foo y ...)))))

      (define-optimizer bar
        (syntax-rules ()
          ((bar x ...)
            (foo x ...))))

      (bar 65 66 67)
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "ABC"
