Feature: Library system
  @stak @gauche
  Scenario: Define a library
    Given a file named "main.scm" with:
      """scheme
      (define-library (foo)
        (export foo)

        (import (scheme base))

        (begin
          (define (foo x)
            (write-u8 x))))
      """
    When I successfully run `scheme main.scm`
    Then the exit status should be 0

  @stak @gauche
  Scenario: Import a library twice
    Given a file named "main.scm" with:
      """scheme
      (define-library (foo)
        (import (scheme base) (scheme write))

        (begin
          (write-u8 65)))

      (import (foo))
      (import (foo))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  @stak @gauche
  Scenario: Import a function
    Given a file named "main.scm" with:
      """scheme
      (define-library (foo)
        (export foo)

        (import (scheme base))

        (begin
          (define (foo x)
            (write-u8 x))))

      (import (foo))

      (foo 65)
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  @stak @gauche
  Scenario: Import a macro
    Given a file named "main.scm" with:
      """scheme
      (define-library (foo)
        (export foo)

        (import (scheme base))

        (begin
          (define-syntax foo
            (syntax-rules ()
              ((_ x)
                (write-u8 x))))))

      (import (foo))

      (foo 65)
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  @stak @gauche
  Scenario: Import functions
    Given a file named "main.scm" with:
      """scheme
      (define-library (foo)
        (export foo bar)

        (import (scheme base))

        (begin
          (define (foo x)
            (write-u8 x))

          (define (bar x)
            (write-u8 (+ x 1)))))

      (import (foo))

      (foo 65)
      (bar 65)
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "AB"

  @stak @gauche
  Scenario: Import a function with a prefix
    Given a file named "main.scm" with:
      """scheme
      (define-library (foo)
        (export foo)

        (import (scheme base))

        (begin
          (define (foo x)
            (write-u8 x))))

      (import (prefix (foo) bar-))

      (bar-foo 65)
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  @stak @gauche
  Scenario: Import a renamed function
    Given a file named "main.scm" with:
      """scheme
      (define-library (foo)
        (export foo)

        (import (scheme base))

        (begin
          (define (foo x)
            (write-u8 x))))

      (import (rename (foo) (foo bar)))

      (bar 65)
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  @stak @gauche
  Scenario: Export a renamed function
    Given a file named "main.scm" with:
      """scheme
      (define-library (foo)
        (export (rename foo bar))

        (import (scheme base))

        (begin
          (define (foo x)
            (write-u8 x))))

      (import (foo))

      (bar 65)
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  @stak @gauche
  Scenario: Modify a library environment
    Given a file named "main.scm" with:
      """scheme
      (define-library (foo)
        (export foo bar)

        (import (scheme base))

        (begin
          (define (foo x)
            (write-u8 x))

          (define (bar x)
            (foo x))))

      (import (scheme base) (scheme write) (foo))

      (foo 65)
      (bar 65)

      (set! foo (lambda (x) (write-u8 66)))

      (foo 65)
      (bar 65)
      """
    When I successfully run `scheme main.scm`
    # spell-checker: disable-next-line
    Then the stdout should contain exactly "AABB"
