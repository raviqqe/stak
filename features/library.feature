Feature: Library system
  @gauche @stak
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

  @gauche @stak
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

  @gauche @stak
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

  @gauche @stak
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

  @gauche @stak
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

  @gauche @stak
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

  @gauche @stak
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

  @gauche @stak
  Scenario: Export an imported function
    Given a file named "main.scm" with:
      """scheme
      (define-library (foo)
        (export foo)

        (import (scheme base))

        (begin
          (define (foo x)
            (write-u8 x))))

      (define-library (bar)
        (export foo)

        (import (foo)))

      (import (bar))

      (foo 65)
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  @gauche @stak
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

  @gauche @stak
  Scenario: Do not modify a library environment
    Given a file named "main.scm" with:
      """scheme
      (define-library (foo)
        (export bar)

        (import (scheme base))

        (begin
          (define (foo x)
            (write-u8 x))

          (define (bar x)
            (foo x))))

      (import (scheme base) (scheme write) (foo))

      (define (foo x)
        (write-u8 66))

      (foo 65)
      (bar 65)
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "BA"

  @gauche @stak
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
