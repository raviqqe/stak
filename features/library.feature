@gauche @library @stak
Feature: Library system
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

  Scenario: Import a procedure
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

  Scenario: Import procedures
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

  Scenario: Import a procedure with a prefix
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

  Scenario: Import only a symbol
    Given a file named "main.scm" with:
      """scheme
      (define-library (foo)
        (export foo)

        (import (scheme base))

        (begin
          (define (foo x)
            (write-u8 x))))

      (import (only (foo) foo))

      (foo 65)
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Import only a symbol and use one of the others
    Given a file named "main.scm" with:
      """scheme
      (define-library (foo)
        (export foo bar)

        (import (scheme base))

        (begin
          (define (foo)
            #f)

          (define (bar)
            #f)))

      (import (only (foo) foo))

      (bar)
      """
    When I run `scheme main.scm`
    Then the exit status should not be 0

  Scenario: Import symbols except one
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

      (import (except (foo) foo))

      (bar 65)
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "B"

  Scenario: Import symbols except one and use it
    Given a file named "main.scm" with:
      """scheme
      (define-library (foo)
        (export foo bar)

        (import (scheme base))

        (begin
          (define (foo)
            #f)

          (define (bar)
            #f)))

      (import (except (foo) foo))

      (foo)
      """
    When I run `scheme main.scm`
    Then the exit status should not be 0

  Scenario: Import a renamed procedure
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

  Scenario Outline: Nest import qualifiers
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

      (import <import set>)

      (<symbol> 65)
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | import set                                            | symbol | output |
      | (only (prefix (foo) my-) my-foo)                      | my-foo | A      |
      | (rename (prefix (foo) my-) (my-foo my-baz))           | my-baz | A      |
      | (except (prefix (rename (foo) (bar baz)) my-) my-foo) | my-baz | B      |

  Scenario: Export an imported procedure
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

  Scenario: Export a renamed procedure
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
