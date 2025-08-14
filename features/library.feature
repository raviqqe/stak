Feature: Library system

  Scenario: Define a library
    Given a file named "foo.scm" with:
      """scheme
      (define-library (foo)
        (export foo)

        (import (scheme base))

        (begin
          (define (foo x)
            (write-u8 x))))
      """
    And a file named "main.scm" with:
      """scheme
      """
    When I successfully run `stak -l foo.scm main.scm`
    Then the exit status should be 0

  Scenario: Import a library twice
    Given a file named "foo.scm" with:
      """scheme
      (define-library (foo)
        (import (scheme base))

        (begin
          (write-u8 65)))
      """
    And a file named "main.scm" with:
      """scheme
      (import (foo))
      (import (foo))
      """
    When I successfully run `stak -l foo.scm main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Import a procedure
    Given a file named "foo.scm" with:
      """scheme
      (define-library (foo)
        (export foo)

        (import (scheme base))

        (begin
          (define (foo x)
            (write-u8 x))))
      """
    And a file named "main.scm" with:
      """scheme
      (import (foo))

      (foo 65)
      """
    When I successfully run `stak -l foo.scm main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Import a macro
    Given a file named "foo.scm" with:
      """scheme
      (define-library (foo)
        (export foo)

        (import (scheme base))

        (begin
          (define-syntax foo
            (syntax-rules ()
              ((_ x)
                (write-u8 x))))))
      """
    And a file named "main.scm" with:
      """scheme
      (import (foo))

      (foo 65)
      """
    When I successfully run `stak -l foo.scm main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Import procedures
    Given a file named "foo.scm" with:
      """scheme
      (define-library (foo)
        (export foo bar)

        (import (scheme base))

        (begin
          (define (foo x)
            (write-u8 x))

          (define (bar x)
            (write-u8 (+ x 1)))))
      """
    And a file named "main.scm" with:
      """scheme
      (import (foo))

      (foo 65)
      (bar 65)
      """
    When I successfully run `stak -l foo.scm main.scm`
    Then the stdout should contain exactly "AB"

  Scenario: Import a procedure with a prefix
    Given a file named "foo.scm" with:
      """scheme
      (define-library (foo)
        (export foo)

        (import (scheme base))

        (begin
          (define (foo x)
            (write-u8 x))))
      """
    And a file named "main.scm" with:
      """scheme
      (import (prefix (foo) bar-))

      (bar-foo 65)
      """
    When I successfully run `stak -l foo.scm main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Import only a symbol
    Given a file named "foo.scm" with:
      """scheme
      (define-library (foo)
        (export foo)

        (import (scheme base))

        (begin
          (define (foo x)
            (write-u8 x))))
      """
    And a file named "main.scm" with:
      """scheme
      (import (only (foo) foo))

      (foo 65)
      """
    When I successfully run `stak -l foo.scm main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Import only a symbol and use one of the others
    Given a file named "foo.scm" with:
      """scheme
      (define-library (foo)
        (export foo bar)

        (import (scheme base))

        (begin
          (define (foo)
            #f)

          (define (bar)
            #f)))
      """
    And a file named "main.scm" with:
      """scheme
      (import (only (foo) foo))

      (bar)
      """
    When I run `stak -l foo.scm main.scm`
    Then the exit status should not be 0

  Scenario: Import symbols except one
    Given a file named "foo.scm" with:
      """scheme
      (define-library (foo)
        (export foo bar)

        (import (scheme base))

        (begin
          (define (foo x)
            (write-u8 x))

          (define (bar x)
            (write-u8 (+ x 1)))))
      """
    And a file named "main.scm" with:
      """scheme
      (import (except (foo) foo))

      (bar 65)
      """
    When I successfully run `stak -l foo.scm main.scm`
    Then the stdout should contain exactly "B"

  Scenario: Import symbols except one and use it
    Given a file named "foo.scm" with:
      """scheme
      (define-library (foo)
        (export foo bar)

        (import (scheme base))

        (begin
          (define (foo)
            #f)

          (define (bar)
            #f)))
      """
    And a file named "main.scm" with:
      """scheme
      (import (except (foo) foo))

      (foo)
      """
    When I run `stak -l foo.scm main.scm`
    Then the exit status should not be 0

  Scenario: Import a renamed procedure
    Given a file named "foo.scm" with:
      """scheme
      (define-library (foo)
        (export foo)

        (import (scheme base))

        (begin
          (define (foo x)
            (write-u8 x))))
      """
    And a file named "main.scm" with:
      """scheme
      (import (rename (foo) (foo bar)))

      (bar 65)
      """
    When I successfully run `stak -l foo.scm main.scm`
    Then the stdout should contain exactly "A"

  @gauche @guile @stak
  Scenario Outline: Nest import qualifiers
    Given a file named "foo.scm" with:
      """scheme
      (define-library (foo)
        (export foo bar)

        (import (scheme base))

        (begin
          (define (foo x)
            (write-u8 x))

          (define (bar x)
            (write-u8 (+ x 1)))))
      """
    And a file named "main.scm" with:
      """scheme
      (import <import set>)

      (<symbol> 65)
      """
    When I successfully run `stak -l foo.scm main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | import set                                            | symbol | output |
      | (only (prefix (foo) my-) my-foo)                      | my-foo | A      |
      | (rename (prefix (foo) my-) (my-foo my-baz))           | my-baz | A      |
      | (except (prefix (rename (foo) (bar baz)) my-) my-foo) | my-baz | B      |

  Scenario: Re-export an imported procedure
    Given a file named "foo.scm" with:
      """scheme
      (define-library (foo)
        (export foo)

        (import (scheme base))

        (begin
          (define (foo x)
            (write-u8 x))))
      """
    And a file named "bar.scm" with:
      """scheme
      (define-library (bar)
        (export foo)

        (import (foo)))
      """
    And a file named "main.scm" with:
      """scheme
      (import (bar))

      (foo 65)
      """
    When I successfully run `stak -l foo.scm -l bar.scm main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Re-export an imported procedure twice
    Given a file named "foo.scm" with:
      """scheme
      (define-library (foo)
        (export foo)

        (import (scheme base))

        (begin
          (define (foo x)
            (write-u8 x))))
      """
    And a file named "bar.scm" with:
      """scheme
      (define-library (bar)
        (export foo)

        (import (foo)))
      """
    And a file named "baz.scm" with:
      """scheme
      (define-library (baz)
        (export foo)

        (import (bar)))
      """
    And a file named "main.scm" with:
      """scheme
      (import (baz))

      (foo 65)
      """
    When I successfully run `stak -l foo.scm -l bar.scm -l baz.scm main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Re-export an imported syntax
    Given a file named "foo.scm" with:
      """scheme
      (define-library (foo)
        (export foo)

        (import (scheme base))

        (begin
          (define-syntax foo
            (syntax-rules ()
              ((_ x)
                (write-u8 x))))))
      """
    And a file named "bar.scm" with:
      """scheme
      (define-library (bar)
        (export foo)

        (import (foo)))
      """
    And a file named "main.scm" with:
      """scheme
      (import (bar))

      (foo 65)
      """
    When I successfully run `stak -l foo.scm -l bar.scm main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Re-export an imported syntax twice
    Given a file named "foo.scm" with:
      """scheme
      (define-library (foo)
        (export foo)

        (import (scheme base))

        (begin
          (define-syntax foo
            (syntax-rules ()
              ((_ x)
                (write-u8 x))))))
      """
    And a file named "bar.scm" with:
      """scheme
      (define-library (bar)
        (export foo)

        (import (foo)))
      """
    And a file named "baz.scm" with:
      """scheme
      (define-library (baz)
        (export foo)

        (import (bar)))
      """
    And a file named "main.scm" with:
      """scheme
      (import (baz))

      (foo 65)
      """
    When I successfully run `stak -l foo.scm -l bar.scm -l baz.scm main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Re-export a qualified imported procedure
    Given a file named "foo.scm" with:
      """scheme
      (define-library (foo)
        (export foo)

        (import (scheme base))

        (begin
          (define (foo x)
            (write-u8 x))))
      """
    And a file named "bar.scm" with:
      """scheme
      (define-library (bar)
        (export foo)

        (import (only (foo) foo)))
      """
    And a file named "main.scm" with:
      """scheme
      (import (bar))

      (foo 65)
      """
    When I successfully run `stak -l foo.scm -l bar.scm main.scm`
    Then the stdout should contain exactly "A"

  
  Scenario: Export a renamed procedure
    Given a file named "foo.scm" with:
      """scheme
      (define-library (foo)
        (export (rename foo bar))

        (import (scheme base))

        (begin
          (define (foo x)
            (write-u8 x))))
      """
    And a file named "main.scm" with:
      """scheme
      (import (foo))

      (bar 65)
      """
    When I successfully run `stak -l foo.scm main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Do not modify a library environment
    Given a file named "foo.scm" with:
      """scheme
      (define-library (foo)
        (export bar)

        (import (scheme base))

        (begin
          (define (foo x)
            (write-u8 x))

          (define (bar x)
            (foo x))))
      """
    And a file named "main.scm" with:
      """scheme
      (import (scheme base) (foo))

      (define (foo x)
        (write-u8 66))

      (foo 65)
      (bar 65)
      """
    When I successfully run `stak -l foo.scm main.scm`
    Then the stdout should contain exactly "BA"

  
  Scenario: Modify a library environment
    Given a file named "foo.scm" with:
      """scheme
      (define-library (foo)
        (export foo bar)

        (import (scheme base))

        (begin
          (define (foo x)
            (write-u8 x))

          (define (bar x)
            (foo x))))
      """
    And a file named "main.scm" with:
      """scheme
      (import (scheme base) (foo))

      (foo 65)
      (bar 65)

      (set! foo (lambda (x) (write-u8 66)))

      (foo 65)
      (bar 65)
      """
    When I successfully run `stak -l foo.scm main.scm`
    # spell-checker: disable-next-line
    Then the stdout should contain exactly "AABB"

  Scenario: Display a symbol from a standard library
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme write))

      (display 'define)
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "define"

  Scenario: Display a symbol from an external library
    Given a file named "foo.scm" with:
      """scheme
      (define-library (foo)
        (export foo)

        (import (scheme base))

        (begin
          (define foo 42)))
      """
    And a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme write) (foo))

      (display 'foo)
      """
    When I successfully run `stak -l foo.scm main.scm`
    Then the stdout should contain exactly "foo"
