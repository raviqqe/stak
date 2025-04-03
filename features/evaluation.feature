@long
Feature: Evaluation
  Scenario: Import an `eval` library
    Given a file named "main.scm" with:
      """scheme
      (import (scheme eval))
      """
    When I successfully run `stak main.scm`
    Then the exit status should be 0

  Scenario: Evaluate false
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme eval))

      (write-u8 (if (eval #f (environment)) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "B"

  Scenario: Evaluate a number
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme eval))

      (write-u8 (eval 65 (environment)))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Evaluate a string
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme eval))

      (write-string (eval "foo" (environment)))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "foo"

  Scenario: Use an environment
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme eval))

      (write-u8 (eval 65 (environment '(scheme base))))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Use two environments
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme eval))

      (write-u8 (eval 65 (environment '(scheme base) '(scheme write))))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Use an interaction environment
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme eval) (scheme repl))

      (write-u8 (eval 65 (interaction-environment)))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Define variables in an interaction environment
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme eval) (scheme repl))

      (eval '(define x 42) (interaction-environment))
      (eval '(define y 23) (interaction-environment))
      (write-u8 (eval '(+ x y) (interaction-environment)))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

  Rule: Environment
    @gauche @guile @stak
    Scenario: Use a `define` syntax with a variable
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base) (scheme eval))

        (eval
          '(begin
            (define x 65)
            (write-u8 x))
          (environment '(scheme base)))
        """
      When I successfully run `stak main.scm`
      Then the stdout should contain exactly "A"

    @gauche @guile @stak
    Scenario: Use a `define` syntax with a procedure
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base) (scheme eval))

        (eval
          '(begin
            (define (f x)
              (+ x 65))
            (write-u8 (f 1)))
          (environment '(scheme base)))
        """
      When I successfully run `stak main.scm`
      Then the stdout should contain exactly "B"

    Scenario: Do not access outer environment
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base) (scheme eval))

        (define x #t)

        (eval
          '(unless x (error "Oh, no!"))
          (environment '(scheme base)))
        """
      When I run `stak main.scm`
      Then the exit status should not be 0

    @gauche @guile @stak
    Scenario: Do not corrupt outer environment
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base) (scheme eval))

        (define x 65)

        (eval
          '(begin (define x 66))
          (environment '(scheme base)))

        (write-u8 x)
        """
      When I successfully run `stak main.scm`
      Then the stdout should contain exactly "A"

  Rule: Procedures
    Scenario: Use a `+` procedure
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base) (scheme eval))

        (write-u8 (eval '(+ 60 5) (environment '(scheme base))))
        """
      When I successfully run `stak main.scm`
      Then the stdout should contain exactly "A"

    Scenario: Use a `display` procedure
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base) (scheme eval) (scheme write))

        (eval '(display "foo") (environment '(scheme write)))
        """
      When I successfully run `stak main.scm`
      Then the stdout should contain exactly "foo"

    Scenario: Use a `set!` procedure
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base) (scheme eval))

        (write-u8
          (eval
            '(let ((x 0))
              (set! x 65)
              x)
            (environment '(scheme base))))
        """
      When I successfully run `stak main.scm`
      Then the stdout should contain exactly "A"

  Rule: Syntaxes
    Scenario: Use a `begin` syntax
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base) (scheme eval))

        (write-u8
          (eval
            '(begin 42 65)
            (environment '(scheme base))))
        """
      When I successfully run `stak main.scm`
      Then the stdout should contain exactly "A"

    Scenario Outline: Use an `if` syntax
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base) (scheme eval))

        (eval '(write-u8 (if <value> 65 66)) (environment '(scheme base)))
        """
      When I successfully run `stak main.scm`
      Then the stdout should contain exactly "<output>"

      Examples:
        | value | output |
        | #f    | B      |
        | #t    | A      |

    Scenario: Use a `lambda` syntax with no argument
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base) (scheme eval))

        (write-u8
          (eval
            '((lambda () 65))
            (environment '(scheme base))))
        """
      When I successfully run `stak main.scm`
      Then the stdout should contain exactly "A"

    Scenario: Use a `lambda` syntax with an argument
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base) (scheme eval))

        (write-u8
          (eval
            '((lambda (x) x) 65)
            (environment '(scheme base))))
        """
      When I successfully run `stak main.scm`
      Then the stdout should contain exactly "A"
