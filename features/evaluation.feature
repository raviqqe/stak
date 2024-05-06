Feature: Evaluation
  Scenario: Import an `eval` library
    Given a file named "main.scm" with:
      """scheme
      (import (scheme eval))
      """
    When I successfully run `scheme main.scm`
    Then the exit status should be 0

  Scenario: Evaluate false
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme eval))

      (write-u8 (if (eval #f (environment)) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "B"

  Scenario: Evaluate a number
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme eval))

      (write-u8 (eval 65 (environment)))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Evaluate a string
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme eval))

      (write-string (eval "foo" (environment)))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "foo"

  Scenario: Use an environment
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme eval))

      (write-u8 (eval 65 (environment '(scheme base))))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Use a `+` procedure
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme eval))

      (write-u8 (eval '(+ 60 5) (environment '(scheme base))))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Use a `display` procedure
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme eval))

      (write-u8 (eval '(display "foo") (environment '(scheme write))))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "foo"

  @stak
  Rule: Primitives
    Scenario: Use a `$$begin` primitive
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base) (scheme eval))

        (write-u8 (eval '($$begin 42 65) (environment)))
        """
      When I successfully run `scheme main.scm`
      Then the stdout should contain exactly "A"

    Scenario: Use a `$$if` primitive with a false value
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base) (scheme eval))

        (write-u8 (eval '($$if #f 65 66) (environment)))
        """
      When I successfully run `scheme main.scm`
      Then the stdout should contain exactly "B"

    Scenario: Use a `$$if` primitive with a true value
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base) (scheme eval))

        (write-u8 (eval '($$if #t 65 66) (environment)))
        """
      When I successfully run `scheme main.scm`
      Then the stdout should contain exactly "A"

    Scenario: Use a `$$lambda` primitive with no argument
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base) (scheme eval))

        (write-u8 (eval '(($$lambda () 65)) (environment)))
        """
      When I successfully run `scheme main.scm`
      Then the stdout should contain exactly "A"

    Scenario: Use a `$$lambda` primitive with an argument
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base) (scheme eval))

        (write-u8 (eval '(($$lambda (x) x) 65) (environment)))
        """
      When I successfully run `scheme main.scm`
      Then the stdout should contain exactly "A"

    Scenario: Use a `$$set!` primitive
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base) (scheme eval))

        (write-u8 (eval '($$begin ($$set! x 65) x) (environment)))
        """
      When I successfully run `scheme main.scm`
      Then the stdout should contain exactly "A"