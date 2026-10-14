@extra
Feature: REPL

  Scenario: Use a base library
    Given a file named "input.scm" with:
      """scheme
      (define (foo x) (+ x 1))
      (foo 41)
      """
    When I run `stak-repl` interactively
    And I pipe in the file "input.scm"
    Then the exit status should be 0
    And the stdout should contain "42"

  Scenario: Load a file
    Given a file named "foo.scm" with:
      """scheme
      (define (foo x) (+ x 1))
      """
    And a file named "input.scm" with:
      """scheme
      (import (scheme load))
      (load "./foo.scm")
      (foo 41)
      """
    When I run `stak-repl` interactively
    And I pipe in the file "input.scm"
    Then the exit status should be 0
    And the stdout should contain "42"

  Scenario: Import a library in a data directory
    Given a file named "data/stak/foo.sld" with:
      """scheme
      (define-library (foo)
        (export foo)

        (import (scheme base))

        (begin
          (define foo 42)))
      """
    And a file named "input.scm" with:
      """scheme
      (import (foo))
      foo
      """
    And I set the environment variable "XDG_DATA_HOME" to "data"
    When I run `stak-repl` interactively
    And I pipe in the file "input.scm"
    Then the exit status should be 0
    And the stdout should contain "42"
