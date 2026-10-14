@extra
Feature: Extra functionalities

  Background:
    Given I run the following script:
      """sh
      cp $STAK_ROOT/prelude.scm .
      """
    And the exit status should be 0

  Scenario: Dump a stack trace on an error
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (stak backtrace))

      (define (foo)
        (error "foo")
        #f)

      (define (bar)
        (foo)
        #f)

      (define (baz)
        (bar)
        #f)

      (let ()
        (baz)
        #f)
      """
    When I run `stak main.scm`
    Then the exit status should be 1
    And the stderr should contain "foo"
    And the stderr should contain "[error foo bar baz eval #f]"

  Scenario: Show an unknown irritant of an error
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (error "foo" 42)
      """
    When I run the following script:
      """sh
      cat prelude.scm main.scm | stak-compile > main.bc
      """
    And I run `stak-interpret main.bc`
    Then the exit status should be 1
    And the stderr should contain "foo"
    And the stderr should contain "unknown"

  Scenario: Use a base library in a REPL
    Given a file named "input.scm" with:
      """scheme
      (define (foo x) (+ x 1))
      (foo 41)
      """
    When I run `stak-repl` interactively
    And I pipe in the file "input.scm"
    Then the exit status should be 0
    And the stdout should contain "42"

  Scenario: Load a file in a REPL
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

  Scenario: Import a library in a data directory in a REPL
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
