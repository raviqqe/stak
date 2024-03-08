Feature: Exception
  Scenario: Raise an error
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (error "Oh, no!")
      """
    When I run `scheme main.scm`
    Then the stderr should contain "Oh, no!"
    And the exit status should not be 0

  @stak
  Scenario: Raise an error with an unknown value
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (error "Oh, no!" 42)
      """
    When I run `scheme main.scm`
    Then the stderr should contain "Oh, no!"
    And the stderr should contain "unknown"
    And the exit status should not be 0

  @stak @gauche
  Scenario: Raise an error with a value
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme write))

      (error "Oh, no!" 42)
      """
    When I run `scheme main.scm`
    Then the stderr should contain "Oh, no! 42"
    And the exit status should not be 0

  Scenario: Halt execution on an exception
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 65)

      (error "")

      (write-u8 66)
      """
    When I run `scheme main.scm`
    Then the exit status should not be 0
    And the stdout should contain exactly "A"

  Scenario: Raise a non-continuable exception
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8
        (with-exception-handler
          (lambda (value) 65)
          (lambda () (raise #f))))
      """
    When I run `scheme main.scm`
    Then the exit status should not be 0
    And the stdout should contain exactly ""

  Scenario: Raise a continuable exception
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8
        (with-exception-handler
          (lambda (value) 65)
          (lambda () (raise-continuable #f))))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Raise an exception in a handler
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (with-exception-handler
        (lambda (value) (raise #f))
        (lambda () (raise-continuable #f)))
      """
    When I run `scheme main.scm`
    Then the exit status should not be 0

  Scenario: Raise an exception in nested handlers
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme write))

      (with-exception-handler
        (lambda (value) (error "foo"))
        (lambda ()
          (with-exception-handler
            (lambda (value)
              (display "bar")
              (raise #f))
            (lambda () (raise #f)))))
      """
    When I run `scheme main.scm`
    Then the exit status should not be 0
    And the stderr should contain "foo"
    And the stdout should contain "bar"

  Scenario: Raise an exception in deeply nested handlers
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme write))

      (with-exception-handler
        (lambda (value) (error "foo"))
        (lambda ()
          (with-exception-handler
            (lambda (value)
              (display "bar")
              (raise #f))
            (lambda ()
              (with-exception-handler
                (lambda (value)
                  (display "baz")
                  (raise #f))
                (lambda () (raise #f)))))))
      """
    When I run `scheme main.scm`
    Then the exit status should not be 0
    And the stderr should contain "foo"
    And the stdout should contain "bar"
    And the stdout should contain "baz"

  Scenario: Terminate on a continued non-continuable exception
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8
        (with-exception-handler
          (lambda (value) 65)
          (lambda () (raise #f))))
      """
    When I run `scheme main.scm`
    Then the exit status should not be 0
    And the stdout should contain exactly ""

  @stak
  Scenario: Terminate on a continued non-continuable exception with a proper message
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8
        (with-exception-handler
          (lambda (value) 65)
          (lambda () (raise #f))))
      """
    When I run `scheme main.scm`
    Then the exit status should not be 0
    And the stdout should contain exactly ""
    And the stderr should contain "exception handler returned on a non-continuable exception"

  @stak @gauche @guile
  Scenario: Leave a dynamic extent
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme process-context))

      (dynamic-wind
        (lambda () (write-u8 65))
        (lambda () (error ""))
        (lambda () (write-u8 66)))
      """
    When I run `scheme main.scm`
    Then the exit status should not be 0
    And the stdout should contain exactly "AB"

  Scenario: Use a `guard` expression
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (guard
        (condition
          ((null? condition)
            #f)

          ((number? condition)
            (write-u8 condition))

          ((string? condition)
            #f))
        (raise 65))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Use an `else` clause in a guard expression
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (guard
        (condition
          ((null? condition)
            #f)

          ((string? condition)
            #f)

          (else
            (write-u8 condition)))
        (raise 65))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Use nested `guard` expressions
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (guard
        (condition
          ((null? condition)
            #f)

          ((number? condition)
            (write-u8 condition))

          ((string? condition)
            #f))
        (guard
          (condition
            ((null? condition)
              #f)

            ((number? condition)
              (write-u8 condition))

            ((string? condition)
              #f))
          (raise 65)))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Use a `guard` expression in a clause
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (guard
        (condition
          ((null? condition)
            #f)

          ((number? condition)
            (guard
              (condition
                ((null? condition)
                  #f)

                ((number? condition)
                  (write-u8 condition))

                ((string? condition)
                  #f))
              (raise condition)))

          ((string? condition)
            #f))
        (raise 65))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"
