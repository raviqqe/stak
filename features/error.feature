Feature: Error
  Scenario: Raise an error
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (error "Oh, no!")
    """
    When I run `scheme main.scm`
    Then the stderr should contain "Oh, no!"
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

  Scenario: Raise an exception in handlers
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (with-exception-handler
      (lambda (value) (error "Oh, no!"))
      (lambda ()
        (with-exception-handler
          (lambda (value) (raise #f))
          (lambda () (raise-continuable #f)))))
    """
    When I run `scheme main.scm`
    Then the exit status should not be 0
    And the stderr should contain "Oh, no!"

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
