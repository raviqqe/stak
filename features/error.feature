Feature: Error
  Scenario: Raise an error
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (write-u8 65)

    (error "")

    (write-u8 65)
    """
    When I run `scheme main.scm`
    Then the stdout should contain "A"
    And the exit status should not be 0

  Scenario: Print an error message
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (error "Oh, no!")
    """
    When I run `scheme main.scm`
    Then the stderr should contain "Oh, no!"
    And the exit status should not be 0

  Scenario: Raise a continuable exception
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (write-u8
      (with-exception-handler
        (lambda (error) 65)
        (lambda () (raise-continuable #f))))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain "A"

  Scenario: Raise an exception in a handler
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (with-exception-handler
      (lambda (error) (raise-continuable #f))
      (lambda () (raise-continuable #f)))
    """
    When I run `scheme main.scm`
    Then the exit status should not be 0
