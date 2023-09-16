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
    # TODO Test an exit code.

  @stak
  Scenario: Print an error message
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (error "Oh, no!")
    """
    When I run `scheme main.scm`
    Then the stdout should contain "Oh, no!"
    # TODO Write an error messasge to stderr.
    # TODO Test an exit code.
