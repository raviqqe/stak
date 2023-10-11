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

  @stak
  Scenario: Print an error message
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (error "Oh, no!")
    """
    When I run `scheme main.scm`
    # TODO Write an error message to stderr.
    Then the stdout should contain "Oh, no!"
    And the exit status should not be 0

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
