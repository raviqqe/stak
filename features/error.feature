Feature: Error
  Scenario: Raise an error
    Given a file named "main.scm" with:
    """scheme
    (write-u8 65)

    (error "")

    (write-u8 65)
    """
    When I run `scheme main.scm`
    Then the stdout should contain "A"
    # TODO Test an exit code.

  Scenario: Print an error message
    Given a file named "main.scm" with:
    """scheme
    (error "Oh, no!")
    """
    When I run `scheme main.scm`
    Then the stdout should contain "Oh, no!"
    # TODO Test an exit code.
