Feature: unless
  Scenario: Evaluate a clause
    Given a file named "main.scm" with:
    """scheme
    (unless #f (write-u8 65))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Do not evaluate a clause
    Given a file named "main.scm" with:
    """scheme
    (unless #t (write-u8 65))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly ""
