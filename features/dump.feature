Feature: Dump
  Scenario: Dump a value
    Given a file named "main.scm" with:
    """scheme
    (dump 42)
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly ""

  Scenario: Pass through a value
    Given a file named "main.scm" with:
    """scheme
    (write-u8 (dump 65))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"
