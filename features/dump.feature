Feature: Dump
  @stak
  Scenario: Dump a value
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (dump 42)
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly ""

  @stak
  Scenario: Pass through a value
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (write-u8 (dump 65))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"
