Feature: Quote
  Scenario: Quote a number
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (write-u8 '65)
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Quote a list
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (for-each write-u8 '(65 66 67))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "ABC"
