Feature: Write
  Scenario: Write a character integer
    Given a file named "main.scm" with:
    """scheme
    (write-u8 65)
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Write a character
    Given a file named "main.scm" with:
    """scheme
    (write-char (integer->char 65))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"
