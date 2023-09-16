Feature: Character
  Scenario: Check if a value is a character
    Given a file named "main.scm" with:
    """scheme
    (write-u8 (if (char? (integer->char 65)) 65 66))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Write a character
    Given a file named "main.scm" with:
    """scheme
    (write-char #\A)
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Write a newline character
    Given a file named "main.scm" with:
    """scheme
    (write-char #\A)
    (newline)
    (write-char #\B)
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly:
    """
    A
    B
    """
