Feature: Boolean
  Scenario: Use if expressions
    Given a file named "main.scm" with:
    """scheme
    (write-u8 (if #f 65 66))
    (write-u8 (if #t 65 66))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "BA"

  Scenario: Use nested if expressions
    Given a file named "main.scm" with:
    """scheme
    (write-u8 (if #t (if #t 65 67) 67))
    (write-u8 (if #f 67 (if #f 67 66)))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "AB"

  Scenario: Use deeply nested if expressions
    Given a file named "main.scm" with:
    """scheme
    (write-u8 (if #t (if #t (if #t 65 67) 67) 67))
    (write-u8 (if #f 67 (if #f 67 (if #f 67 66))))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "AB"

  Scenario: Use a not operator
    Given a file named "main.scm" with:
    """scheme
    (write-u8 (if (not #f) 65 66))
    (write-u8 (if (not #t) 65 66))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "AB"
