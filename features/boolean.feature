Feature: Boolean
  Scenario: Use if expressions
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (write-u8 (if #f 65 66))
    (write-u8 (if #t 65 66))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "BA"

  Scenario: Use nested if expressions
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (write-u8 (if #t (if #t 65 67) 67))
    (write-u8 (if #f 67 (if #f 67 66)))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "AB"

  Scenario: Use deeply nested if expressions
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (write-u8 (if #t (if #t (if #t 65 67) 67) 67))
    (write-u8 (if #f 67 (if #f 67 (if #f 67 66))))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "AB"

  Scenario: Use a not operator
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (write-u8 (if (not #f) 65 66))
    (write-u8 (if (not #t) 65 66))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "AB"

  Scenario: Use a one-sided if expression
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (if #t (write-u8 65))
    (if #f (write-u8 66))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario Outline: Use deeply nested if expressions
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (if <value1>
      (begin
        (write-u8 65)
          (if <value2>
            (begin
              (write-u8 65)
              (if <value3>
                (write-u8 65)
                (write-u8 66)))
            (begin
              (write-u8 66)
              (if <value3>
                (write-u8 65)
                (write-u8 66)))))
      (begin
        (write-u8 66)
          (if <value2>
            (begin
              (write-u8 65)
              (if <value3>
                (write-u8 65)
                (write-u8 66)))
            (begin
              (write-u8 66)
              (if <value3>
                (write-u8 65)
                (write-u8 66))))))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | value1 | value2 | value3 | output |
      | #t     | #t     | #t     | AAA    |
      | #t     | #t     | #f     | AAB    |
      | #t     | #f     | #t     | ABA    |
      | #t     | #f     | #f     | ABB    |
      | #f     | #t     | #t     | BAA    |
      | #f     | #t     | #f     | BAB    |
      | #f     | #f     | #t     | BBA    |
      | #f     | #f     | #f     | BBB    |

  Scenario Outline: Use sequenced if expressions in a nested if expression
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (if <value1>
      (begin
        (write-u8 65)
        (if <value2>
          (write-u8 65)
          (write-u8 66))
        (if <value3>
          (write-u8 65)
          (write-u8 66)))
      (begin
        (write-u8 66)
        (if <value2>
          (write-u8 65)
          (write-u8 66))
        (if <value3>
          (write-u8 65)
          (write-u8 66))))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | value1 | value2 | value3 | output |
      | #t     | #t     | #t     | AAA    |
      | #t     | #t     | #f     | AAB    |
      | #t     | #f     | #t     | ABA    |
      | #t     | #f     | #f     | ABB    |
      | #f     | #t     | #t     | BAA    |
      | #f     | #t     | #f     | BAB    |
      | #f     | #f     | #t     | BBA    |
      | #f     | #f     | #f     | BBB    |
