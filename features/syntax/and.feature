Feature: and
  Scenario: Use an `and` operator
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (write-u8 (if (and) 65 66))
    (write-u8 (if (and #t) 65 66))
    (write-u8 (if (and #f) 65 66))
    (write-u8 (if (and #t #t) 65 66))
    (write-u8 (if (and #t #f) 65 66))
    """
    When I successfully run `scheme main.scm`
    # spell-checker: disable-next-line
    Then the stdout should contain exactly "AABAB"
