Feature: or
  Scenario: Use an or operator
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (write-u8 (if (or) 65 66))
    (write-u8 (if (or #t) 65 66))
    (write-u8 (if (or #f) 65 66))
    (write-u8 (if (or #f #f) 65 66))
    (write-u8 (if (or #f #t) 65 66))
    (write-u8 (or 65 #f))
    (write-u8 (or #f 65))
    """
    When I successfully run `scheme main.scm`
    # spell-checker: disable-next-line
    Then the stdout should contain exactly "BABBAAA"
