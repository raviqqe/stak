Feature: and
  Scenario: Use an and operator
    Given a file named "main.scm" with:
    """scheme
    (write-u8 (if (and) 65 66))
    (write-u8 (if (and #t) 65 66))
    (write-u8 (if (and #f) 65 66))
    (write-u8 (if (and #t #t) 65 66))
    (write-u8 (if (and #t #f) 65 66))
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "AABAB"
