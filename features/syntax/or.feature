Feature: or
  Scenario: Use an or operator
    Given a file named "main.scm" with:
    """scheme
    (write-u8 (if (or) 65 66))
    (write-u8 (if (or #t) 65 66))
    (write-u8 (if (or #f) 65 66))
    (write-u8 (if (or #f #f) 65 66))
    (write-u8 (if (or #f #t) 65 66))
    (write-u8 (or 65 #f))
    (write-u8 (or #f 65))
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "BABBAAA"
