Feature: Boolean
  Background:
    Given I run the following script:
    """sh
    cp -r $ROOT/examples .
    cp $ROOT/*.scm .
    """

  Scenario: Use a not operator
    Given a file named "source.scm" with:
    """scheme
    (write-u8 (if (not #f) 65 66))
    (write-u8 (if (not #t) 65 66))
    """
    When I run the following script:
    """sh
    cat prelude.scm source.scm | ./main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "AB"

  Scenario: Use an and operator
    Given a file named "source.scm" with:
    """scheme
    (write-u8 (if (and) 65 66))
    (write-u8 (if (and #t) 65 66))
    (write-u8 (if (and #f) 65 66))
    (write-u8 (if (and #t #t) 65 66))
    (write-u8 (if (and #t #f) 65 66))
    """
    When I run the following script:
    """sh
    cat prelude.scm source.scm | ./main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "AABAB"

  Scenario: Use an or operator
    Given a file named "source.scm" with:
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
    cat prelude.scm source.scm | ./main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "BABBAAA"

  Scenario: Use if expressions
    Given a file named "source.scm" with:
    """scheme
    (write-u8 (if #f 65 66))
    (write-u8 (if #t 65 66))
    """
    When I run the following script:
    """sh
    cat prelude.scm source.scm | ./main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "BA"

  Scenario: Use nested if expressions
    Given a file named "source.scm" with:
    """scheme
    (write-u8 (if #t (if #t 65 67) 67))
    (write-u8 (if #f 67 (if #f 67 66)))
    """
    When I run the following script:
    """sh
    cat prelude.scm source.scm | ./main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "AB"

  Scenario: Use deeply nested if expressions
    Given a file named "source.scm" with:
    """scheme
    (write-u8 (if #t (if #t (if #t 65 67) 67) 67))
    (write-u8 (if #f 67 (if #f 67 (if #f 67 66))))
    """
    When I run the following script:
    """sh
    cat prelude.scm source.scm | ./main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "AB"
