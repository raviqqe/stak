Feature: when
  Scenario: Evaluate a clause
    Given a file named "main.scm" with:
    """scheme
    (when #t (write-u8 65))
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"

  Scenario: Do not evaluate a clause
    Given a file named "main.scm" with:
    """scheme
    (when #f (write-u8 65))
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly ""
