Feature: cond
  Scenario: Evalute the first clause
    Given a file named "main.scm" with:
    """scheme
    (write-u8
      (cond
        (#t
          65)
        (else
          66)))
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"
