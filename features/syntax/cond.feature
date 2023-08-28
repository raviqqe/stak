Feature: cond
  Scenario: Evaluate the first clause
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

  Scenario: Evaluate the second clause
    Given a file named "main.scm" with:
    """scheme
    (write-u8
      (cond
        (#f
          65)
        (#t
          66)
        (else
          67)))
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "B"
