Feature: Number
  Background:
    Given I run the following script:
    """sh
    cp -r $ROOT/examples .
    cp $ROOT/*.scm .
    """

  Scenario: Use a negative integer
    Given a file named "source.scm" with:
    """scheme
    (define x -1)
    (write-u8 (+ 66 x))
    """
    When I run the following script:
    """sh
    cat prelude.scm source.scm | ./main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"

  Scenario: Use large (but not big) integers
    Given a file named "source.scm" with:
    """scheme
    (write-u8 (- 1065 1000))
    """
    When I run the following script:
    """sh
    cat prelude.scm source.scm | ./main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"

  Scenario: Use integers around the encoding base
    Given a file named "source.scm" with:
    """scheme
    (write-u8 (- 127 60))
    (write-u8 (- 128 60))
    (write-u8 (- 129 60))
    """
    When I run the following script:
    """sh
    cat prelude.scm source.scm | ./main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "CDE"
