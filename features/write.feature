Feature: Write
  Background:
    Given I run the following script:
    """sh
    cp -r $ROOT/examples .
    cp -r $ROOT/tools .
    cp $ROOT/*.scm .
    """

  Scenario: Write a character integer
    Given a file named "source.scm" with:
    """scheme
    (write-u8 65)
    """
    When I run the following script:
    """sh
    cat prelude.scm source.scm | tools/compile.sh > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"

  Scenario: Write a character
    Given a file named "source.scm" with:
    """scheme
    (write-char (integer->char 65))
    """
    When I run the following script:
    """sh
    cat prelude.scm source.scm | tools/compile.sh > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"
