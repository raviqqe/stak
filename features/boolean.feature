Feature: Boolean
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
    (write-u8 (if (not #f) 65 66))
    (write-u8 (if (not #t) 65 66))
    """
    When I run the following script:
    """sh
    cat prelude.scm source.scm | tools/compile.sh > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "BA"
