Feature: Vector
  Background:
    Given I run the following script:
    """sh
    cp -r $ROOT/examples .
    cp $ROOT/*.scm .
    """

  Scenario: Convert a vector to a list
    Given a file named "main.scm" with:
    """scheme
    (map write-u8 (vector->list #(65 66 67)))
    """
    When I run the following script:
    """sh
    cat prelude.scm main.scm | compile.sh > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "ABC"
