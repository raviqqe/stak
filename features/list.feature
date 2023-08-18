Feature: List
  Background:
    Given I run the following script:
    """sh
    cp -r $ROOT/examples .
    cp $ROOT/*.scm .
    """

  Scenario: Use a map function
    Given a file named "main.scm" with:
    """scheme
    (map write-u8 '(65 66 67))
    """
    When I run the following script:
    """sh
    cat prelude.scm main.scm | compile.sh > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "ABC"
