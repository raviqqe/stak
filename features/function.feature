Feature: Function
  Background:
    Given I run the following script:
    """sh
    cp -r $ROOT/examples .
    cp $ROOT/*.scm .
    """

  Scenario: Call a continuation with a local variable
    Given a file named "source.scm" with:
    """scheme
    (write-u8 (+ 60 ((lambda (x) x) 5)))
    """
    When I run the following script:
    """sh
    cat prelude.scm source.scm | ./main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"
