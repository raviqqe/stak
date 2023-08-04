Feature: Error
  Background:
    Given I run the following script:
    """sh
    cp -r $ROOT/examples .
    cp $ROOT/*.scm .
    """

  Scenario: Raise an error
    Given a file named "source.scm" with:
    """scheme
    (write-u8 65)

    (error #f)

    (write-u8 65)
    """
    When I run the following script:
    """sh
    cat prelude.scm source.scm | ./main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"
