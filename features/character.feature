Feature: Character
  Background:
    Given I run the following script:
    """sh
    cp -r $ROOT/examples .
    cp $ROOT/*.scm .
    """

  Scenario: Check if a value is a character
    Given a file named "source.scm" with:
    """scheme
    (write-u8 (if (char? (integer->char 65)) 65 66))
    """
    When I run the following script:
    """sh
    cat prelude.scm source.scm | ./compile.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"
