Feature: Character
  Background:
    Given I run the following script:
    """sh
    cp -r $ROOT/examples .
    cp $ROOT/*.scm .
    """

  Scenario: Check if a value is a character
    Given a file named "main.scm" with:
    """scheme
    (write-u8 (if (char? (integer->char 65)) 65 66))
    """
    When I run the following script:
    """sh
    cat prelude.scm main.scm | ./compile.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"

  Scenario: Write a character
    Given a file named "main.scm" with:
    """scheme
    (write-char #\A)
    """
    When I run the following script:
    """sh
    cat prelude.scm main.scm | ./compile.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"
