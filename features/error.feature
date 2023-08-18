Feature: Error
  Background:
    Given I run the following script:
    """sh
    cp -r $ROOT/examples .
    cp $ROOT/*.scm .
    """

  Scenario: Raise an error
    Given a file named "main.scm" with:
    """scheme
    (write-u8 65)

    (error "")

    (write-u8 65)
    """
    When I run the following script:
    """sh
    cat prelude.scm main.scm | compile.sh > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"

  Scenario: Print an error message
    Given a file named "main.scm" with:
    """scheme
    (error "Oh, no!")
    """
    When I run the following script:
    """sh
    cat prelude.scm main.scm | compile.sh > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "Oh, no!"
