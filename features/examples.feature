Feature: Examples
  Background:
    Given I run the following script:
    """sh
    cp -r $ROOT/examples .
    cp $ROOT/compile.scm .
    """

  Scenario Outline: Run examples
		When I run the following script:
    """sh
    gsi compile.scm < examples/<example>.scm > main.out
    """
    Then I successfully run `stak main.out`

    Examples:
      | example         |
      | define-variable |
