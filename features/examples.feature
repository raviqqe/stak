Feature: Examples
  Background:
    Given I run the following script:
    """
    cp -r $ROOT/examples .
    cp -r $ROOT/compile.scm .
    """

  Scenario Outline: Run examples
    When I successfully run `gsi compile.scm < examples/<example>.scm > main.out`
    Then I successfully run `stak main.out`

    Examples:
      | example         |
      | define-variable |
