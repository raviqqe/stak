Feature: Examples
  Background:
    Given I run the following script:
    """sh
    cp -r $ROOT/examples .
    cp -r $ROOT/tools .
    cp $ROOT/*.scm .
    """

  Scenario Outline: Run examples
    When I run the following script:
    """sh
    tools/compile.sh < examples/<example>.scm > main.out
    """
    Then I successfully run `stak main.out`

    Examples:
      | example              |
      | set-global-variable  |
      | set-global-variables |
      | get-global-variable  |
      | literals             |
