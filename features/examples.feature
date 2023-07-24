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
    cat prelude.scm examples/<example>.scm | tools/compile.sh > main.out
    """
    Then I successfully run `stak main.out`

    Examples:
      | example              |
      | set-global-variable  |
      | set-global-variables |
      | get-global-variable  |
      | literals             |
      | rib-primitive        |
      | write-u8             |
