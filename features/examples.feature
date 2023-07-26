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
      | extra-literals       |
      | rib                  |
      | write-u8             |
      | if                   |
      # TODO | lambda               |

  Scenario Outline: Run examples
		Given a file named "main.scm" with:
		"""scheme
		(define x -1)
		(write-u8 (+ 66 x))
		"""
    When I run the following script:
    """sh
    cat prelude.scm main.scm | tools/compile.sh > main.out
    """
    And I successfully run `stak main.out`
		Then the output should exactly be "A"
