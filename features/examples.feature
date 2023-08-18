Feature: Examples
  Background:
    Given I run the following script:
    """sh
    cp -r $ROOT/examples .
    cp $ROOT/*.scm .
    """

  Scenario Outline: Run examples
    When I run the following script:
    """sh
    cat prelude.scm examples/<example>.scm | compile.sh > main.out
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
      | lambda               |

  Scenario: Call a fibonacci function
    Given a file named "main.scm" with:
    """scheme
    (define (fibonacci x)
      (if (< x 2)
        x
        (+
          (fibonacci (- x 1))
          (fibonacci (- x 2)))))

    (write-u8 (+ 33 (fibonacci 10)))
    """
    When I run the following script:
    """sh
    cat prelude.scm main.scm | compile.sh > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "X"
