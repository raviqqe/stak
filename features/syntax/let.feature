Feature: let
  Scenario: Bind a variable
    Given a file named "main.scm" with:
    """scheme
    (write-u8 (let ((x 65)) x))
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"

  Scenario: Bind two variables
    Given a file named "main.scm" with:
    """scheme
    (write-u8 (let ((x 60) (y 5)) (+ x y)))
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"
