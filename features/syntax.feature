Feature: Syntax
  Background:
    Given I run the following script:
    """sh
    cp -r $ROOT/examples .
    cp -r $ROOT/tools .
    cp $ROOT/*.scm .
    """

  Scenario Outline: Use a negative integer
    Given a file named "source.scm" with:
    """scheme
    (define x -1)
    (write-u8 (+ 66 x))
    """
    When I run the following script:
    """sh
    cat prelude.scm source.scm | tools/compile.sh > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"

  Scenario Outline: Use an if expression with false condition
    Given a file named "source.scm" with:
    """scheme
    (if #f
      (write-u8 65)
      (write-u8 66))
    """
    When I run the following script:
    """sh
    cat prelude.scm source.scm | tools/compile.sh > main.out
    """
    And I successfully run `stak main.out`

  Scenario Outline: Use an if expression with true condition
    Given a file named "source.scm" with:
    """scheme
    (if #t
      (write-u8 65)
      (write-u8 66))
    """
    When I run the following script:
    """sh
    cat prelude.scm source.scm | tools/compile.sh > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"
