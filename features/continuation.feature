Feature: Continuation
  Background:
    Given I run the following script:
    """sh
    cp -r $ROOT/examples .
    cp $ROOT/*.scm .
    """

  Scenario: Call a continuation
    Given a file named "source.scm" with:
    """scheme
    (write-u8 (call/cc (lambda (k) (k 65))))
    """
    When I run the following script:
    """sh
    cat prelude.scm source.scm | ./main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"

  Scenario: Return a value from a receiver
    Given a file named "source.scm" with:
    """scheme
    (write-u8 (call/cc (lambda (k) 65)))
    """
    When I run the following script:
    """sh
    cat prelude.scm source.scm | ./main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"
