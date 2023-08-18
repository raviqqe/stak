Feature: Continuation
  Scenario: Call a continuation
    Given a file named "main.scm" with:
    """scheme
    (write-u8 (call/cc (lambda (k) (k 65))))
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"

  Scenario: Call a continuation with a global variable
    Given a file named "main.scm" with:
    """scheme
    (define x 5)

    (write-u8 (+ 60 (call/cc (lambda (k) (k x)))))
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"

  Scenario: Call a continuation with a local variable
    Given a file named "main.scm" with:
    """scheme
    (define (f x) (call/cc (lambda (k) (k x))))

    (write-u8 (+ 60 (f 5)))
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"

  Scenario: Return a value from a receiver
    Given a file named "main.scm" with:
    """scheme
    (write-u8 (call/cc (lambda (k) 65)))
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"
