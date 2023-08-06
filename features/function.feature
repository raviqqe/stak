Feature: Function
  Background:
    Given I run the following script:
    """sh
    cp -r $ROOT/examples .
    cp $ROOT/*.scm .
    """

  Scenario: Call a global function
    Given a file named "source.scm" with:
    """scheme
    (define (f x) (+ x 5))

    (write-u8 (f 60))
    """
    When I run the following script:
    """sh
    cat prelude.scm source.scm | ./main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"

  Scenario: Call a local function
    Given a file named "source.scm" with:
    """scheme
    (let ((f (lambda (x) (+ x 5))))
      (write-u8 (f 60)))
    """
    When I run the following script:
    """sh
    cat prelude.scm source.scm | ./main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"

  Scenario: Call an immediate function
    Given a file named "source.scm" with:
    """scheme
    (write-u8 (+ 60 ((lambda (x) x) 5)))
    """
    When I run the following script:
    """sh
    cat prelude.scm source.scm | ./main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"

  Scenario: Return a constant
    Given a file named "source.scm" with:
    """scheme
    (define (f) 65)
    (write-u8 (f))
    """
    When I run the following script:
    """sh
    cat prelude.scm source.scm | ./main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"

  Scenario: Return the first argument
    Given a file named "source.scm" with:
    """scheme
    (define (f x) x)
    (write-u8 (f 65))
    """
    When I run the following script:
    """sh
    cat prelude.scm source.scm | ./main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"

  Scenario: Return the second argument
    Given a file named "source.scm" with:
    """scheme
    (define (f x y) y)
    (write-u8 (f 66 65))
    """
    When I run the following script:
    """sh
    cat prelude.scm source.scm | ./main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"

  Scenario: Compute a value with arguments
    Given a file named "source.scm" with:
    """scheme
    (define (f x y) (+ x y))
    (write-u8 (f 60 5))
    """
    When I run the following script:
    """sh
    cat prelude.scm source.scm | ./main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"

  Scenario: Update a captured variable in a closure
    Given a file named "source.scm" with:
    """scheme
    (define (f x) (lambda () (set! x (+ x 1)) x))
    (define g (f 64))

    (write-u8 (g))
    (write-u8 (g))
    (write-u8 (g))
    """
    When I run the following script:
    """sh
    cat prelude.scm source.scm | ./main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "ABC"
