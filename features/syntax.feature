Feature: Syntax
  Background:
    Given I run the following script:
    """sh
    cp -r $ROOT/examples .
    cp -r $ROOT/tools .
    cp $ROOT/*.scm .
    """

  Scenario: Use a negative integer
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

  Scenario: Use an if expression with false condition
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
    Then the stdout should contain exactly "B"

  Scenario: Use an if expression with true condition
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

  Scenario: Return a constant
    Given a file named "source.scm" with:
    """scheme
    (define (f) 65)
    (write-u8 (f))
    """
    When I run the following script:
    """sh
    cat prelude.scm source.scm | tools/compile.sh > main.out
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
    cat prelude.scm source.scm | tools/compile.sh > main.out
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
    cat prelude.scm source.scm | tools/compile.sh > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"

  Scenario: Add arguments
    Given a file named "source.scm" with:
    """scheme
    (define (f x y) (+ x y))
    (write-u8 (f 60 5))
    """
    When I run the following script:
    """sh
    cat prelude.scm source.scm | tools/compile.sh > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"

  Scenario: Use large (but not big) integers
    Given a file named "source.scm" with:
    """scheme
    (write-u8 (- 1065 1000))
    """
    When I run the following script:
    """sh
    cat prelude.scm source.scm | tools/compile.sh > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"

  Scenario: Use integers around the encoding base
    Given a file named "source.scm" with:
    """scheme
    (write-u8 (- 127 60))
    (write-u8 (- 128 60))
    (write-u8 (- 129 60))
    """
    When I run the following script:
    """sh
    cat prelude.scm source.scm | tools/compile.sh > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "CDE"

  Scenario: Use a let expression
    Given a file named "source.scm" with:
    """scheme
    (write-u8 (let ((x 65)) x))
    """
    When I run the following script:
    """sh
    cat prelude.scm source.scm | tools/compile.sh > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"

  Scenario: Use a let expression with two bindings
    Given a file named "source.scm" with:
    """scheme
    (write-u8 (let ((x 60) (y 5)) (+ x y)))
    """
    When I run the following script:
    """sh
    cat prelude.scm source.scm | tools/compile.sh > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"

  Scenario: Define a recursive function
    Given a file named "source.scm" with:
    """scheme
    (define (sum x)
      (if (eq? x 0) 0 (+ x (sum (- x 1)))))

    (write-u8 (sum 11))
    """
    When I run the following script:
    """sh
    cat prelude.scm source.scm | tools/compile.sh > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "B"
