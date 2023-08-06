Feature: Syntax
  Background:
    Given I run the following script:
    """sh
    cp -r $ROOT/examples .
    cp $ROOT/*.scm .
    """

  Scenario: Use a let expression
    Given a file named "source.scm" with:
    """scheme
    (write-u8 (let ((x 65)) x))
    """
    When I run the following script:
    """sh
    cat prelude.scm source.scm | ./main.scm > main.out
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
    cat prelude.scm source.scm | ./main.scm > main.out
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
    cat prelude.scm source.scm | ./main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "B"

  Scenario: Use a local variable in a definition
    Given a file named "source.scm" with:
    """scheme
    (define (f x)
      (let ((y x))
        (define z y)
        z))

    (write-u8 (f 65))
    """
    When I run the following script:
    """sh
    cat prelude.scm source.scm | ./main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"

  Scenario: Use a letrec expresion
    Given a file named "source.scm" with:
    """scheme
    (define (f x)
      (letrec (
          (f
            (lambda (x)
              (if (eqv? x 65)
                x
                (f (+ x 1))))))
        (f x)))

    (write-u8 (f 0))
    """
    When I run the following script:
    """sh
    cat prelude.scm source.scm | ./main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"

  Scenario: Use a letrec expresion with two bindings
    Given a file named "source.scm" with:
    """scheme
    (define (f x)
      (letrec (
          (f (lambda (x) (if (eqv? x 65) x (g (+ x 1)))))
          (g (lambda (x) (f x))))
        (f x)))

    (write-u8 (f 0))
    """
    When I run the following script:
    """sh
    cat prelude.scm source.scm | ./main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"

  Scenario: Call a function bound by a let expression
    Given a file named "source.scm" with:
    """scheme
    (define (f) 65)

    (define (g)
      (let ((h f))
        (h)))

    (write-u8 (g))
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
