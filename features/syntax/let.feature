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

  Scenario: Call a bound function
    Given a file named "main.scm" with:
    """scheme
    (define (f) 65)

    (define (g)
      (let ((h f))
        (h)))

    (write-u8 (g))
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"

  Scenario: Cause a side effect in a body
    Given a file named "main.scm" with:
    """scheme
    (write-u8
      (let ((x 66))
        (write-u8 65)
        x))
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "AB"

  Scenario: Do not corrupt a function environment
    Given a file named "main.scm" with:
    """scheme
    (define (f)
      (let (
          (g
            (let ((x 65))
              (lambda () x))))
        g))

    (write-u8 ((f)))
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"
