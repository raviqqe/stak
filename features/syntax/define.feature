Feature: define
  Scenario: Define a recursive function
    Given a file named "main.scm" with:
    """scheme
    (define (sum x)
      (if (eq? x 0) 0 (+ x (sum (- x 1)))))

    (write-u8 (sum 11))
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "B"

  Scenario: Use a local variable in a definition
    Given a file named "main.scm" with:
    """scheme
    (define (f x)
      (let ((y x))
        (define z y)
        z))

    (write-u8 (f 65))
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"
