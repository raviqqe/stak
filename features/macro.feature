Feature: Macro
  Scenario: Match a rule
    Given a file named "main.scm" with:
    """scheme
    (define-syntax foo
      (syntax-rules ()
        ((_ x)
          x)))

    (write-u8 (foo 65))
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"

  Scenario: Match rules
    Given a file named "main.scm" with:
    """scheme
    (define-syntax foo
      (syntax-rules ()
        ((_ x)
          x)
        ((_ x y)
          y)))

    (write-u8 (foo 65 66))
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "B"

  Scenario: Capture a free variable
    Given a file named "main.scm" with:
    """scheme
    (define x 65)

    (define-syntax modify
      (syntax-rules ()
        ((_)
          (set! x 66))))

    (let ((x 65))
      (modify)
      (write-u8 x))

    (modify)
    (write-u8 x)
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    # TODO Fix this to "AB".
    Then the stdout should contain exactly "BB"
