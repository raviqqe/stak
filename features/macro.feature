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

  Scenario: Match an ellipsis
    Given a file named "main.scm" with:
    """scheme
    (define-syntax foo
      (syntax-rules ()
        ((_ x ...)
          (x ...))))

    (foo write-u8 65)
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"

  Scenario: Match a succeeding ellipsis
    Given a file named "main.scm" with:
    """scheme
    (define-syntax foo
      (syntax-rules ()
        ((_ x ... y)
          (y x ...))))

    (foo 65 write-u8)
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"

  Scenario: Expand an ellipsis
    Given a file named "main.scm" with:
    """scheme
    (define-syntax foo
      (syntax-rules ()
        ((_ (x y) ...)
          (begin (x y) ...))))

    (foo
      (write-u8 65)
      (write-char #\B))
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "AB"

  Scenario: Match a literal identifier
    Given a file named "main.scm" with:
    """scheme
    (define-syntax my-if
      (syntax-rules (then else)
        ((_ x then y else z)
          (if x y z))))

    (write-u8 (my-if #f then 65 else 66))
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "B"
