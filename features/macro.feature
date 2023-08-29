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

  Scenario: Match a nested pattern
    Given a file named "main.scm" with:
    """scheme
    (define-syntax my-cond
      (syntax-rules (else)
        ((_ (condition then-result) (else else-result))
          (if condition then-result else-result))))

    (write-u8
      (my-cond
        (#t
          65)
        (else
          66)))
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"

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

  Scenario: Match an ellipsis with an empty list
    Given a file named "main.scm" with:
    """scheme
    (define-syntax foo
      (syntax-rules ()
        ((_ x ...)
          (write-u8 65 x ...))))

    (foo)
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

  Scenario: Match two ellipses at different levels
    Given a file named "main.scm" with:
    """scheme
    (define-syntax plus
      (syntax-rules ()
        ((_ (x y ...) v w ...)
          (+ x v))))

    (write-u8
      (plus
        (60 "foo")
        5
        "bar"))
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"

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

  Scenario: Expand a macro recursively
    Given a file named "main.scm" with:
    """scheme
    (define-syntax foo
      (syntax-rules ()
        ((_) (foo 65))
        ((_ x) x)
        ((_ x y) (foo))))

    (write-u8 (foo))
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"

  Scenario: Expand a spread variable with a constant
    Given a file named "main.scm" with:
    """scheme
    (define-syntax foo
      (syntax-rule ()
        ((_ x ...)
          (let ((x #f) ...)
            65))))

    (write-u8 (foo x y z))
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"

  Scenario: Throw an error if no rule matches
    Given a file named "main.scm" with:
    """scheme
    (define-syntax foo
      (syntax-rules ()
        ((_) #f)))

    (foo 42)
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    Then the exit status should not be 0
