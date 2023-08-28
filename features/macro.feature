Feature: Macro
  Scenario: Define identity syntax
    Given a file named "main.scm" with:
    """scheme
    (define-syntax id
      (syntax-rules ()
        ((_ x)
          x)))

    (write-u8 (id 65))
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
