Feature: Syntax
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
