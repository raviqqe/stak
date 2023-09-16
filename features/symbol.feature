Feature: Symbol
  Scenario: Write a symbol
    Given a file named "main.scm" with:
    """scheme
    (write-string (symbol->string 'foo))
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "foo"
