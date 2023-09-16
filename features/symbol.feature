Feature: Symbol
  Scenario: Write a symbol
    Given a file named "main.scm" with:
    """scheme
    (write-string (symbol->string 'foo))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "foo"
