Feature: Symbol
  Scenario: Write a symbol
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-string (symbol->string 'foo))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "foo"

  Scenario: Convert a string to an existing symbol
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (eq? (string->symbol "foo") 'foo) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Convert a string to a new symbol
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (eq? (string->symbol "foo") (string->symbol "foo")) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"
