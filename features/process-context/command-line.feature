@command-line
Feature: Command line

  Scenario: Get an argument
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme process-context))

      (map write-string (command-line))
      """
    When I successfully run `stak main.scm hello`
    Then the stdout should contain "hello"

  Scenario: Get two arguments
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme process-context))

      (map write-string (command-line))
      """
    When I successfully run `stak main.scm hello world`
    Then the stdout should contain "hello"
    And the stdout should contain "world"

  Scenario Outline: Get an argument at an index
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme cxr) (scheme process-context))

      (write-string (<procedure> (command-line)))
      """
    When I successfully run `stak main.scm foo bar baz`
    Then the stdout should contain "<output>"

    Examples:
      | procedure | output   |
      | car       | main.scm |
      | cadr      | foo      |
      | caddr     | bar      |
      | cadddr    | baz      |
