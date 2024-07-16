@command-line
Feature: Command line
  Scenario: Get an argument
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme process-context))

      (map write-string (command-line))
      """
    When I successfully run `scheme main.scm hello`
    Then the stdout should contain "hello"

  Scenario: Get two arguments
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme process-context))

      (map write-string (command-line))
      """
    When I successfully run `scheme main.scm hello world`
    Then the stdout should contain "hello"
    And the stdout should contain "world"
