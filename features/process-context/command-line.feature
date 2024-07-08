Feature: Command line
  Scenario: Get a command line
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme process-context))

      (map write-string (command-line))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly ""
