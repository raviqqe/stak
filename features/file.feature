Feature: File
  Scenario: Open an input file
    Given a file named "main.scm" with:
      """scheme
      (import (scheme file))

      (open-input-file "foo.txt")
      """
    And a file named "foo.txt" with:
      """text
      """
    When I successfully run `scheme main.scm`
    Then the exit status should be 0

  Scenario: Open an output file
    Given a file named "main.scm" with:
      """scheme
      (import (scheme file))

      (open-output-file "foo.txt")
      """
    And a file named "foo.txt" with:
      """text
      """
    When I successfully run `scheme main.scm`
    Then the exit status should be 0
