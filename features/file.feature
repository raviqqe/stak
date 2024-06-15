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

  Scenario: Close an input file
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme file))

      (close-input-port (open-input-file "foo.txt"))
      """
    And a file named "foo.txt" with:
      """text
      """
    When I successfully run `scheme main.scm`
    Then the exit status should be 0

  Scenario: Close an output file
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme file))

      (close-output-port (open-output-file "foo.txt"))
      """
    And a file named "foo.txt" with:
      """text
      """
    When I successfully run `scheme main.scm`
    Then the exit status should be 0

  Scenario Outline: Close a file
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme file))

      (close-port (<procedure> "foo.txt"))
      """
    And a file named "foo.txt" with:
      """text
      """
    When I successfully run `scheme main.scm`
    Then the exit status should be 0

    Examples:
      | procedure        |
      | open-input-file  |
      | open-output-file |
