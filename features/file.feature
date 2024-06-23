Feature: File
  Scenario Outline: Open a file
    Given a file named "main.scm" with:
      """scheme
      (import (scheme file))

      (<procedure> "foo.txt")
      """
    And a file named "foo.txt" with:
      """text
      """
    When I successfully run `scheme main.scm`
    Then the exit status should be 0

    Examples:
      | procedure               |
      | open-input-file         |
      | open-output-file        |
      | open-binary-input-file  |
      | open-binary-output-file |

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
      | procedure               |
      | open-input-file         |
      | open-output-file        |
      | open-binary-input-file  |
      | open-binary-output-file |

  Scenario Outline: Close an input file
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme file))

      (close-input-port (<procedure> "foo.txt"))
      """
    And a file named "foo.txt" with:
      """text
      """
    When I successfully run `scheme main.scm`
    Then the exit status should be 0

    Examples:
      | procedure              |
      | open-input-file        |
      | open-binary-input-file |

  Scenario Outline: Close an output file
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme file))

      (close-output-port (<procedure> "foo.txt"))
      """
    And a file named "foo.txt" with:
      """text
      """
    When I successfully run `scheme main.scm`
    Then the exit status should be 0

    Examples:
      | procedure               |
      | open-output-file        |
      | open-binary-output-file |

  @chibi @gauche @stak
  Scenario: Call a thunk with an input file
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme file))

      (write-u8 (with-input-from-file "foo.txt" read-u8))
      """
    And a file named "foo.txt" with:
      """text
      A
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Call a thunk with an output file
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme file))

      (with-output-to-file "foo.txt" (lambda () (write-u8 65)))
      """
    When I successfully run `scheme main.scm`
    Then a file named "foo.txt" should contain exactly:
      """
      A
      """

  Scenario: Call a procedure with an input file
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme file))

      (write-u8 (call-with-input-file "foo.txt" read-u8))
      """
    And a file named "foo.txt" with:
      """text
      A
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Call a procedure with an output file
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme file))

      (call-with-output-file "foo.txt" (lambda () (write-u8 65)))
      """
    When I successfully run `scheme main.scm`
    Then a file named "foo.txt" should contain exactly:
      """
      A
      """

  Scenario: Delete a file
    Given a file named "main.scm" with:
      """scheme
      (import (scheme file))

      (delete-file "foo.txt")
      """
    And a file named "foo.txt" with:
      """text
      """
    When I successfully run `scheme main.scm`
    Then a file named "foo.txt" should not exist

  Scenario Outline: Check if a file exists
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme file))

      (write-u8 (if (file-exists? "<path>") 65 66))
      """
    And a file named "foo.txt" with:
      """text
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | path    | output |
      | foo.txt | A      |
      | bar.txt | B      |
