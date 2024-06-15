Feature: File
  Scenario Outline: Open a file
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

  @stak @chibi @gauche
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
