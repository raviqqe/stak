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
      | procedure | output |
      | cadr      | foo    |
      | caddr     | bar    |
      | cadddr    | baz    |

  @extra
  Rule: Options

    Scenario Outline: Show a version
      When I successfully run `<command> --version`
      Then the stdout should contain "<command> "

      Examples:
        | command      |
        | stak         |
        | stak-compile |
        | stak-repl    |

    Scenario Outline: Set a heap size
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base))
        """
      When I run `stak main.scm <option>`
      Then the exit status should be 1
      And the stderr should contain "Error: out of memory"

      Examples:
        | option        |
        | -s 0          |
        | --heap-size 0 |

    Scenario: Pass an option to a script
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base) (scheme cxr) (scheme process-context))

        (write-string (caddr (command-line)))
        """
      When I successfully run `stak main.scm -- --version`
      Then the stdout should contain exactly "--version"
