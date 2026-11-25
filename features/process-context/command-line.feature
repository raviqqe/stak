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

    Background:
      Given I run the following script:
        """sh
        cp $STAK_ROOT/prelude.scm .
        """
      And the exit status should be 0

    Scenario Outline: Show a version
      When I successfully run `<command> --version`
      Then the stdout should contain "<command> "

      Examples:
        | command        |
        | stak           |
        | stak-compile   |
        | stak-interpret |
        | stak-repl      |

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

    Scenario Outline: Set a heap size for bytecode
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base))
        """
      And I run the following script:
        """sh
        cat prelude.scm main.scm | stak-compile > main.bc
        """
      When I run `stak-interpret <option> main.bc`
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

    Scenario: Pass an option to bytecode
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base) (scheme process-context))

        (write-string (cadr (command-line)))
        """
      And I run the following script:
        """sh
        cat prelude.scm main.scm | stak-compile > main.bc
        """
      When I successfully run `stak-interpret main.bc --version`
      Then the stdout should contain exactly "--version"

    Scenario: Strip options from a command line of bytecode
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base) (scheme process-context) (scheme write))

        (write (command-line))
        """
      And I run the following script:
        """sh
        cat prelude.scm main.scm | stak-compile > main.bc
        """
      When I successfully run `stak-interpret -s 1000000 main.bc foo`
      Then the stdout should contain exactly:
        """
        ("main.bc" "foo")
        """

    Scenario: Interpret no bytecode file
      When I run `stak-interpret`
      Then the exit status should be 1
      And the stderr should contain "Error: bytecode file missing"

    Scenario: Interpret a nonexistent bytecode file
      When I run `stak-interpret foo.bc`
      Then the exit status should be 1
      And the stderr should contain "Error: "
