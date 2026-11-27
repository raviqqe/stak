@extra
Feature: Tools

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

  Scenario Outline: Set an invalid heap size for bytecode
    When I run `stak-interpret <option>`
    Then the exit status should be 1
    And the stderr should contain "Error: invalid heap size"

    Examples:
      | option                  |
      | -s                      |
      | -s foo                  |
      | -s 18446744073709551616 |

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
