Feature: Exit

  Scenario Outline: Exit an interpreter
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme process-context))

      (<procedure>)

      (write-u8 65)
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly ""

    Examples:
      | procedure      |
      | exit           |
      | emergency-exit |

  Scenario Outline: Exit an interpreter with a true value
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme process-context))

      (<procedure> #t)
      """
    When I successfully run `stak main.scm`
    Then the exit status should be 0

    Examples:
      | procedure      |
      | exit           |
      | emergency-exit |

  Scenario Outline: Exit an interpreter with a false value
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme process-context))

      (<procedure> #f)
      """
    When I run `stak main.scm`
    Then the exit status should not be 0

    Examples:
      | procedure      |
      | exit           |
      | emergency-exit |

  Scenario Outline: Exit an interpreter with zero
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme process-context))

      (<procedure> 0)
      """
    When I successfully run `stak main.scm`
    Then the exit status should be 0

    Examples:
      | procedure      |
      | exit           |
      | emergency-exit |

  @chibi @gauche @stak
  Scenario Outline: Exit an interpreter with a non-zero integer
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme process-context))

      (<procedure> 42)
      """
    When I run `stak main.scm`
    Then the exit status should be 42

    Examples:
      | procedure      |
      | exit           |
      | emergency-exit |

  @chibi @gauche @stak
  Scenario Outline: Exit an interpreter with a non-integer value
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme process-context))

      (<procedure> 'foo)
      """
    When I run `stak main.scm`
    Then the exit status should not be 0

    Examples:
      | procedure      |
      | exit           |
      | emergency-exit |

  Rule: `exit`

    Scenario: Leave a dynamic extent
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base) (scheme process-context))

        (dynamic-wind
          (lambda () (write-u8 65))
          (lambda () (exit))
          (lambda () (write-u8 66)))
        """
      When I successfully run `stak main.scm`
      Then the stdout should contain exactly "AB"

  Rule: `emergency-exit`

    Scenario: Leave a dynamic extent
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base) (scheme process-context))

        (dynamic-wind
          (lambda () #f)
          (lambda () (emergency-exit))
          (lambda () (write-u8 65)))
        """
      When I successfully run `stak main.scm`
      Then the stdout should contain exactly ""
