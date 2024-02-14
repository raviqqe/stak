Feature: Exit
  Rule: `exit`
    Scenario: Exit an interpreter
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base) (scheme process-context))

        (exit)

        (write-u8 65)
        """
      When I successfully run `scheme main.scm`
      Then the stdout should contain exactly ""

    Scenario: Exit an interpreter with a true value
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base) (scheme process-context))

        (exit #t)
        """
      When I successfully run `scheme main.scm`
      Then the exit status should be 0

    Scenario: Exit an interpreter with a false value
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base) (scheme process-context))

        (exit #f)
        """
      When I run `scheme main.scm`
      Then the exit status should not be 0

    @stak @gauche @guile
    Scenario: Leave a dynamic extent
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base) (scheme process-context))

        (dynamic-wind
          (lambda () (write-u8 65))
          (lambda () (exit))
          (lambda () (write-u8 66)))
        """
      When I successfully run `scheme main.scm`
      Then the stdout should contain exactly "AB"

  Rule: `emergency-exit`
    Scenario: Exit an interpreter
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base) (scheme process-context))

        (emergency-exit)

        (write-u8 65)
        """
      When I successfully run `scheme main.scm`
      Then the stdout should contain exactly ""

    @stak @chibi @guile
    Scenario: Exit an interpreter with a true value
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base) (scheme process-context))

        (emergency-exit #t)
        """
      When I successfully run `scheme main.scm`
      Then the exit status should be 0

    Scenario: Exit an interpreter with a false value
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base) (scheme process-context))

        (emergency-exit #f)
        """
      When I run `scheme main.scm`
      Then the exit status should not be 0

    @stak @gauche @guile
    Scenario: Leave a dynamic extent
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base) (scheme process-context))

        (dynamic-wind
          (lambda () #f)
          (lambda () (emergency-exit))
          (lambda () (write-u8 65)))
        """
      When I successfully run `scheme main.scm`
      Then the stdout should contain exactly ""
