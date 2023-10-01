Feature: Record
  Scenario: Define a record type
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define-record-type foo
      (make-foo)
      foo?)
    """
    When I successfully run `scheme main.scm`
    Then the exit status should be 0
