Feature: Record
  Rule: Record with no field
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

    Scenario: Make a record
      Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define-record-type foo
        (make-foo)
        foo?)

      (make-foo)
      """
      When I successfully run `scheme main.scm`
      Then the exit status should be 0

    Scenario: Check a record type
      Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define-record-type foo
        (make-foo)
        foo?)

      (write-u8 (if (foo? (make-foo)) 65 66))
      """
      When I successfully run `scheme main.scm`
      Then the stdout should contain exactly "A"

  Rule: Record with fields
    Scenario: Define a record type
      Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define-record-type foo
        (make-foo x y)
        foo?
        (x foo-x)
        (y foo-y foo-set-y!))
      """
      When I successfully run `scheme main.scm`
      Then the exit status should be 0

    Scenario: Make a record
      Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define-record-type foo
        (make-foo x y)
        foo?
        (x foo-x)
        (y foo-y foo-set-y!))

      (make-foo 1 2)
      """
      When I successfully run `scheme main.scm`
      Then the exit status should be 0

    Scenario: Check a record type
      Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define-record-type foo
        (make-foo)
        foo?)

      (write-u8 (if (foo? (make-foo 1 2)) 65 66))
      """
      When I successfully run `scheme main.scm`
      Then the stdout should contain exactly "A"

    Scenario: Make a record
      Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define-record-type foo
        (make-foo x y)
        foo?
        (x foo-x)
        (y foo-y foo-set-y!))

      (define record (make-foo 65 66))

      (write-u8 (foo-x record))
      (write-u8 (foo-y record))
      """
      When I successfully run `scheme main.scm`
      Then the stdout should contain exactly "AB"
