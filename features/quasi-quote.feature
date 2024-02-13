Feature: Quasi-quote
  Scenario: Quote a number
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 `65)
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Quote a list
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (for-each write-u8 `(65 66 67))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "ABC"

  Scenario: Unquote a number
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define x 65)
      (define y 66)
      (define z 67)

      (for-each write-u8 `(,x ,y ,z))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "ABC"

  Scenario: Unquote a list
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define x 65)
      (define y 66)
      (define z '(67))

      (for-each write-u8 `(,x ,y . ,z))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "ABC"

  Scenario: Unquote and splice a list
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define x '(65))

      (for-each write-u8 `(,@x))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Unquote and splice two lists
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define x '(65))
      (define y '(66))

      (for-each write-u8 `(,@x ,@y))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "AB"

  Scenario: Unquote and splice three lists
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define x '(65))
      (define y '(66))
      (define z '(67))

      (for-each write-u8 `(,@x ,@y ,@z))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "ABC"

  Scenario: Unquote and splice an expression
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define (f x) (list x))

      (for-each write-u8 `(,@(f 65)))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Capture a local variable
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define (foo x)
        `(,x))

      (for-each write-u8 (foo 65))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"
