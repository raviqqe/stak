Feature: Smoke
  Scenario: Initialize constants in a correct order
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define a #\A)

      (for-each
        (lambda (x) (write-u8 (if (not x) 65 66)))
        '(#\A #\B))

      (define b #\B)
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "BB"

  Scenario: Initialize a character in a list
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define x '(#\A))

      (write-u8 (char->integer #\A))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Compile symbols in an if expression in a procedure
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme write))

      (define (foo)
        (if #f 'foo 'bar))

      (write-string (symbol->string (foo)))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "bar"

  @chibi @guile @long @stak
  Scenario: Compile many sequential `if` expressions
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme write))
      """
    And a file named "foo.scm" with:
      """scheme
      (write-u8 (if #t (if #t 65 66) (if #t 67 68)))
      """
    And I run the following script:
      """sh
      for _ in $(seq 10); do
        for _ in $(seq 2); do
          cat foo.scm >> bar.scm
        done

        cp bar.scm foo.scm
        rm bar.scm
      done

      cat foo.scm >> main.scm
      """
    And the exit status should be 0
    When I successfully run `stak main.scm`
    Then the exit status should be 0

  Scenario: Escape a `$$begin` symbol in a list
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define x '($$begin))
      """
    When I successfully run `stak main.scm`
    Then the exit status should be 0

  Scenario: Escape a `$$begin` symbol in a list with other elements
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define xs '($$begin 65 66))

      (write-u8 (cadr xs))
      (write-u8 (cadr (cdr xs)))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "AB"

  Scenario: Define a record type at the beginning of a function body
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define x 65)

      (define (foo)
        (define-record-type bar
          (make-bar)
          bar?)
        (define x 66)
        #f)

      (foo)
      (write-u8 x)
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Define a record type in the middle of a function body
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define x 65)

      (define (foo)
        (define y 0)
        (define-record-type bar
          (make-bar)
          bar?)
        (define x 66)
        #f)

      (foo)
      (write-u8 x)
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"
