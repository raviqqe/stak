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
    When I successfully run `stak  main.scm`
    Then the stdout should contain exactly "BB"

  Scenario: Initialize a character in a list
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define x '(#\A))

      (write-u8 (char->integer #\A))
      """
    When I successfully run `stak  main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Compile symbols in an if expression in a procedure
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme write))

      (define (foo)
        (if #f 'foo 'bar))

      (write-string (symbol->string (foo)))
      """
    When I successfully run `stak  main.scm`
    Then the stdout should contain exactly "bar"

  @long
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
    When I successfully run `stak  main.scm`
    Then the exit status should be 0
