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
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "BB"

  Scenario: Initialize a character in a list
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define x '(#\A))

    (write-u8 (char->integer #\A))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Compile symbols in an if expression in a function
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base) (scheme write))

    (define (foo)
      (if #f 'foo 'bar))

    (write-string (symbol->string (foo)))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "bar"

  @stak
  Scenario: Preserve a tag when a cdr is set
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base) (scheme write))

    (define x (rib #f #f 7))

    (write-u8 (+ 48 (rib-tag x)))
    (write-u8 (if (rib-cdr x) 65 66))

    (rib-set-cdr! x #t)

    (write-u8 (+ 48 (rib-tag x)))
    (write-u8 (if (rib-cdr x) 65 66))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "7B7A"

  @long
  Scenario: Compile many conditionals
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base) (scheme write))
    """
    And a file named "line.scm" with:
    """scheme
    (write-u8 (if #t (if #t 65 66) (if #t 67 68)))
    """
    And I run the following script:
    """sh
    for _ in $(seq 10000); do
      cat line.scm >> main.scm
    done
    """
    And the exit status should be 0
    When I successfully run `scheme main.scm`
    Then the exit status should be 0
