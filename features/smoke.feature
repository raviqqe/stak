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
      (if #t
        (list 'foo)
        (list 'bar)))

    (write (foo))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "foo"
