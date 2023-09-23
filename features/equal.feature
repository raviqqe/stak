Feature: Equality
  Scenario: Use an eq? procedure
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define (test x y)
      (write-u8 (if (eq? x y) 65 66)))

    (test '() '())
    (test #f #f)
    (test #t #t)
    (test '() #f)
    (test #f #t)
    (test #t '())
    (test 42 42)
    (test 42 0)
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "AAABBBAB"

  Scenario: Use an equal? procedure with scalar values
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define (test x y)
      (write-u8 (if (equal? x y) 65 66)))

    (test '() '())
    (test #f #f)
    (test #t #t)
    (test '() #f)
    (test #f #t)
    (test #t '())
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "AAABBB"

  Scenario: Use an equal? procedure with collections
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define (test x y)
      (write-u8 (if (equal? x y) 65 66)))

    (test '() '())
    (test '(1) '(1))
    (test '(1 2) '(1 2))
    (test '(1 2 3) '(1 2 3))
    (test '(1 2 3) '(1 2 3 4))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "AAAAB"
