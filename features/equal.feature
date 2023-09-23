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
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "AAABBB"

  Scenario: Use an equal? procedure
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
