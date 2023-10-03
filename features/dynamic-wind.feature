Feature: Dynamic wind
  Scenario: Run callbacks
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (dynamic-wind
      (lambda () (write-u8 65))
      (lambda () (write-u8 66))
      (lambda () (write-u8 67)))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "ABC"

  Scenario: Call an after callback on exit from dynamic extent
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define f #f)

    (define (g)
      (dynamic-wind
        (lambda () (write-u8 65))
        (lambda () (call/cc (lambda (k) (f #f))))
        (lambda () (write-u8 66))))

    (call/cc (lambda (k) (set! f k)))

    (g)
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "AB"
