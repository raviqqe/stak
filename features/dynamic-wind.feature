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

  Scenario: Call a before callback on an entrance into a dynamic extent
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define f #f)

      (define (g)
        (dynamic-wind
          (lambda () (write-u8 65))
          (lambda () (call/cc (lambda (k) (set! f k))))
          (lambda () (write-u8 66))))

      (g)

      (when f
        (let ((g f))
          (set! f #f)
          (g #f)))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "ABAB"

  Scenario: Call an after callback on an exit from a dynamic extent
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define f #f)

      (define (g)
        (dynamic-wind
          (lambda () (write-u8 65))
          (lambda () (f #f))
          (lambda () (write-u8 66))))

      (call/cc
        (lambda (k)
          (set! f k)
          (g)))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "AB"

  Scenario: Call callbacks for nested dynamic extents
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define f #f)

      (define (g)
        (dynamic-wind
          (lambda () (write-u8 65))
          (lambda ()
            (dynamic-wind
              (lambda () (write-u8 66))
              (lambda () (call/cc (lambda (k) (set! f k))))
              (lambda () (write-u8 67))))
          (lambda () (write-u8 68))))

      (g)

      (when f
        (let ((g f))
          (set! f #f)
          (g #f)))
      """
    When I successfully run `scheme main.scm`
    # spell-checker: disable-next-line
    Then the stdout should contain exactly "ABCDABCD"

  Scenario: Call callbacks for nested dynamic extents
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define f #f)
      (define g #f)

      (define (h)
        (dynamic-wind
          (lambda () (write-u8 65))
          (lambda ()
            (dynamic-wind
              (lambda () (write-u8 66))
              (lambda ()
                (call/cc (lambda (k) (set! f k)))
                (when g (g #f)))
              (lambda () (write-u8 67))))
          (lambda () (write-u8 68)))
        (dynamic-wind
          (lambda () (write-u8 69))
          (lambda ()
            (dynamic-wind
              (lambda () (write-u8 70))
              (lambda () (call/cc (lambda (k) (set! g k))))
              (lambda () (write-u8 71))))
          (lambda () (write-u8 72))))

      (h)

      (when f
        (let ((h f))
          (set! f #f)
          (h #f)))
      """
    When I successfully run `scheme main.scm`
    # spell-checker: disable-next-line
    Then the stdout should contain exactly "ABCDEFGHABCDEFGH"
