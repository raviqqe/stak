Feature: Multiple values
  Scenario: Pass multiple values to a continuation
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8
        (call-with-values
          (lambda () (values 1 4 60))
          (lambda (x y z) (+ x y z))))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Call `call-with-values` with a value
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (+ 66 (call-with-values * -)))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

  Rule: `define-values`
    Scenario: Define no value
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base))

        (define-values () (values))
        """
      When I successfully run `stak main.scm`
      Then the exit status should be 0

    Scenario: Define a value
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base))

        (define-values (x) (values 65))

        (write-u8 x)
        """
      When I successfully run `stak main.scm`
      Then the stdout should contain exactly "A"

    Scenario: Define multiple values
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base))

        (define-values (x y z) (values 65 66 67))

        (write-u8 x)
        (write-u8 y)
        (write-u8 z)
        """
      When I successfully run `stak main.scm`
      Then the stdout should contain exactly "ABC"

    Scenario: Define a list from values
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base) (scheme cxr))

        (define-values xs (values 65 66 67))

        (write-u8 (car xs))
        (write-u8 (cadr xs))
        (write-u8 (caddr xs))
        """
      When I successfully run `stak main.scm`
      Then the stdout should contain exactly "ABC"

    Scenario: Define values and a list from values
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base))

        (define-values (x . xs) (values 65 66 67))

        (write-u8 x)
        (write-u8 (car xs))
        (write-u8 (cadr xs))
        """
      When I successfully run `stak main.scm`
      Then the stdout should contain exactly "ABC"

    Scenario: Define values in a definition
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base))

        (define (f)
          (values 60 5))

        (define (g)
          (define-values (x y) (f))
          (+ x y))

        (write-u8 (g))
        """
      When I successfully run `stak main.scm`
      Then the stdout should contain exactly "A"

    Scenario: Define values in a definition multiple times
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base))

        (define (f x)
          (values 65 x))

        (define (g)
          (define-values (x y) (f 0))
          (define-values (v w) (f 1))

          (write-u8 (+ x y))
          (write-u8 (+ v w)))

        (g)
        """
      When I successfully run `stak main.scm`
      Then the stdout should contain exactly "AB"

    Scenario: Define values in a definition multiple times sequentially
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base))

        (define (f x)
          (values 65 x))

        (define (g)
          (define-values (x y) (f 0))
          (define-values (v w) (f (+ y 1)))

          (write-u8 (+ x y))
          (write-u8 (+ v w)))

        (g)
        """
      When I successfully run `stak main.scm`
      Then the stdout should contain exactly "AB"

  Rule: `let-values`
    Scenario: Define no value
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base))

        (let-values ((() (values)))
          #f)
        """
      When I successfully run `stak main.scm`
      Then the exit status should be 0

    Scenario: Define a value
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base))

        (let-values (((x) (values 65)))
          (write-u8 x))
        """
      When I successfully run `stak main.scm`
      Then the stdout should contain exactly "A"

    Scenario: Define multiple values
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base))

        (let-values (((x y z) (values 65 66 67)))
          (write-u8 x)
          (write-u8 y)
          (write-u8 z))
        """
      When I successfully run `stak main.scm`
      Then the stdout should contain exactly "ABC"

    Scenario: Define a list from values
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base) (scheme cxr))

        (let-values ((xs (values 65 66 67)))
          (write-u8 (car xs))
          (write-u8 (cadr xs))
          (write-u8 (caddr xs)))
        """
      When I successfully run `stak main.scm`
      Then the stdout should contain exactly "ABC"

    Scenario: Define values and a list from values
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base))

        (let-values (((x . xs) (values 65 66 67)))
          (write-u8 x)
          (write-u8 (car xs))
          (write-u8 (cadr xs)))
        """
      When I successfully run `stak main.scm`
      Then the stdout should contain exactly "ABC"
