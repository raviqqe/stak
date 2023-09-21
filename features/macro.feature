Feature: Macro
  Scenario: Match a rule
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define-syntax foo
      (syntax-rules ()
        ((_ x)
          x)))

    (write-u8 (foo 65))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Match rules
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define-syntax foo
      (syntax-rules ()
        ((_ x)
          x)
        ((_ x y)
          y)))

    (write-u8 (foo 65 66))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "B"

  Scenario: Match a nested pattern
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define-syntax my-cond
      (syntax-rules (else)
        ((_ (condition then-result) (else else-result))
          (if condition then-result else-result))))

    (write-u8
      (my-cond
        (#t
          65)
        (else
          66)))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  @stak
  Scenario: Capture a free variable
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define x 65)

    (define-syntax modify
      (syntax-rules ()
        ((_)
          (set! x 66))))

    (let ((x 65))
      (modify)
      (write-u8 x))

    (modify)
    (write-u8 x)
    """
    When I successfully run `scheme main.scm`
    # TODO Fix this to "AB".
    Then the stdout should contain exactly "BB"

  Scenario: Match an ellipsis
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define-syntax foo
      (syntax-rules ()
        ((_ x ...)
          (x ...))))

    (foo write-u8 65)
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  @advanced
  Scenario: Match a succeeding ellipsis
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define-syntax foo
      (syntax-rules ()
        ((_ x ... y)
          (y x ...))))

    (foo 65 write-u8)
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Match an ellipsis with an empty list
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define-syntax foo
      (syntax-rules ()
        ((_ x ...)
          (write-u8 65 x ...))))

    (foo)
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Expand an ellipsis
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define-syntax foo
      (syntax-rules ()
        ((_ (x y) ...)
          (begin (x y) ...))))

    (foo
      (write-u8 65)
      (write-char #\B))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "AB"

  Scenario: Match two ellipses at different levels
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define-syntax plus
      (syntax-rules ()
        ((_ (x y ...) v w ...)
          (+ x v))))

    (write-u8
      (plus
        (60 "foo")
        5
        "bar"))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Match an improper list
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define-syntax foo
      (syntax-rules ()
        ((_ . x)
          x)))

    (write-u8 (foo . 65))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Match an ellipsis and an improper list
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define-syntax foo
      (syntax-rules ()
        ((_ x ... . y)
          y)))

    (write-u8 (foo 65 66 . 67))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "C"

  Scenario: Match an ellipsis to an improper list
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define-syntax foo
      (syntax-rules ()
        ((_ x ...)
          (x ...))))

    (foo . 65)
    """
    When I run `scheme main.scm`
    Then the exit status should not be 0

  Scenario: Match a literal identifier
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define-syntax my-if
      (syntax-rules (then else)
        ((_ x then y else z)
          (if x y z))))

    (write-u8 (my-if #f then 65 else 66))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "B"

  Scenario: Expand a macro recursively
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define-syntax foo
      (syntax-rules ()
        ((_) (foo 65))
        ((_ x) x)
        ((_ x y) (foo))))

    (write-u8 (foo))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Expand a spread variable with a constant
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define-syntax foo
      (syntax-rules ()
        ((_ x ...)
          (let ((x #f) ...)
            65))))

    (write-u8 (foo x y z))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Throw an error if no rule matches
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define-syntax foo
      (syntax-rules ()
        ((_) #f)))

    (foo 42)
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    Then the exit status should not be 0

  @advanced
  Scenario: Define a local macro
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (let-syntax (
      (foo
        (syntax-rules ()
          ((_ x)
            x))))
      (write-u8 (foo 65)))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  @advanced
  Scenario: Define a recursive local macro
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (letrec-syntax
      ((foo
        (syntax-rules ()
          ((_ x)
            x)
          ((_ x y)
            (foo y)))))
      (write-u8 (foo 65 66)))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "B"

  @advanced
  Scenario: Define a mutually recursive local macro
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (letrec-syntax (
      (foo
        (syntax-rules ()
          ((_ x)
            x)
          ((_ x ... y)
            (bar x ...))))
      (bar
        (syntax-rules ()
          ((_ x)
            x)
          ((_ x ... y)
            (foo x ...)))))
      (write-u8 (foo 65 66 67)))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  @advanced
  Scenario: Define a recursive local macro in a body
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (let ()
      (define-syntax foo
        (syntax-rules ()
          ((_ x)
            x)
          ((_ x y)
            (foo y))))

      (write-u8 (foo 65 66)))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "B"

  @advanced
  Scenario: Define a mutually recursive local macro in a body
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (let ()
      (define-syntax foo
        (syntax-rules ()
          ((_ x)
            x)
          ((_ x ... y)
            (bar x ...))))

      (define-syntax bar
        (syntax-rules ()
          ((_ x)
            x)
          ((_ x ... y)
            (foo x ...))))

      (write-u8 (foo 65 66 67)))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  @advanced
  Scenario: Shadow a global value by a global macro
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define foo 42)

    (define-syntax foo
      (syntax-rules ()
        ((_ x)
          x)))

    (write-u8 (foo 65))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  @advanced
  Scenario: Use a global macro as a shadowed value
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define foo 65)

    (define-syntax foo
      (syntax-rules ()
        ((_ x)
          x)))

    (write-u8 foo)
    """
    When I run `scheme main.scm`
    Then the exit status should not be 0

  @advanced
  Scenario: Use a higher-order macro
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define-syntax foo
      (syntax-rules ()
        ((_ x y)
          (x y))))

    (let-syntax (
        (bar
          (syntax-rules ()
            ((_ x)
              x))))
      (write-u8 (foo bar 65)))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  @advanced
  Scenario: Shadow a global macro by a global value
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define-syntax foo
      (syntax-rules ()
        ((_ x)
          x)))

    (define foo 65)

    (write-u8 foo)
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  @advanced
  Scenario: Shadow a local value by a local macro
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (let ((foo 42))
      (let-syntax (
        (foo
          (syntax-rules ()
            ((_ x)
              x))))
        (write-u8 (foo 65))))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  @advanced
  Scenario: Use a local macro as a shadowed value
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (let ((foo 65))
      (let-syntax (
        (foo
          (syntax-rules ()
            ((_ x)
              x))))
        (write-u8 foo)))
    """
    When I run `scheme main.scm`
    Then the exit status should not be 0

  @advanced
  Scenario: Shadow a local macro by a local value
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (let-syntax (
      (foo
        (syntax-rules ()
          ((_ x)
            x))))
      (let ((foo 65))
        (write-u8 foo)))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  @advanced
  Scenario: Shadow a literal by a global value
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define-syntax foo
      (syntax-rules (bar)
        ((_ bar x)
          x)))

    (define bar #f)

    (write-u8 (foo bar 65))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  @advanced
  Scenario: Shadow a literal by a local value
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define-syntax foo
      (syntax-rules (bar)
        ((_ bar x)
          x)))

    (let ((bar #f))
      (write-u8 (foo bar 65)))
    """
    When I run `scheme main.scm`
    Then the exit status should not be 0

  @advanced
  Scenario: Capture a local value in a local macro
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (let ((x 65))
      (let-syntax (
          (foo
            (syntax-rules ()
              ((_)
                x))))
        (let ((x 66))
          (write-u8 (foo)))))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  @advanced
  Scenario: Capture a local macro in a local macro
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (let-syntax (
        (foo
          (syntax-rules ()
            ((_)
              65))))
      (let-syntax (
          (bar
            (syntax-rules ()
              ((_)
                (foo)))))
        (let ((foo #f))
          (write-u8 (bar)))))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  @advanced
  Scenario: Capture a local macro in a local macro of the same name
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (let-syntax (
        (foo
          (syntax-rules ()
            ((_)
              65))))
      (let-syntax (
          (foo
            (syntax-rules ()
              ((_)
                (foo)))))
        (write-u8 (foo))))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"
