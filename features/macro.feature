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
    When I successfully run `stak main.scm`
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
    When I successfully run `stak main.scm`
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
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

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
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "AB"

  Scenario: Match an ellipsis pattern
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define-syntax foo
        (syntax-rules ()
          ((_ x ...)
            (x ...))))

      (foo write-u8 65)
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Match a preceding ellipsis pattern
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define-syntax foo
        (syntax-rules ()
          ((_ x ... y)
            (y x ...))))

      (foo 65 write-u8)
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Match a succeeding ellipsis pattern
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define-syntax foo
        (syntax-rules ()
          ((_ x y ...)
            (x y ...))))

      (foo write-u8 65)
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Match an ellipsis pattern with an empty list
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define-syntax foo
        (syntax-rules ()
          ((_ x ...)
            (write-u8 65 x ...))))

      (foo)
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Match a nested ellipsis pattern
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define-syntax foo
        (syntax-rules ()
          ((_ (x ...) ...)
            (begin (x ...) ...))))

      (foo (write-u8 65) (write-u8 66 (current-output-port)))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "AB"

  Scenario: Match a deeply nested ellipsis pattern
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define-syntax foo
        (syntax-rules ()
          ((_ ((x ...) ...) ...)
            (begin (begin (x ...) ...) ...))))

      (foo
        ((write-u8 65))
        ((write-u8 66) (write-u8 67 (current-output-port))))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "ABC"

  Scenario: Expand an ellipsis pattern
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
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "AB"

  Scenario: Match a custom ellipsis pattern
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define-syntax foo
        (syntax-rules many ()
          ((_ x many)
            (x many))))

      (foo write-u8 65)
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Keep an original ellipsis with a custom ellipsis
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define-syntax foo
        (syntax-rules ::: ()
          ((_ name)
            (define name '(x ...)))))

      (foo y)

      (write-u8 (if (equal? y '(x ...)) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

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
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Match an improper list
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define-syntax foo
        (syntax-rules ()
          ((_ (x . y))
            y)))

      (write-u8 (foo (65 . 66)))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "B"

  Scenario: Match an ellipsis pattern and an improper list
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define-syntax foo
        (syntax-rules ()
          ((_ (x ... . y))
            y)))

      (write-u8 (foo (65 66 . 67)))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "C"

  Scenario: Expand an empty ellipsis pattern and an improper list
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme write))

      (define-syntax foo
        (syntax-rules ()
          ((_ (x y ... . z))
            '(y ... . z))))

      (write-u8 (foo (65 . 66)))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "B"

  Scenario: Match an ellipsis pattern to an improper list
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define-syntax foo
        (syntax-rules ()
          ((_ (x y ...))
            #f)))

      (foo (1 . 2))
      """
    When I run `stak main.scm`
    Then the exit status should not be 0

  Scenario: Expand an ellipsis pattern of an improper list
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define-syntax foo
        (syntax-rules ()
          ((_ (x . y) ...)
            (begin (define (x . y) y) ...))))

      (foo (f . x))

      (for-each write-u8 (f 65 66 67))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "ABC"

  Scenario: Expand a list with an ellipsis pattern
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define-syntax foo
        (syntax-rules ()
          ((_ x)
            (x ...))))

      (foo (write-u8 65))
      """
    When I run `stak main.scm`
    Then the exit status should not be 0

  Scenario: Expand ellipsis and singleton patterns
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define-syntax foo
        (syntax-rules ()
          ((_ x y ...)
            (begin (x y) ...))))

      (foo write-u8 65 66 67)
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "ABC"

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
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "B"

  Scenario: Expand a macro recursively
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define-syntax foo
        (syntax-rules ()
          ((_)
            (foo 65))
          ((_ x)
            x)
          ((_ x y)
            (foo))))

      (write-u8 (foo))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Expand a macro mutually recursively
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define-syntax foo
        (syntax-rules ()
          ((_)
            65)
          ((_ x y ...)
            (bar y ...))))

      (define-syntax bar
        (syntax-rules ()
          ((_)
            66)
          ((_ x y ...)
            (foo y ...))))

      (write-u8 (foo 1 2 3 4))
      """
    When I successfully run `stak main.scm`
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
    When I successfully run `stak main.scm`
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
    When I run `stak main.scm`
    Then the exit status should not be 0

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
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "B"

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
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

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
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

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
    When I run `stak main.scm`
    Then the exit status should not be 0

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
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

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
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

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
    When I run `stak main.scm`
    Then the exit status should not be 0

  @chibi @guile @stak
  Scenario: Use a macro as a value
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define-syntax foo
        (syntax-rules ()
          ((_)
            65)))

      foo
      """
    When I run `stak main.scm`
    Then the exit status should not be 0

  Scenario: Resolve denotations recursively
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define (id x)
        x)

      (define-syntax foo
        (syntax-rules ()
          ((_ y)
            (let ((x 65)) y))))

      (define (bar x)
        (foo (id x)))

      (write-u8 (bar 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "B"

  Scenario: Bind the same name as a global value
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define x 42)

      (define-syntax foo
        (syntax-rules ()
          ((_ y)
            (let ((x y)) x))))

      (write-u8 (foo 65))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Bind the same name as a global macro
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define-syntax x
         (syntax-rules ()
            ((_) 42)))

      (define-syntax foo
        (syntax-rules ()
          ((_ y)
            (let ((x y)) x))))

      (write-u8 (foo 65))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Raise a syntax error
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (lambda () (syntax-error "foo"))
      """
    When I run `stak main.scm`
    Then the exit status should not be 0
    And the stderr should contain "foo"

  Scenario: Re-define `define-syntax`
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define define-syntax 65)

      (write-u8 define-syntax)
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Re-define `...`
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define ... 65)

      (write-u8 ...)
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Define `_`
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define _ 65)

      (write-u8 _)
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Define a nested syntax
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define-syntax define-begin
        (syntax-rules ::: ()
          ((define-begin name)
            (define-syntax name
              (syntax-rules ()
                ((name x ...)
                  (begin x ...)))))))

      (define-begin sequence)

      (write-u8 (sequence 1 2 3 65))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Capture an undefined global variable
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define-syntax foo
        (syntax-rules ()
          ((_)
            x)))

      (define x 65)

      (write-u8 (foo))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Capture an undefined global variable in a nested syntax
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define-syntax define-foo
        (syntax-rules ()
          ((_ name)
            (define-syntax name
              (syntax-rules ()
                ((name)
                  x))))))

      (define-foo bar)

      (define x 65)

      (write-u8 (bar))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"
