Feature: let-syntax

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
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Define a local macro capturing a global value of the same name
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))
      
      (define foo 65)
      
      (let-syntax (
        (foo
          (syntax-rules ()
            ((_)
              foo))))
        (write-u8 (foo)))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Define a self-recursive local macro
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))
      
      (let-syntax (
        (foo
          (syntax-rules ()
            ((_ x)
              x)
            ((_ x y)
              (foo y)))))
        (write-u8 (foo 65 66)))
      """
    When I run `stak main.scm`
    Then the exit status should not be 0

  Scenario: Define a local macros shadowing a global macro
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))
      
      (define-syntax foo
        (syntax-rules ()
          ((_ x y)
            x)))
      
      (let-syntax (
        (foo
          (syntax-rules ()
            ((_ x y)
              y)))
        (bar
          (syntax-rules ()
            ((_ x y)
              (foo x y)))))
        (write-u8 (bar 65 66)))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

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
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

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
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

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
    When I run `stak main.scm`
    Then the exit status should not be 0

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
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

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
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

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
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

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
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Put a sequence in a body of `let-syntax`
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))
      
      (let-syntax ()
        (write-u8 65)
        (write-u8 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "AB"

  Scenario: Bind the same name as a local value
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))
      
      (let ((x 42))
        (let-syntax (
            (foo
              (syntax-rules ()
                ((_ y)
                  (let ((x y)) x)))))
          (write-u8 (foo 65))))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Bind the same name as a local macro
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))
      
      (let-syntax (
          (x
            (syntax-rules ()
              ((_) 42))))
        (let-syntax (
            (foo
              (syntax-rules ()
                ((_ y)
                  (let ((x y)) x)))))
          (write-u8 (foo 65))))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"
