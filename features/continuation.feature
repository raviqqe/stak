Feature: Continuation
  Scenario: Call a continuation
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (call/cc (lambda (k) (k 65))))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Call a continuation with a global variable
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define x 5)

      (write-u8 (+ 60 (call/cc (lambda (k) (k x)))))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Call a continuation with a local variable
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define (f x) (call/cc (lambda (k) (k x))))

      (write-u8 (+ 60 (f 5)))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Return a value from a receiver
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (call/cc (lambda (k) 65)))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Modify environment
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define backtrack #f)

      (let ((i 65))
        (call/cc
          (lambda (target)
            (set! backtrack target)
            #f))
        (write-u8 i)
        (set! i (+ i 1))
        (unless (< i 91) (error "Oh, no!"))
        (backtrack #f))
      """
    When I run `scheme main.scm`
    Then the exit status should not be 0
    And the stdout should contain exactly:
      """
      ABCDEFGHIJKLMNOPQRSTUVWXYZ
      """
    And the stderr should contain "Oh, no!"
