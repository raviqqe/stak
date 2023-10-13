Feature: Function
  Scenario: Call a global function
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define (f x) (+ x 5))

    (write-u8 (f 60))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Call a local function
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (let ((f (lambda (x) (+ x 5))))
      (write-u8 (f 60)))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Call an immediate function
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (write-u8 (+ 60 ((lambda (x) x) 5)))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Call nested immediate functions
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define (f x)
      ((lambda () ((lambda () x)))))

    (write-u8 (f 65))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Return a constant
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define (f) 65)
    (write-u8 (f))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Return the first argument
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define (f x) x)
    (write-u8 (f 65))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Return the second argument
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define (f x y) y)
    (write-u8 (f 66 65))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Compute a value with arguments
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define (f x y) (+ x y))
    (write-u8 (f 60 5))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Update a captured variable in a closure
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define (f x) (lambda () (set! x (+ x 1)) x))
    (define g (f 64))

    (write-u8 (g))
    (write-u8 (g))
    (write-u8 (g))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "ABC"

  Scenario: Use variadic arguments
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define (f . xs) (map write-u8 xs))
    (f 65 66 67)
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "ABC"

  Scenario: Use variadic arguments with a fixed argument
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define (f x . ys) (map (lambda (z) (write-u8 (+ x z))) ys))
    (f 65 0 1 2)
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "ABC"

  Scenario: Call a fibonacci function
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define (fibonacci x)
      (if (< x 2)
        x
        (+
          (fibonacci (- x 1))
          (fibonacci (- x 2)))))

    (write-u8 (+ 33 (fibonacci 10)))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "X"

  Scenario Outline: Call an apply function
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (write-u8 (+ 48 (apply + '(<values>))))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | values | output |
      |        | 0      |
      | 1      | 1      |
      | 1 2    | 3      |
      | 1 2 3  | 6      |

  Scenario Outline: Call an apply function with a correct argument order
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (map write-u8 (apply append '(<values>)))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | values                  | output |
      |                         |        |
      | ()                      |        |
      | () ()                   |        |
      | (65)                    | A      |
      | (65) (66)               | AB     |
      | (65) (66) (67)          | ABC    |
      | (65 66) (67 68)         | ABCD   |
      | (65 66) (67 68) (69 70) | ABCDEF |

  Scenario: Call an apply function with a fixed number of arguments
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define (f x y)
      (+ x y))

    (write-u8 (apply f '(60 5)))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Call an apply function twice with the same list
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define (f x y)
      (+ x y))

    (define xs '(60 5))

    (write-u8 (apply f xs))
    (write-u8 (apply f xs))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "AA"

  Scenario: Call immediate functions capturing a local variable
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define (foo x y z)
      (x)
      (y)
      (z))

    (let ((f write-u8))
      (foo
        (lambda () (f 65))
        (lambda () (f 66))
        (lambda () (f 67))))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "ABC"

  Scenario: Call a function with too few arguments
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define (f x) (+ x 5))

    (write-u8 (f))
    """
    When I run `scheme main.scm`
    Then the exit status should not be 0

  Scenario: Call a function with too many arguments
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define (f x) (+ x 5))

    (write-u8 (f))
    """
    When I run `scheme main.scm`
    Then the exit status should not be 0
