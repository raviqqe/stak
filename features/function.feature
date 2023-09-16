Feature: Function
  Scenario: Call a global function
    Given a file named "main.scm" with:
    """scheme
    (define (f x) (+ x 5))

    (write-u8 (f 60))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Call a local function
    Given a file named "main.scm" with:
    """scheme
    (let ((f (lambda (x) (+ x 5))))
      (write-u8 (f 60)))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Call an immediate function
    Given a file named "main.scm" with:
    """scheme
    (write-u8 (+ 60 ((lambda (x) x) 5)))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Call nested immediate functions
    Given a file named "main.scm" with:
    """scheme
    (define (f x)
      ((lambda () ((lambda () x)))))

    (write-u8 (f 65))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Return a constant
    Given a file named "main.scm" with:
    """scheme
    (define (f) 65)
    (write-u8 (f))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Return the first argument
    Given a file named "main.scm" with:
    """scheme
    (define (f x) x)
    (write-u8 (f 65))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Return the second argument
    Given a file named "main.scm" with:
    """scheme
    (define (f x y) y)
    (write-u8 (f 66 65))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Compute a value with arguments
    Given a file named "main.scm" with:
    """scheme
    (define (f x y) (+ x y))
    (write-u8 (f 60 5))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Update a captured variable in a closure
    Given a file named "main.scm" with:
    """scheme
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
    (define (f . xs) (map write-u8 xs))
    (f 65 66 67)
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "ABC"

  Scenario: Use variadic arguments with a fixed argument
    Given a file named "main.scm" with:
    """scheme
    (define (f x . ys) (map (lambda (z) (write-u8 (+ x z))) ys))
    (f 65 0 1 2)
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "ABC"

  Scenario: Call a fibonacci function
    Given a file named "main.scm" with:
    """scheme
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
