Feature: Parameter
  Scenario: Make a parameter
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (write-u8 ((make-parameter 65)))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Parameterize a function
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define x (make-parameter 65))

    (write-u8 (x))
    (write-u8 (parameterize ((x 66)) (x)))
    (write-u8 (x))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "ABA"

  Scenario: Capture a parameter
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define x (make-parameter 65))
    (define f (lambda () (x)))

    (parameterize ((x 66))
      (write-u8 (f))
      (write-u8 (x)))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "ABA"
