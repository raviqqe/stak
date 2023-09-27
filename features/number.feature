Feature: Number
  Scenario: Use literals
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define x 0)
    (define y 1)
    (define z 42)
    (define v -1)
    (define w -42)
    """
    When I successfully run `scheme main.scm`
    Then the exit status should be 0

  Scenario: Use a negative integer
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (write-u8 (+ 66 -1))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Use large (but not big) integers
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (write-u8 (- 1065 1000))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Use integers around the encoding base
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (write-u8 (- 127 60))
    (write-u8 (- 128 60))
    (write-u8 (- 129 60))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "CDE"

  Scenario: Use arithmetic operators
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define (test x y)
      (write-u8 (if (= x y) 65 66)))

    (test (+) 0)
    (test (+ 1) 1)
    (test (+ 1 2) 3)
    (test (- 1) -1)
    (test (- 0 1) -1)
    (test (- 0 1 2) -3)
    (test (*) 1)
    (test (* 2) 2)
    (test (* 2 3) 6)
    (test (/ 6 2) 3)
    (test (/ 6 2 3) 1)
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "AAAAAAAAAAA"

  @stak
  Scenario: Calculate a multiplicative inverse
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (define (test x y)
      (write-u8 (if (= x y) 65 66)))

    (test (/ 2) 0)
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Use comparison operators
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (write-u8 (if (< 0 1) 65 66))
    (write-u8 (if (< 0 1 2) 65 66))
    (write-u8 (if (> 1 0) 65 66))
    (write-u8 (if (<= 0 1) 65 66))
    (write-u8 (if (<= 0 0) 65 66))
    (write-u8 (if (>= 1 0) 65 66))
    (write-u8 (if (>= 0 0) 65 66))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "AAAAAAA"

  @stak @guile
  Scenario: Use comparison operators with an insufficient number of arguments
    Given a file named "main.scm" with:
    """scheme
    (import (scheme base))

    (write-u8 (if (<) 65 66))
    (write-u8 (if (< 0) 65 66))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "AA"
