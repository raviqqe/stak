Feature: letrec-syntax

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
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "B"

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
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Put a sequence in a body of `letrec-syntax`
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))
      
      (letrec-syntax ()
        (write-u8 65)
        (write-u8 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "AB"
