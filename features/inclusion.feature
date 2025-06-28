Feature: Inclusion
  Scenario: Import an `eval` library
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (include "./foo.scm")
      """
    And a file named "foo.scm" with:
      """scheme
      (write-string "Hello, World!")
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "Hello, world!"
