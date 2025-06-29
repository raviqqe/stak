Feature: Load
  Scenario: Loads a file
    Given a file named "main.scm" with:
      """scheme
      (import (scheme load))

      (load "./foo.scm" (environment '(scheme base)))
      """
    And a file named "foo.scm" with:
      """scheme
      (write-string "foo")
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "foo"

  Scenario: load two files
    Given a file named "main.scm" with:
      """scheme
      (import (scheme load))

      (load "./foo.scm" (environment '(scheme base)))
      (load "./bar.scm" (environment '(scheme base)))
      """
    And a file named "foo.scm" with:
      """scheme
      (write-string "foo")
      """
    And a file named "bar.scm" with:
      """scheme
      (write-string "bar")
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "foobar"

  Scenario: load a file in an loadd file
    Given a file named "main.scm" with:
      """scheme
      (import (scheme load))

      (load "./foo.scm")
      """
    And a file named "foo.scm" with:
      """scheme
      (load "./bar.scm" (environment '(scheme base)))
      """
    And a file named "bar.scm" with:
      """scheme
      (write-string "bar")
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "bar"
