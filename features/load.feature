Feature: Load
  Scenario: Load a file
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme eval) (scheme load))

      (load "./foo.scm" (environment '(scheme base)))
      """
    And a file named "foo.scm" with:
      """scheme
      (write-string "foo")
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "foo"

  Scenario: Load two files
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme eval) (scheme load))

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

  Scenario: Load a file in an loaded file
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme eval) (scheme load))

      (load "./foo.scm" (environment '(scheme base) '(scheme eval) '(scheme load)))
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
