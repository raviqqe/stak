Feature: Inclusion

  Scenario: Include a file
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (include "./foo.scm")
      """
    And a file named "foo.scm" with:
      """scheme
      (write-string "foo")
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "foo"

  Scenario: Include two files
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (include "./foo.scm")
      (include "./bar.scm")
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

  Scenario: Include a file in an included file
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (include "./foo.scm")
      """
    And a file named "foo.scm" with:
      """scheme
      (include "./bar.scm")
      """
    And a file named "bar.scm" with:
      """scheme
      (write-string "bar")
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "bar"
