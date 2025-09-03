Feature: Comment

  Scenario: Skip a line comment
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      ; foo

      (write-u8 65)
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Skip a block comment
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      #|
      foo
      |#

      (write-u8 65)
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Skip a shebang
    Given a file named "main.scm" with:
      """scheme
      #!/usr/bin/env stak

      (import (scheme base))

      (write-u8 65)
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"
