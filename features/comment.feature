Feature: Comment

  Scenario: Skip a shebag
    Given a file named "main.scm" with:
      """scheme
      #!/usr/bin/evn stak

      (import (scheme base))

      (write-u8 65)
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"
