Feature: Time
  Scenario: Get a current second
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme time))

      (write-u8 (if (> (current-second) 1727447593) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  @chibi @stak
  Scenario: Get a current jiffy
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme time))

      (define seconds (current-second))

      (write-u8 (if (>= (current-jiffy) (* seconds (jiffies-per-second))) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"
