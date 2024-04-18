Feature: Lazy
  Scenario: Delay an expression
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme lazy))

      (delay (write-u8 65))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly ""

  Scenario Outline: Check if a value is a promise
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme lazy))

      (write-u8 (if (promise? <value>) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | value      | output |
      | #f         | B      |
      | (delay #f) | A      |

  Scenario: Force a promise
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme lazy))

      (force (delay (write-u8 65)))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Force and delay an expression
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme lazy))

      (delay-force (write-u8 65))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly ""

  Scenario: Force, delay, and force an expression
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme lazy))

      (write-u8 (force (delay-force (make-promise (+ 60 5)))))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Force and delay expressions in a loop
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme lazy))

      (write-u8
        (force
          (let loop ((i 10000))
            (if (zero? i)
              (make-promise 65)
              (delay-force (loop (- i 1)))))))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Make a promise
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme lazy))

      (write-u8 (force (make-promise 65)))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"
