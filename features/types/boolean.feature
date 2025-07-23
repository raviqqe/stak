Feature: Boolean

  Scenario Outline: Check if a value is a boolean
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))
      
      (write-u8 (if (boolean? <value>) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | value | output |
      | #f    | A      |
      | #t    | A      |
      | '()   | B      |

  Scenario Outline: Use a `not` operator
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))
      
      (write-u8 (if (not <value>) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | value | output |
      | #f    | A      |
      | #t    | B      |

  Scenario Outline: Check equality of boolean values
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))
      
      (write-u8 (if (boolean=? <values>) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | values   | output |
      | #f #f    | A      |
      | #f #t    | B      |
      | #t #f    | B      |
      | #t #t    | A      |
      | #f #f #f | A      |
      | #t #t #t | A      |
      | #f #f #t | B      |
      | #t #t #f | B      |
