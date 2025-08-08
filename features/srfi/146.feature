@stak
Feature: SRFI 146

  Scenario: Create an empty tree
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (srfi 146))

      (mapping-empty <)
      """
    When I successfully run `stak main.scm`
    Then the exit status should be 0

  Scenario: Check if a value is a tree
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (srfi 146))

      (write-u8 (if (mapping? (mapping-empty <)) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the exit status should be 0
    And the stdout should contain exactly "A"

  Scenario: Find no value
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (srfi 146))

      (define tree (mapping-empty <))

      (write-u8 (if (mapping-find tree 1) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the exit status should be 0
    And the stdout should contain exactly "B"

  Scenario: Insert a value into a tree
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (srfi 146))

      (define tree (mapping-empty <))

      (mapping-insert! tree 1)

      (write-u8 (if (= (mapping-find tree 1) 1) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the exit status should be 0
    And the stdout should contain exactly "A"

  Scenario: Insert a value into a left of a tree
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (srfi 146))

      (define tree (mapping-empty <))

      (mapping-insert! tree 2)
      (mapping-insert! tree 1)

      (for-each
        (lambda (x)
          (write-u8 (if (eq? (mapping-find tree x) x) 65 66)))
        '(1 2 3))
      """
    When I successfully run `stak main.scm`
    Then the exit status should be 0
    And the stdout should contain exactly "AAB"

  Scenario: Insert a value into a right of a tree
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (srfi 146))

      (define tree (mapping-empty <))

      (mapping-insert! tree 1)
      (mapping-insert! tree 2)

      (for-each
        (lambda (x)
          (write-u8 (if (eq? (mapping-find tree x) x) 65 66)))
        '(1 2 3))
      """
    When I successfully run `stak main.scm`
    Then the exit status should be 0
    And the stdout should contain exactly "AAB"

  Scenario: Insert a value into the same node of a tree
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (srfi 146))

      (define tree (mapping-empty <))

      (mapping-insert! tree 1)
      (mapping-insert! tree 1)

      (write-u8 (if (eq? (mapping-find tree 1) 1) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the exit status should be 0
    And the stdout should contain exactly "A"

  Scenario Outline: Insert values into a tree
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (srfi 146))

      (define tree (mapping-empty <))

      (define (check x)
        (write-u8 (if (eq? (mapping-find tree x) x) 65 66)))

      (for-each
        (lambda (x)
          (check x)
          (mapping-insert! tree x)
          (check x))
        '(<values>))

      (for-each check '(<values>))
      """
    When I successfully run `stak main.scm`
    Then the exit status should be 0
    And the stdout should contain exactly "<output>"

    # spell-checker: disable
    Examples:
      | values  | output       |
      | 1 2 3   | BABABAAAA    |
      | 1 3 2   | BABABAAAA    |
      | 2 1 3   | BABABAAAA    |
      | 2 3 1   | BABABAAAA    |
      | 3 1 2   | BABABAAAA    |
      | 3 2 1   | BABABAAAA    |
      | 1 2 3 4 | BABABABAAAAA |
      | 1 2 4 3 | BABABABAAAAA |
      | 1 3 2 4 | BABABABAAAAA |
      | 1 3 4 2 | BABABABAAAAA |
      | 1 4 2 3 | BABABABAAAAA |
      | 1 4 3 2 | BABABABAAAAA |
      | 2 1 3 4 | BABABABAAAAA |
      | 2 1 4 3 | BABABABAAAAA |
      | 2 3 1 4 | BABABABAAAAA |
      | 2 3 4 1 | BABABABAAAAA |
      | 2 4 1 3 | BABABABAAAAA |
      | 2 4 3 1 | BABABABAAAAA |

  Scenario Outline: Convert a list into a tree
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (srfi 146))

      (write-u8 (if (equal? (mapping->list (list->mapping '(<values>) <)) '(<output>)) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the exit status should be 0
    And the stdout should contain exactly "A"

    # spell-checker: disable
    Examples:
      | values  | output  |
      |         |         |
      | 1       | 1       |
      | 1 2     | 1 2     |
      | 2 1     | 1 2     |
      | 1 2 3   | 1 2 3   |
      | 1 3 2   | 1 2 3   |
      | 2 1 3   | 1 2 3   |
      | 2 3 1   | 1 2 3   |
      | 3 1 2   | 1 2 3   |
      | 3 2 1   | 1 2 3   |
      | 1 2 3 4 | 1 2 3 4 |
      | 1 2 4 3 | 1 2 3 4 |
      | 1 3 2 4 | 1 2 3 4 |
      | 1 3 4 2 | 1 2 3 4 |
      | 1 4 2 3 | 1 2 3 4 |
      | 1 4 3 2 | 1 2 3 4 |
      | 2 1 3 4 | 1 2 3 4 |
      | 2 1 4 3 | 1 2 3 4 |
      | 2 3 1 4 | 1 2 3 4 |
      | 2 3 4 1 | 1 2 3 4 |
      | 2 4 1 3 | 1 2 3 4 |
      | 2 4 3 1 | 1 2 3 4 |
