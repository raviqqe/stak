@library
Feature: AA tree
  Scenario: Create an empty tree
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (stak aa-tree))

      (aa-tree-empty <)
      """
    When I run the following script:
      """sh
      scheme -l $STAK_ROOT/aa-tree.scm main.scm
      """
    Then the exit status should be 0

  Scenario: Check if a value is a tree
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (stak aa-tree))

      (write-u8 (if (aa-tree? (aa-tree-empty <)) 65 66))
      """
    When I run the following script:
      """sh
      scheme -l $STAK_ROOT/aa-tree.scm main.scm
      """
    Then the exit status should be 0
    And the stdout should contain exactly "A"

  Scenario: Find no value
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (stak aa-tree))

      (define tree (aa-tree-empty <))

      (write-u8 (if (aa-tree-find tree 1) 65 66))
      """
    When I run the following script:
      """sh
      scheme -l $STAK_ROOT/aa-tree.scm main.scm
      """
    Then the exit status should be 0
    And the stdout should contain exactly "B"

  Scenario: Insert a value into a tree
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (stak aa-tree))

      (define tree (aa-tree-empty <))

      (aa-tree-insert! tree 1)

      (write-u8 (if (= (aa-tree-find tree 1) 1) 65 66))
      """
    When I run the following script:
      """sh
      scheme -l $STAK_ROOT/aa-tree.scm main.scm
      """
    Then the exit status should be 0
    And the stdout should contain exactly "A"

  Scenario: Insert a value into a left of a tree
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (stak aa-tree))

      (define tree (aa-tree-empty <))

      (aa-tree-insert! tree 2)
      (aa-tree-insert! tree 1)

      (for-each
        (lambda (x)
          (write-u8 (if (eq? (aa-tree-find tree x) x) 65 66)))
        '(1 2 3))
      """
    When I run the following script:
      """sh
      scheme -l $STAK_ROOT/aa-tree.scm main.scm
      """
    Then the exit status should be 0
    And the stdout should contain exactly "AAB"

  Scenario: Insert a value into a right of a tree
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (stak aa-tree))

      (define tree (aa-tree-empty <))

      (aa-tree-insert! tree 1)
      (aa-tree-insert! tree 2)

      (for-each
        (lambda (x)
          (write-u8 (if (eq? (aa-tree-find tree x) x) 65 66)))
        '(1 2 3))
      """
    When I run the following script:
      """sh
      scheme -l $STAK_ROOT/aa-tree.scm main.scm
      """
    Then the exit status should be 0
    And the stdout should contain exactly "AAB"

  Scenario: Insert a value into the same node of a tree
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (stak aa-tree))

      (define tree (aa-tree-empty <))

      (aa-tree-insert! tree 1)
      (aa-tree-insert! tree 1)

      (write-u8 (if (= (aa-tree-find tree 1) 1) 65 66))
      """
    When I run the following script:
      """sh
      scheme -l $STAK_ROOT/aa-tree.scm main.scm
      """
    Then the exit status should be 0
    And the stdout should contain exactly "A"

  Scenario Outline: Insert values into a tree
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (stak aa-tree))

      (define tree (aa-tree-empty <))

      (define (check x)
        (write-u8 (if (= (aa-tree-find tree x) x) 65 66)))

      (for-each
        (lambda (x)
          (check x)
          (aa-tree-insert! tree x)
          (check x))
        '(<values>))

      (for-each check '(<values>))
      """
    When I run the following script:
      """sh
      scheme -l $STAK_ROOT/aa-tree.scm main.scm
      """
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
      (import (scheme base) (stak aa-tree))

      (write-u8 (if (equal? (aa-tree->list (list->aa-tree '(<values>) <)) '(<output>)) 65 66))
      """
    When I run the following script:
      """sh
      scheme -l $STAK_ROOT/aa-tree.scm main.scm
      """
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
