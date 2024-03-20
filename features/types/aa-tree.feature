Feature: AA tree
  @stak
  Scenario: Create an empty tree
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (stak aa-tree))

      (aa-tree-empty <)
      """
    When I successfully run `scheme main.scm`
    Then the exit status should be 0

  @stak
  Scenario: Check if a value is a tree
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (stak aa-tree))

      (write-u8 (if (aa-tree? (aa-tree-empty <)) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  @stak
  Scenario: Find no value
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (stak aa-tree))

      (define tree (aa-tree-empty <))

      (write-u8 (if (aa-tree-find tree 1) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "B"

  @stak
  Scenario: Insert a value into a tree
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (stak aa-tree))

      (define tree (aa-tree-empty <))

      (aa-tree-insert! tree 1)

      (write-u8 (if (= (aa-tree-find tree 1) 1) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  @stak
  Scenario: Insert a value into a left of a tree
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (stak aa-tree))

      (define tree (aa-tree-empty <))

      (aa-tree-insert! tree 2)
      (aa-tree-insert! tree 1)

      (write-u8 (if (= (aa-tree-find tree 1) 1) 65 66))
      (write-u8 (if (= (aa-tree-find tree 2) 2) 65 66))
      (write-u8 (if (= (aa-tree-find tree 3) 3) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "AAB"

  @stak
  Scenario: Insert a value into a right of a tree
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (stak aa-tree))

      (define tree (aa-tree-empty <))

      (aa-tree-insert! tree 1)
      (aa-tree-insert! tree 2)

      (write-u8 (if (= (aa-tree-find tree 1) 1) 65 66))
      (write-u8 (if (= (aa-tree-find tree 2) 2) 65 66))
      (write-u8 (if (= (aa-tree-find tree 3) 3) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "AAB"

  @stak
  Scenario: Insert a value into the same node of a tree
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (stak aa-tree))

      (define tree (aa-tree-empty <))

      (aa-tree-insert! tree 1)
      (aa-tree-insert! tree 1)

      (write-u8 (if (= (aa-tree-find tree 1) 1) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"
