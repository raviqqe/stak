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
  Scenario: Check if a value is an AA tree
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (stak aa-tree))

      (write-u8 (if (aa-tree? (aa-tree-empty <)) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"
