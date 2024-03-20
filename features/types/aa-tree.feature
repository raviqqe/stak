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
