@stak
Feature: Radix vector

  Scenario Outline: Make a vector
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme write) (stak radix-vector))

      (write (radix-vector->list (make-radix-vector <arguments> <fill>)))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | length | fill | output |
      | 0      |      | ()     |
      | 1      |      | (#f)   |
