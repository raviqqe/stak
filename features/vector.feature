Feature: Vector
  Scenario: Convert a vector to a list
    Given a file named "main.scm" with:
    """scheme
    (map write-u8 (vector->list #(65 66 67)))
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "ABC"
