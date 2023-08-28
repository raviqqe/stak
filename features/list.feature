Feature: List
  Scenario: Use a map function
    Given a file named "main.scm" with:
    """scheme
    (map write-u8 '(65 66 67))
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "ABC"

  Scenario: Use a memq function
    Given a file named "main.scm" with:
    """scheme
    (write-u8 (if (memq 2 '(1 2 3)) 65 66))
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"

  Scenario: Use a memv function
    Given a file named "main.scm" with:
    """scheme
    (write-u8 (if (memv 2 '(1 2 3)) 65 66))
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"
