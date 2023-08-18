Feature: String
  Scenario: Write a string
    Given a file named "main.scm" with:
    """scheme
    (write-string "Hello, world!")
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "Hello, world!"
