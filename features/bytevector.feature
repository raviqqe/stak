Feature: Bytevector
  Scenario: Write a bytevector
    Given a file named "main.scm" with:
    """scheme
    (write-bytevector #u8(65 66 67))
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "ABC"
