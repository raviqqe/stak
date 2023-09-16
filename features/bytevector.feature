Feature: Bytevector
  Scenario: Write a bytevector
    Given a file named "main.scm" with:
    """scheme
    (write-bytevector #u8(65 66 67))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "ABC"
