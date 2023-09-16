Feature: String
  Scenario: Write a string
    Given a file named "main.scm" with:
    """scheme
    (write-string "Hello, world!")
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "Hello, world!"
