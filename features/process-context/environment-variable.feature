@process-context
Feature: Environment variables
  Scenario: Get an environment variable
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme process-context))

      (write-string (get-environment-variable "FOO"))
      """
    And I set the environment variable "FOO" to "bar"
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "bar"

  Scenario: Get environment variables
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme process-context))

      (for-each
        (lambda (pair)
          (write-string (car pair))
          (write-char #\=)
          (write-string (cdr pair))
          (newline))
        (get-environment-variables))
      """
    And I set the environment variable "FOO" to "bar"
    And I set the environment variable "BAZ" to "qux"
    When I successfully run `scheme main.scm`
    Then the stdout should contain "FOO=bar"
    And the stdout should contain "BAZ=qux"
