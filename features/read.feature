Feature: Read
  @stak @chibi @gauche
  Scenario: Read a byte
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (read-u8))
      """
    And a file named "input.txt" with:
      """text
      A
      """
    When I run `scheme main.scm` interactively
    And I pipe in the file "input.txt"
    Then the exit status should be 0
    And the stdout should contain exactly "A"

  @stak @chibi @gauche
  Scenario: Read bytes
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (read-u8))
      (write-u8 (read-u8))
      (write-u8 (read-u8))
      """
    And a file named "input.txt" with:
      """text
      ABC
      """
    When I run `scheme main.scm` interactively
    And I pipe in the file "input.txt"
    Then the exit status should be 0
    And the stdout should contain exactly "ABC"

  @stak @chibi @gauche
  Scenario: Peek a byte
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (peek-u8))
      """
    And a file named "input.txt" with:
      """text
      A
      """
    When I run `scheme main.scm` interactively
    And I pipe in the file "input.txt"
    Then the exit status should be 0
    And the stdout should contain exactly "A"

  @stak @chibi @gauche
  Scenario: Peek a byte multiple times
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (peek-u8))
      (write-u8 (peek-u8))
      """
    And a file named "input.txt" with:
      """text
      A
      """
    When I run `scheme main.scm` interactively
    And I pipe in the file "input.txt"
    Then the exit status should be 0
    And the stdout should contain exactly "AA"

  @stak @chibi @gauche
  Scenario: Peek and read bytes
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (peek-u8))
      (write-u8 (read-u8))
      (write-u8 (read-u8))
      """
    And a file named "input.txt" with:
      """text
      AB
      """
    When I run `scheme main.scm` interactively
    And I pipe in the file "input.txt"
    Then the exit status should be 0
    And the stdout should contain exactly "AAB"

  Scenario: Read a character
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-char (read-char))
      """
    And a file named "input.txt" with:
      """text
      A
      """
    When I run `scheme main.scm` interactively
    And I pipe in the file "input.txt"
    Then the exit status should be 0
    And the stdout should contain exactly "A"

  Scenario: Peek a character
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-char (peek-char))
      """
    And a file named "input.txt" with:
      """text
      A
      """
    When I run `scheme main.scm` interactively
    And I pipe in the file "input.txt"
    Then the exit status should be 0
    And the stdout should contain exactly "A"

  Scenario: Peek a character multiple times
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-char (peek-char))
      (write-char (peek-char))
      """
    And a file named "input.txt" with:
      """text
      A
      """
    When I run `scheme main.scm` interactively
    And I pipe in the file "input.txt"
    Then the exit status should be 0
    And the stdout should contain exactly "AA"

  Scenario: Peek and read characters
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-char (peek-char))
      (write-char (read-char))
      (write-char (read-char))
      """
    And a file named "input.txt" with:
      """text
      AB
      """
    When I run `scheme main.scm` interactively
    And I pipe in the file "input.txt"
    Then the exit status should be 0
    And the stdout should contain exactly "AAB"

  @long
  Scenario Outline: Read a value
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme read))

      (write-u8 (if (equal? (read) '<value>) 65 66))
      """
    And a file named "input.txt" with:
      """text
      <value>
      """
    When I run `scheme main.scm` interactively
    And I pipe in the file "input.txt"
    Then the exit status should be 0
    And the stdout should contain exactly "A"

    Examples:
      | value           |
      | #f              |
      | #t              |
      | 0               |
      | 1               |
      | 2               |
      | 42              |
      | -1              |
      | -2              |
      | -42             |
      | a               |
      | x               |
      | foo             |
      | #\\A            |
      | #\\newline      |
      | ""              |
      | "foo"           |
      | "Hello, world!" |
      | "\\n\\r\\t"     |
      | ()              |
      | (1)             |
      | (1 2)           |
      | (1 2 3)         |
      | (1 . 2)         |
      | (1 2 . 3)       |
      | (foo)           |
      | (foo bar)       |
      | (foo bar baz)   |
      | (foo . bar)     |
      | (foo bar . baz) |
      | #(foo)          |
      | #(foo bar)      |
      | #(foo bar baz)  |
      | #u8()           |
      | #u8(1)          |
      | #u8(1 2)        |
      | #u8(1 2 3)      |

  Scenario: Read from a port
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme read))

      (write-u8 (if (equal? (read (current-input-port)) 'foo) 65 66))
      """
    And a file named "input.txt" with:
      """text
      foo
      """
    When I run `scheme main.scm` interactively
    And I pipe in the file "input.txt"
    Then the exit status should be 0
    And the stdout should contain exactly "A"
