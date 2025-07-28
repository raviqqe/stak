Feature: Read

  @chibi @gauche @stak
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
    When I run `stak main.scm` interactively
    And I pipe in the file "input.txt"
    Then the exit status should be 0
    And the stdout should contain exactly "A"

  @chibi @gauche @stak
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
    When I run `stak main.scm` interactively
    And I pipe in the file "input.txt"
    Then the exit status should be 0
    And the stdout should contain exactly "ABC"

  @chibi @gauche @stak
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
    When I run `stak main.scm` interactively
    And I pipe in the file "input.txt"
    Then the exit status should be 0
    And the stdout should contain exactly "A"

  @chibi @gauche @stak
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
    When I run `stak main.scm` interactively
    And I pipe in the file "input.txt"
    Then the exit status should be 0
    And the stdout should contain exactly "AA"

  @chibi @gauche @stak
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
    When I run `stak main.scm` interactively
    And I pipe in the file "input.txt"
    Then the exit status should be 0
    And the stdout should contain exactly "AAB"

  Scenario Outline: Check if a byte is ready or not.
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (u8-ready? (open-input-bytevector #u8(<bytes>))) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "<output>"
    # TODO Add false cases.

    Examples:
      | bytes | output |
      |       | A      |
      | 65    | A      |

  Scenario Outline: Read a character
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-char (read-char))
      """
    And a file named "input.txt" with:
      """text
      <value>
      """
    When I run `stak main.scm` interactively
    And I pipe in the file "input.txt"
    Then the exit status should be 0
    And the stdout should contain exactly "<value>"

    Examples:
      | value |
      | A     |
      | a     |
      | ~     |
      | д     |
      | ∰     |
      | 😄     |

  Scenario Outline: Peek a character
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-char (peek-char))
      """
    And a file named "input.txt" with:
      """text
      <value>
      """
    When I run `stak main.scm` interactively
    And I pipe in the file "input.txt"
    Then the exit status should be 0
    And the stdout should contain exactly "<value>"

    Examples:
      | value |
      | A     |
      | a     |
      | ~     |
      | д     |
      | ∰     |
      | 😄     |

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
    When I run `stak main.scm` interactively
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
    When I run `stak main.scm` interactively
    And I pipe in the file "input.txt"
    Then the exit status should be 0
    And the stdout should contain exactly "AAB"

  Scenario Outline: Check if a character is ready or not.
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (char-ready? (open-input-bytevector #u8(<bytes>))) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "<output>"
    # TODO Add false cases.

    Examples:
      | bytes           | output |
      |                 | A      |
      | 65              | A      |
      | 227 129 130     | A      |
      | 240 159 152 132 | A      |

  Scenario Outline: Read a string
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-string (read-string <count>))
      """
    And a file named "input.txt" with "<value>"
    When I run `stak main.scm` interactively
    And I pipe in the file "input.txt"
    Then the exit status should be 0
    And the stdout should contain exactly "<output>"

    Examples:
      | value    | count | output   |
      | A        | 0     |          |
      | A        | 1     | A        |
      | A        | 2     | A        |
      | ABC      | 2     | AB       |
      | ABC      | 3     | ABC      |
      | ABC      | 4     | ABC      |
      | A😄あ      | 3     | A😄あ      |

  Scenario Outline: Read a byte vector
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (parameterize ((current-input-port (open-input-bytevector #u8(<bytes>))))
        (write-u8 (if (equal? (read-bytevector <count>) #u8(<bytes>)) 65 66)))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | bytes | count |
      |       | 0     |
      | 1     | 1     |
      | 1     | 2     |
      | 1 2   | 2     |
      | 1 2 3 | 3     |
      | 1 2 3 | 4     |

  Scenario Outline: Read a byte vector in place
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define xs (bytevector <original>))

      (read-bytevector! xs (open-input-bytevector #u8(<input>)) <start> <end>)

      (write-u8 (if (equal? xs #u8(<output>)) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | original | input | start | end | output |
      |          |       |       |     |        |

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
    When I run `stak main.scm` interactively
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
    When I run `stak main.scm` interactively
    And I pipe in the file "input.txt"
    Then the exit status should be 0
    And the stdout should contain exactly "A"

  @stak
  Scenario: Read a list without a closing parenthesis
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme read))

      (read)
      """
    And a file named "input.txt" with:
      """text
      (foo
      """
    When I run `stak main.scm` interactively
    And I pipe in the file "input.txt"
    Then the exit status should not be 0
    And the stdout should contain exactly ""
    And the stderr should contain ") expected"
