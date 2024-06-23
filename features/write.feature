Feature: Write
  Scenario: Write a byte
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 65)
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Write a character
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-char #\A)
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario Outline: Write an escaped character
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme write))

      (write <value>)
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<value>"

    Examples:
      | value     |
      | #\\a      |
      | #\\A      |
      | #\\\\\\\\ |
      | #\\(      |

  @gauche @stak
  Scenario Outline: Write an escaped special character
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme write))

      (write <value>)
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<value>"

    Examples:
      | value        |
      | #\\alarm     |
      | #\\backspace |
      | #\\delete    |
      | #\\escape    |
      | #\\newline   |
      | #\\null      |
      | #\\return    |
      | #\\space     |
      | #\\tab       |

  Scenario: Write a string
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-string "Hello, world!")
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "Hello, world!"

  Scenario Outline: Write a special character in a string
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-string "<value>")
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<value>"

    Examples:
      | value |
      | \\n   |
      | \\t   |
      | \\"   |

  Scenario Outline: Write a boolean
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme write))

      (write <value>)
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<value>"

    Examples:
      | value |
      | #f    |
      | #t    |

  Scenario Outline: Write a list
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme write))

      (write '<value>)
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<value>"

    Examples:
      | value             |
      | ()                |
      | (1)               |
      | (1 2)             |
      | (1 2 3)           |
      | (1 (1 2) (3 4 5)) |
      | (1 . 2)           |
      | (1 2 . 3)         |

  Scenario Outline: Write a number
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme write))

      (write <value>)
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<value>"

    Examples:
      | value |
      | 0     |
      | 1     |
      | 42    |
      | -1    |
      | -42   |

  @stak
  Scenario: Write a procedure
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme write))

      (write (lambda () #f))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "#procedure"

  @stak
  Scenario: Write a record
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme write))

      (define-record-type foo
        (make-foo)
        foo?)

      (write (make-foo))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "#record"

  Scenario Outline: Write a string
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme write))

      (write "<value>")
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "\"<value>\""

    Examples:
      | value         |
      |               |
      | foo           |
      | Hello, world! |
      | \\n           |
      | \\t           |
      | \\r           |
      | \\n\\t\\r     |

  Scenario: Write a symbol
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme write))

      (write 'foo)
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "foo"

  Scenario Outline: Write a vector
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme write))

      (write <value>)
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<value>"

    Examples:
      | value                |
      | #()                  |
      | #(1)                 |
      | #(1 2)               |
      | #(1 2 3)             |
      | #(1 #(1 2) #(3 4 5)) |

  @gauche @guile @stak
  Scenario Outline: Write a bytevector
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme write))

      (write <value>)
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<value>"

    Examples:
      | value      |
      | #u8()      |
      | #u8(1)     |
      | #u8(1 2)   |
      | #u8(1 2 3) |

  Scenario: Write to a port
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme write))

      (write '(42 foo #f) (current-output-port))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "(42 foo #f)"

  @gauche @stak
  Scenario Outline: Write a quote
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme write))

      (write '<value>)
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<value>"

    Examples:
      | value             |
      | '()               |
      | `()               |
      | `(,1)             |
      | (quote)           |
      | (quasiquote)      |
      | (unquote)         |
      | (quote . 42)      |
      | (quasiquote . 42) |
      | (unquote . 42)    |

  @gauche @guile @stak
  Scenario Outline: Write a value in a collection
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme write))

      (write '<value>)
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | value          | output          |
      | (#\\a)         | (#\\a)          |
      | (#\\space)     | (#\\space)      |
      | ("foo")        | (\\"foo\\")     |
      | ((#\\a))       | ((#\\a))        |
      | ((#\\space))   | ((#\\space))    |
      | (("foo"))      | ((\\"foo\\"))   |
      | #(#\\a)        | #(#\\a)         |
      | #(#\\space)    | #(#\\space)     |
      | #("foo")       | #(\\"foo\\")    |
      | #(#(#\\a))     | #(#(#\\a))      |
      | #(#(#\\space)) | #(#(#\\space))  |
      | #(#("foo"))    | #(#(\\"foo\\")) |

  @gauche @guile @stak
  Scenario Outline: Display a value
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme write))

      (display '<value>)
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | value          | output    |
      | #\\a           | a         |
      | "foo"          | foo       |
      | (#\\a)         | (a)       |
      | (#\\space)     | ( )       |
      | ("foo")        | (foo)     |
      | ((#\\a))       | ((a))     |
      | ((#\\space))   | (( ))     |
      | (("foo"))      | ((foo))   |
      | #(#\\a)        | #(a)      |
      | #(#\\space)    | #( )      |
      | #("foo")       | #(foo)    |
      | #(#(#\\a))     | #(#(a))   |
      | #(#(#\\space)) | #(#( ))   |
      | #(#("foo"))    | #(#(foo)) |
