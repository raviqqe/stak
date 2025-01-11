Feature: String
  Scenario: Write a string
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-string "Hello, world!")
      """
    When I successfully run `stak  main.scm`
    Then the stdout should contain exactly "Hello, world!"

  Scenario: Convert a string to a list
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (for-each write-char (string->list "Hello, world!"))
      """
    When I successfully run `stak  main.scm`
    Then the stdout should contain exactly "Hello, world!"

  Scenario: Convert a list to a string
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-string (list->string (string->list "Hello, world!")))
      """
    When I successfully run `stak  main.scm`
    Then the stdout should contain exactly "Hello, world!"

  Scenario Outline: Append strings
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-string (string-append <values>))
      """
    When I successfully run `stak  main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | values            | output    |
      |                   |           |
      | ""                |           |
      | "foo"             | foo       |
      | "app" "le"        | apple     |
      | "dis" "cov" "ery" | discovery |

  Scenario Outline: Get a character in a string
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-char (string-ref "<string>" <index>))
      """
    When I successfully run `stak  main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | string | index | output |
      | a      | 0     | a      |
      | ab     | 0     | a      |
      | ab     | 1     | b      |
      | abc    | 0     | a      |
      | abc    | 1     | b      |
      | abc    | 2     | c      |

  Scenario Outline: Get a length of a string
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (= (string-length "<value>") <length>) 65 66))
      """
    When I successfully run `stak  main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | value | length |
      |       | 0      |
      | a     | 1      |
      | aa    | 2      |
      | aaa   | 3      |

  Scenario Outline: Get a sub-string
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (equal? (substring "<value>" <start> <end>) "<output>") 65 66))
      """
    When I successfully run `stak  main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | value | start | end | output |
      |       | 0     | 0   |        |
      | a     | 0     | 0   |        |
      | a     | 0     | 1   | a      |
      | ab    | 0     | 0   |        |
      | ab    | 0     | 1   | a      |
      | ab    | 1     | 2   | b      |
      | ab    | 0     | 2   | ab     |
      | abc   | 0     | 0   |        |
      | abc   | 0     | 1   | a      |
      | abc   | 1     | 2   | b      |
      | abc   | 2     | 3   | c      |
      | abc   | 0     | 2   | ab     |
      | abc   | 1     | 3   | bc     |
      | abc   | 0     | 3   | abc    |

  Scenario Outline: Check string equality
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (string=? "<value>" "<value>") 65 66))
      """
    When I successfully run `stak  main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | value |
      |       |
      | a     |
      | ab    |
      | abc   |

  Scenario Outline: Check string inequality
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (string=? "<left>" "<right>") 65 66))
      """
    When I successfully run `stak  main.scm`
    Then the stdout should contain exactly "B"

    Examples:
      | left | right |
      |      | a     |
      | a    |       |
      | a    | b     |
      | a    | ab    |
      | ab   | a     |
      | aa   | ab    |
      | aa   | aaa   |
      | aaa  | aa    |
      | aaa  | aab   |
      | aab  | aaa   |

  Scenario Outline: Check a string order
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (string<? "<left>" "<right>") 65 66))
      """
    When I successfully run `stak  main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | left | right |
      |      | a     |
      | a    | b     |
      | a    | aa    |
      | aa   | ab    |
      | aa   | aaa   |
      | aaa  | aab   |

  Scenario Outline: Check a string order inverse
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (not (string<? "<left>" "<right>")) 65 66))
      """
    When I successfully run `stak  main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | left | right |
      |      |       |
      | a    |       |
      | a    | a     |
      | b    | a     |
      | aa   | a     |
      | aa   | aa    |
      | ab   | aa    |
      | ba   | aa    |
      | ba   | ab    |
