Feature: String
  Scenario: Write a string
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-string "Hello, world!")
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "Hello, world!"

  Scenario: Create a string
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-string (string #\A #\B #\C))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "ABC"

  Scenario: Convert a string to a list
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (for-each write-char (string->list "Hello, world!"))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "Hello, world!"

  Scenario: Convert a list to a string
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-string (list->string (string->list "Hello, world!")))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "Hello, world!"

  Scenario Outline: Append strings
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-string (string-append <values>))
      """
    When I successfully run `stak main.scm`
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
    When I successfully run `stak main.scm`
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
    When I successfully run `stak main.scm`
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
    When I successfully run `stak main.scm`
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

  Scenario Outline: Get a length of a string
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (= (string-length "<value>") <length>) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | value | length |
      |       | 0      |
      | a     | 1      |
      | aa    | 2      |
      | aaa   | 3      |

  Scenario Outline: Copy a string in place
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define xs (string-copy "<to>"))

      (string-copy! xs <at> "<from>" <start> <end>)

      (write-u8 (if (equal? xs "<output>") 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

    # spell-checker: disable
    Examples:
      | to    | at | from | start | end | output |
      | A     | 0  |      |       |     | A      |
      | ABC   | 0  | DEF  |       |     | DEF    |
      | ABC   | 1  | DE   |       |     | ADE    |
      | ABC   | 2  | D    |       |     | ABD    |
      | ABCDE | 1  | FGH  |       |     | AFGHE  |
      | ABCD  | 1  | EFGH | 1     |     | AFGH   |
      | ABCD  | 1  | EFGH | 1     | 3   | AFGD   |

  # spell-checker: enable
  Scenario Outline: Make a string
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (= (string-length (make-string <length>)) <length>) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | length |
      | 0      |
      | 1      |
      | 2      |
      | 3      |

  Scenario Outline: Make a string with a character
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (equal? (make-string <length> <character>) "<output>") 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | length | character | output |
      | 0      | #\\A      |        |
      | 1      | #\\A      | A      |
      | 2      | #\\A      | AA     |
      | 3      | #\\A      | AAA    |
      | 0      | #\\B      |        |
      | 1      | #\\B      | B      |
      | 2      | #\\B      | BB     |
      | 3      | #\\B      | BBB    |

  Scenario: Iterate over a string
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (string-for-each write-char "ABC")
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "ABC"

  Scenario: Map a function to a string
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8
        (if (equal? (string-map (lambda (x) (integer->char (+ 1 (char->integer x)))) "ABC") "BCD")
          65
          66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

  Scenario Outline: Check string equality
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (string=? "<value>" "<value>") 65 66))
      """
    When I successfully run `stak main.scm`
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
    When I successfully run `stak main.scm`
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
    When I successfully run `stak main.scm`
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
    When I successfully run `stak main.scm`
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

  Scenario Outline: Convert a vector to a string
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-string (vector->string #(<characters>) <range>))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | characters     | range | output |
      |                |       |        |
      | #\\A           |       | A      |
      | #\\A #\\B #\\C |       | ABC    |
      | #\\A #\\B #\\C | 0     | ABC    |
      | #\\A #\\B #\\C | 0 2   | AB     |
      | #\\A #\\B #\\C | 1 3   | BC     |
      | #\\A #\\B #\\C | 1 2   | B      |

  Scenario Outline: Convert a vector to a string
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (equal? (string->vector "<string>" <range>) #(<characters>)) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | string | range | characters     |
      |        |       |                |
      | A      |       | #\\A           |
      | ABC    |       | #\\A #\\B #\\C |
      | ABC    | 0     | #\\A #\\B #\\C |
      | ABC    | 0 2   | #\\A #\\B      |
      | ABC    | 1 3   | #\\B #\\C      |
      | ABC    | 1 2   | #\\B           |
