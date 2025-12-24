Feature: Vector

  Scenario: Make a vector
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (make-vector 42)
      """
    When I successfully run `stak main.scm`
    Then the exit status should be 0

  Scenario: Make a vector with a fill value
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (make-vector 42 #t)
      """
    When I successfully run `stak main.scm`
    Then the exit status should be 0

  Scenario: Convert a vector to a list
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (vector-for-each write-u8 #(65 66 67))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "ABC"

  Scenario Outline: Get a length of a vector
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (= (vector-length <value>) <length>) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | value              | length |
      | #()                | 0      |
      | #(1)               | 1      |
      | #(1 2)             | 2      |
      | #(1 2 3)           | 3      |
      | (make-vector 3)    | 3      |
      | (make-vector 3 #t) | 3      |
      | (vector 1 2 3)     | 3      |

  Scenario Outline: Get an element in a vector
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (vector-ref <vector> <index>))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | vector            | index | output |
      | (vector 65)       | 0     | A      |
      | (vector 65 66)    | 0     | A      |
      | (vector 65 66)    | 1     | B      |
      | (vector 65 66 67) | 0     | A      |
      | (vector 65 66 67) | 1     | B      |
      | (vector 65 66 67) | 2     | C      |

  Scenario Outline: Set an element in a vector
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define xs <vector>)

      (vector-set! xs <index> 88)

      (vector-for-each write-u8 xs)
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | vector            | index | output |
      | (vector 65)       | 0     | X      |
      | (vector 65 66)    | 0     | XB     |
      | (vector 65 66)    | 1     | AX     |
      | (vector 65 66 67) | 0     | XBC    |
      | (vector 65 66 67) | 1     | AXC    |
      | (vector 65 66 67) | 2     | ABX    |

  Scenario Outline: Append vectors
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (vector-for-each write-u8 (vector-append <values>))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | values                     | output |
      | #()                        |        |
      | #() #()                    |        |
      | #(65)                      | A      |
      | #(65) #(66)                | AB     |
      | #(65) #(66) #(67)          | ABC    |
      | #(65) #(66 67) #(68 69 70) | ABCDEF |

  Scenario: Map a function on a vector
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (vector-for-each write-u8 (vector-map (lambda (x) (+ x 65)) #(0 1 2)))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "ABC"

  Scenario Outline: Copy a vector
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (vector-for-each write-u8 (vector-copy <value>))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | value       | output |
      | #()         |        |
      | #(65)       | A      |
      | #(65 66)    | AB     |
      | #(65 66 67) | ABC    |

  Scenario Outline: Copy a vector in place
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define xs (vector <values>))

      (vector-copy! xs <arguments>)

      (for-each
        (lambda (x) (write-u8 (+ x 65)))
        (vector->list xs))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "<output>"

    # spell-checker: disable
    Examples:
      | values    | arguments        | output |
      | 0         | 0 #()            | A      |
      | 0 1 2     | 0 #(3 4 5)       | DEF    |
      | 0 1 2     | 1 #(3 4)         | ADE    |
      | 0 1 2     | 2 #(3)           | ABD    |
      | 0 1 2 3 4 | 1 #(5 6 7)       | AFGHE  |
      | 0 1 2 3   | 1 #(4 5 6 7) 1   | AFGH   |
      | 0 1 2 3   | 1 #(4 5 6 7) 1 3 | AFGD   |

  # spell-checker: enable
  Scenario Outline: Fill a vector
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (define xs (vector <values>))

      (vector-fill! xs <arguments>)

      (for-each
        (lambda (x) (write-u8 (+ x 65)))
        (vector->list xs))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "<output>"

    # spell-checker: disable
    Examples:
      | values  | arguments | output |
      | 0       | 1         | B      |
      | 0 1     | 2 0       | CC     |
      | 0 1     | 2 1       | AC     |
      | 0 1 2   | 3         | DDD    |
      | 0 1 2 3 | 4 1 3     | AEED   |

  @long
  Rule: Large vectors

    Scenario Outline: Reference an element
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base) (scheme write))

        (write (vector-ref (make-vector <length> 42) <index>))
        """
      When I successfully run `stak main.scm`
      Then the stdout should contain exactly "42"

      Examples:
        | length | index |
        | 1      | 0     |
        | 2      | 0     |
        | 2      | 1     |
        | 3      | 0     |
        | 3      | 1     |
        | 3      | 2     |
        | 8      | 0     |
        | 8      | 1     |
        | 8      | 6     |
        | 8      | 7     |
        | 9      | 0     |
        | 9      | 1     |
        | 9      | 7     |
        | 9      | 8     |
        | 64     | 63    |
        | 65     | 64    |
        | 512    | 511   |
        | 513    | 512   |
        | 4096   | 4095  |
        | 4097   | 4096  |

    Scenario Outline: Convert values between a list and a vector
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base) (scheme write) (srfi 1))

        (define xs (iota <count>))

        (write (equal? (vector->list (list->vector xs)) xs))
        """
      When I successfully run `stak main.scm`
      Then the stdout should contain exactly "#t"

      Examples:
        | count |
        | 0     |
        | 1     |
        | 2     |
        | 3     |
        | 4     |
        | 5     |
        | 6     |
        | 7     |
        | 8     |
        | 9     |
        | 16    |
        | 17    |
        | 32    |
        | 33    |
        | 64    |
        | 65    |
        | 128   |
        | 129   |
        | 512   |
        | 513   |
        | 4096  |
        | 4097  |

    @gauche @guile @stak
    Scenario Outline: Use a large vector literal
      Given a file named "main.scm" with:
        """scheme
        (import (scheme base) (scheme write))

        (define xs (include "./vector.scm"))

        (write (vector-ref xs <index>))
        """
      And I run the following script:
        """sh
        {
          echo '#('

          for index in $(seq <length>); do
            echo $index
            echo ' '
          done

          echo ')'
        } >> vector.scm
        """
      When I successfully run `stak main.scm`
      Then the stdout should contain exactly "<output>"

      Examples:
        | length | index | output |
        | 1      | 0     | 1      |
        | 2      | 0     | 1      |
        | 2      | 1     | 2      |
        | 512    | 0     | 1      |
        | 512    | 1     | 2      |
        | 512    | 510   | 511    |
        | 512    | 511   | 512    |
        | 4096   | 0     | 1      |
        | 4096   | 1     | 2      |
        | 4096   | 4094  | 4095   |
        | 4096   | 4095  | 4096   |
