Feature: Port
  Scenario Outline: Check if a value is a port
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (port? <expression>) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | expression            |
      | (current-input-port)  |
      | (current-output-port) |

  Scenario: Check if a value is an input port
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (input-port? (current-input-port)) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

  Scenario Outline: Check if a value is an output port
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (write-u8 (if (output-port? <expression>) 65 66))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "A"

    Examples:
      | expression            |
      | (current-output-port) |
      | (current-error-port)  |

  Scenario Outline: Read from a string port
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (call-with-port
        (open-input-string "<string>")
        (lambda (port)
          (parameterize ((current-input-port port))
            (do ((x (read-u8) (read-u8)))
              ((eof-object? x) #f)
              (write-u8 x)))))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "<string>"

    Examples:
      | string |
      | ABC    |
      | あ      |
      | 😄      |

  Scenario Outline: Write to a string port
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (call-with-port
        (open-output-string)
        (lambda (port)
          (parameterize ((current-output-port port))
            (for-each write-u8 '(<bytes>)))
          (let ((xs (get-output-string port)))
            (unless (= (string-length xs) <length>)
              (error "invalid length"))
            (write-string xs))))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | bytes           | output | length |
      | 65 66 67        | ABC    | 3      |
      | 227 129 130     | あ      | 1      |
      | 240 159 152 132 | 😄      | 1      |

  Scenario: Read from a bytevector port
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (call-with-port
        (open-input-bytevector #u8(65 66 67))
        (lambda (port)
          (parameterize ((current-input-port port))
            (do ((x (read-u8) (read-u8)))
              ((eof-object? x) #f)
              (write-u8 x)))))
      """
    When I successfully run `stak main.scm`

  Scenario Outline: Write to a bytevector port
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (call-with-port
        (open-output-bytevector)
        (lambda (port)
          (parameterize ((current-output-port port))
            (for-each write-u8 '(<bytes>)))
          (let ((xs (get-output-bytevector port)))
            (unless (= (bytevector-length xs) <length>)
              (error "invalid length"))
            (write-bytevector xs))))
      """
    When I successfully run `stak main.scm`
    Then the stdout should contain exactly "<output>"

    Examples:
      | bytes    | output | length |
      | 65       | A      | 1      |
      | 65 66    | AB     | 2      |
      | 65 66 67 | ABC    | 3      |
