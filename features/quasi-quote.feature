Feature: Quasi-quote
  Scenario: Quote a number
    Given a file named "main.scm" with:
    """scheme
    (write-u8 `65)
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Quote a list
    Given a file named "main.scm" with:
    """scheme
    (map write-u8 `(65 66 67))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "ABC"

  Scenario: Unquote a number
    Given a file named "main.scm" with:
    """scheme
    (define x 65)
    (define y 66)
    (define z 67)

    (map write-u8 `(,x ,y ,z))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "ABC"

  Scenario: Unquote a list
    Given a file named "main.scm" with:
    """scheme
    (define x 65)
    (define y 66)
    (define z '(67))

    (map write-u8 `(,x ,y . ,z))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "ABC"

  Scenario: Unquote and splice a list
    Given a file named "main.scm" with:
    """scheme
    (define x '(65))

    (map write-u8 `(,@x))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Unquote and splice multiple lists
    Given a file named "main.scm" with:
    """scheme
    (define x '(65))
    (define y '(66))

    (map write-u8 `(,@x ,@y))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "AB"
