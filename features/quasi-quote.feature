Feature: Quasi-quote
  Scenario: Quote a number
    Given a file named "main.scm" with:
    """scheme
    (write-u8 `65)
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"

  Scenario: Quote a list
    Given a file named "main.scm" with:
    """scheme
    (map write-u8 `(65 66 67))
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "ABC"

  Scenario: Unquote a number
    Given a file named "main.scm" with:
    """scheme
    (define x 65)
    (define y 66)
    (define z 67)

    (map write-u8 `(,x ,y ,z))
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "ABC"

  Scenario: Unquote a list
    Given a file named "main.scm" with:
    """scheme
    (define x 65)
    (define y 66)
    (define z '(67))

    (map write-u8 `(,x ,y . ,z))
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "ABC"

  Scenario: Unquote and unsplice a list in a car
    Given a file named "main.scm" with:
    """scheme
    (define x '(65))

    (map write-u8 `(,@x))
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"

  Scenario: Unquote and unsplice a list in a cdr
    Given a file named "main.scm" with:
    """scheme
    (define x '(65))

    (map write-u8 `(65 . ,@x))
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "AA"
