Feature: List
  Scenario: Use literals
    Given a file named "main.scm" with:
    """scheme
    (define x '())
    (define y '(1 2 3))
    (define z '((1) (2 2) (3 3 3)))
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    Then I successfully run `stak main.out`

  Scenario: Create a pair
    Given a file named "main.scm" with:
    """scheme
    (cons 42 '())
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    Then I successfully run `stak main.out`

  Scenario: Create a pair with a non-cons cdr
    Given a file named "main.scm" with:
    """scheme
    (cons 1 2)
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    Then I successfully run `stak main.out`

  Scenario: Get a tag of a pair with a non-cons cdr
    Given a file named "main.scm" with:
    """scheme
    (rib-tag (cons 1 2))
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    Then I successfully run `stak main.out`

  Scenario: Use a map function
    Given a file named "main.scm" with:
    """scheme
    (map write-u8 '(65 66 67))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "ABC"

  Scenario: Use a memq function
    Given a file named "main.scm" with:
    """scheme
    (write-u8 (if (memq 2 '(1 2 3)) 65 66))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Use a memv function
    Given a file named "main.scm" with:
    """scheme
    (write-u8 (if (memv 2 '(1 2 3)) 65 66))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Use an append function
    Given a file named "main.scm" with:
    """scheme
    (map write-u8 (append '(65) '(66)))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "AB"

  Scenario: Use an append function with three lists
    Given a file named "main.scm" with:
    """scheme
    (map write-u8 (append '(65) '(66) '(67)))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "ABC"
