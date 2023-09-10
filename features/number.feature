Feature: Number
  Scenario: Use literals
    Given a file named "main.scm" with:
    """scheme
    (define x 0)
    (define y 1)
    (define z 42)
    (define v -1)
    (define w -42)
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    Then I successfully run `stak main.out`

  Scenario: Use a negative integer
    Given a file named "main.scm" with:
    """scheme
    (define x -1)
    (write-u8 (+ 66 x))
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"

  Scenario: Use large (but not big) integers
    Given a file named "main.scm" with:
    """scheme
    (write-u8 (- 1065 1000))
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "A"

  Scenario: Use integers around the encoding base
    Given a file named "main.scm" with:
    """scheme
    (write-u8 (- 127 60))
    (write-u8 (- 128 60))
    (write-u8 (- 129 60))
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "CDE"

  Scenario: Use comparison operators
    Given a file named "main.scm" with:
    """scheme
    (write-u8 (if (<) 65 66))
    (write-u8 (if (< 0) 65 66))
    (write-u8 (if (< 0 1) 65 66))
    (write-u8 (if (< 0 1 2) 65 66))
    (write-u8 (if (> 1 0) 65 66))
    (write-u8 (if (<= 0 1) 65 66))
    (write-u8 (if (<= 0 0) 65 66))
    (write-u8 (if (>= 1 0) 65 66))
    (write-u8 (if (>= 0 0) 65 66))
    """
    When I run the following script:
    """sh
    compile.sh main.scm > main.out
    """
    And I successfully run `stak main.out`
    Then the stdout should contain exactly "AAAAAAAAA"
