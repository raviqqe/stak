Feature: Continuation
  Scenario: Call a continuation
    Given a file named "main.scm" with:
    """scheme
    (write-u8 (call/cc (lambda (k) (k 65))))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Call a continuation with a global variable
    Given a file named "main.scm" with:
    """scheme
    (define x 5)

    (write-u8 (+ 60 (call/cc (lambda (k) (k x)))))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Call a continuation with a local variable
    Given a file named "main.scm" with:
    """scheme
    (define (f x) (call/cc (lambda (k) (k x))))

    (write-u8 (+ 60 (f 5)))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Return a value from a receiver
    Given a file named "main.scm" with:
    """scheme
    (write-u8 (call/cc (lambda (k) 65)))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "A"

  Scenario: Modify environment
    Given a file named "main.scm" with:
    """scheme
    (define backtrack #f)

    (let ((i 65))
      (call/cc
        (lambda (target)
          (set! backtrack target)
          #f))
      (write-u8 i)
      (newline)
      (set! i (+ i 1))
      (unless (< i 91) (error "!"))
      (backtrack #f))
    """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly:
    """
    A
    B
    C
    D
    E
    F
    G
    H
    I
    J
    K
    L
    M
    N
    O
    P
    Q
    R
    S
    T
    U
    V
    W
    X
    Y
    Z
    !
    """
