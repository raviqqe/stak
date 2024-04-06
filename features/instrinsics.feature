Feature: Intrinsics
  @stak
  Scenario: Get a tag of a pair with a non-cons `cdr`
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base))

      (rib-cmr (cons 1 2))
      """
    When I successfully run `scheme main.scm`
    Then the exit status should be 0

  @stak
  Scenario: Preserve a tag when a `cdr` is set
    Given a file named "main.scm" with:
      """scheme
      (import (scheme base) (scheme write))

      (define x (rib 0 #f #f 7))

      (write-u8 (+ 48 (rib-cmr x)))
      (write-u8 (if (rib-cdr x) 65 66))

      (rib-set-cdr! x #t)

      (write-u8 (+ 48 (rib-cmr x)))
      (write-u8 (if (rib-cdr x) 65 66))
      """
    When I successfully run `scheme main.scm`
    Then the stdout should contain exactly "7B7A"
