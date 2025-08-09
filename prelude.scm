; Libraries

(define-library (stak base)
  (export
    syntax-rules
    define-syntax
    syntax-error
    _
    ...
    define
    lambda
    let-syntax
    letrec-syntax
    begin
    quasiquote
    unquote
    unquote-splicing
    quote
    set!
    let
    let*
    letrec
    letrec*
    define-values
    let-values
    let*-values
    if
    cond
    case
    else
    =>
    and
    or
    boolean-or
    when
    unless
    do

    cond-expand
    features

    base
    library
    r7rs
    scheme
    stak

    pair-type
    null-type
    boolean-type
    procedure-type
    symbol-type
    string-type
    char-type
    vector-type
    bytevector-type
    record-type

    primitive
    rib
    cons
    close
    rib?
    car
    cdr
    rib-tag
    set-car!
    set-cdr!
    eq?

    apply
    data-rib

    eqv?
    equal?

    procedure?

    boolean?
    not
    boolean=?

    integer?
    rational?
    real?
    complex?
    number?
    exact?
    inexact?
    exact-integer?
    zero?
    positive?
    negative?
    even?
    odd?
    +
    -
    *
    /
    remainder
    quotient
    modulo
    truncate
    truncate-quotient
    truncate-remainder
    truncate/
    floor
    floor-quotient
    floor-remainder
    floor/
    ceiling
    round
    exact
    inexact
    abs
    exp
    expt
    log
    square
    exact-integer-sqrt
    gcd
    lcm
    =
    <
    >
    <=
    >=
    min
    max

    char?
    integer->char
    char->integer
    char=?
    char<?
    char<=?
    char>?
    char>=?

    null?
    pair?
    list?
    caar
    cadr
    cdar
    cddr
    list
    make-list
    length
    map
    for-each
    list-ref
    list-set!
    list-head
    list-tail
    member
    memq
    memv
    assoc
    assq
    assv
    append
    reverse
    fold
    reduce-right
    list-copy

    vector?
    vector
    make-vector
    vector-append
    vector-copy
    vector-copy!
    vector-fill!
    vector-for-each
    vector-length
    vector-map
    vector-ref
    vector-set!
    list->vector
    vector->list
    string->vector
    vector->string

    bytevector?
    bytevector
    make-bytevector
    bytevector-append
    bytevector-copy
    bytevector-copy!
    bytevector-length
    bytevector-u8-ref
    bytevector-u8-set!
    list->bytevector
    bytevector->list

    string?
    string
    list->string
    string->code-points
    code-points->string
    string->list
    string-append
    string-length
    string-ref
    number->string
    string->number
    string-copy
    string-copy!
    substring
    make-string
    string-for-each
    string-map
    string=?
    string<?
    string>?

    symbol?
    symbol=?
    symbol->string
    string->uninterned-symbol

    define-record-type
    record?

    values
    call-with-values

    error
    write-message)

  (begin
    ; Syntax
    ;
    ; Those syntax definitions are mostly ported from https://small.r7rs.org/attachment/r7rs.pdf.

    ;; Base

    ($$define-syntax syntax-rules
      ($$syntax-rules $$... ()
        ((_ (literal $$...) (pattern body) $$...)
          ($$syntax-rules ... (literal $$...) (pattern body) $$...))

        ((_ ellipsis (literal $$...) (pattern body) $$...)
          ($$syntax-rules ellipsis (literal $$...) (pattern body) $$...))))

    ($$define-syntax define-syntax
      (syntax-rules ()
        ((_ name value)
          ($$define-syntax name value))))

    (define-syntax syntax-error
      (syntax-rules ()
        ((_ message value ...)
          ($$syntax-error message value ...))))

    (define-syntax define-optimizer
      (syntax-rules ()
        ((_ name value)
          ($$define-optimizer name value))))

    (define-syntax define
      (syntax-rules ()
        ((_ (name argument ... . rest) body1 body2 ...)
          (define name (lambda (argument ... . rest) body1 body2 ...)))

        ((_ name value)
          ($$define name value))))

    (define-syntax lambda
      (syntax-rules (define define-syntax define-record-type define-values)
        ; Optimize a case where there is only a body of a expression.
        ((_ arguments body)
          ($$lambda arguments body))

        ((_ arguments (define content ...) body1 body2 ...)
          (lambda "value" arguments () (define content ...) body1 body2 ...))

        ((_ "value" arguments ((name value) ...)
            (define (new-name argument ... . rest) body1 body2 ...)
            body3
            body4
            ...)
          (lambda "value" arguments ((name value) ... (new-name (lambda (argument ... . rest) body1 body2 ...)))
            body3
            body4
            ...))

        ((_ "value" arguments ((name value) ...) (define new-name new-value) body1 body2 ...)
          (lambda "value" arguments ((name value) ... (new-name new-value)) body1 body2 ...))

        ((_ "value" arguments ((name value) ...) body1 body2 ...)
          (lambda arguments (letrec* ((name value) ...) body1 body2 ...)))

        ((_ arguments (define-syntax name value) body1 body2 ...)
          (lambda "syntax" arguments ((name value)) body1 body2 ...))

        ((_ "syntax" arguments ((name value) ...) (define-syntax new-name new-value) body1 body2 ...)
          (lambda "syntax" arguments ((name value) ... (new-name new-value)) body1 body2 ...))

        ((_ "syntax" arguments ((name value) ...) body1 body2 ...)
          (lambda arguments (letrec-syntax ((name value) ...) body1 body2 ...)))

        ((_ arguments (define-record-type item ...) body1 body2 ...)
          (lambda arguments (define _ (begin (define-record-type item ...))) body1 body2 ...))

        ((_ arguments (define-values names value) body1 body2 ...)
          (lambda arguments (let-values ((names value)) body1 body2 ...)))

        ((_ arguments body1 body2 ...)
          ($$lambda arguments (begin body1 body2 ...)))))

    (define-syntax let-syntax
      (syntax-rules ()
        ((_ ((name value) ...) body1 body2 ...)
          ($$let-syntax ((name value) ...) (let () body1 body2 ...)))))

    (define-syntax letrec-syntax
      (syntax-rules ()
        ((_ ((name value) ...) body1 body2 ...)
          ($$letrec-syntax ((name value) ...) (let () body1 body2 ...)))))

    (define-syntax begin
      (syntax-rules ()
        ((_ value)
          value)

        ((_ value1 value2 ...)
          ($$begin value1 value2 ...))))

    (define-syntax relaxed-begin
      (syntax-rules ()
        ((_)
          #f)

        ((_ body ...)
          (begin body ...))))

    (define-syntax quasiquote
      (syntax-rules (unquote unquote-splicing)
        ((_ (unquote value))
          value)

        ((_ ((unquote-splicing value1) value2 ...))
          (append value1 (quasiquote (value2 ...))))

        ((_ (value1 value2 ...))
          (cons
            (quasiquote value1)
            (quasiquote (value2 ...))))

        ((_ value)
          (quote value))))

    (define-syntax quote
      (syntax-rules ()
        ((_ value)
          ($$quote value))))

    (define-syntax set!
      (syntax-rules ()
        ((_ name value)
          ($$set! name value))))

    (define-syntax define-features
      (syntax-rules ::: ()
        ((_ cond-expand literals feature-values)
          (begin
            (define (features) 'feature-values)
            (define-features "cond" cond-expand literals feature-values)))

        ((_ "cond" cond-expand literals (feature1 feature2 :::) outer-clause :::)
          (define-features
            "cond"
            cond-expand
            literals
            (feature2 :::)
            ((cond-expand (feature1 body ...) clause ...)
              (relaxed-begin body ...))
            outer-clause
            :::))

        ((_ "cond" cond-expand (and else not or literal :::) () outer-clause :::)
          (define-syntax cond-expand
            (syntax-rules (and else not or literal :::)
              ((cond-expand)
                (syntax-error "unfulfilled cond-expand"))

              ((cond-expand (else body ...))
                (relaxed-begin body ...))

              ((cond-expand ((and) body ...) clause ...)
                (relaxed-begin body ...))

              ((cond-expand ((and requirement1 requirement2 ...) body ...) clause ...)
                (cond-expand
                  (requirement1
                    (cond-expand
                      ((and requirement2 ...) body ...)
                      clause
                      ...))
                  clause
                  ...))

              ((cond-expand ((or) body ...) clause ...)
                (cond-expand clause ...))

              ((cond-expand ((or requirement1 requirement2 ...) body ...) clause ...)
                (cond-expand
                  (requirement1 body ...)
                  ((or requirement2 ...) body ...)
                  clause
                  ...))

              ((cond-expand ((not requirement) body ...) clause ...)
                (cond-expand
                  (requirement
                    (cond-expand
                      clause
                      ...))
                  (else body ...)))

              outer-clause
              :::

              ((cond-expand (feature body ...) clause ...)
                (cond-expand clause ...)))))))

    (define-features
      cond-expand
      (and
        else
        not
        or

        base
        continue
        exception
        library
        r7rs
        read
        scheme
        stak
        write)
      (r7rs
        scheme
        stak
        (library (scheme base))
        (library (scheme read))
        (library (scheme write))
        (library (stak base))
        (library (stak continue))
        (library (stak exception))))

    ;; Binding

    (define-syntax let
      (syntax-rules (define define-record-type define-syntax define-values)
        ((_ () (define content ...) body1 body2 ...)
          ((lambda () (define content ...) body1 body2 ...)))

        ((_ () (define-record-type content ...) body1 body2 ...)
          ((lambda () (define-record-type content ...) body1 body2 ...)))

        ((_ () (define-syntax content ...) body1 body2 ...)
          ((lambda () (define-syntax content ...) body1 body2 ...)))

        ((_ () (define-values content ...) body1 body2 ...)
          ((lambda () (define-values content ...) body1 body2 ...)))

        ; Optimize a case where no definition is in a body.
        ((_ () body1 body2 ...)
          (begin body1 body2 ...))

        ((_ ((name value) ...) body1 body2 ...)
          ((lambda (name ...) body1 body2 ...) value ...))

        ((_ tag ((name value) ...) body1 body2 ...)
          (letrec ((tag (lambda (name ...) body1 body2 ...)))
            (tag value ...)))))

    (define-syntax let*
      (syntax-rules ()
        ((_ () body1 body2 ...)
          (let () body1 body2 ...))

        ((_ ((name1 value1) (name2 value2) ...)
            body1
            body2
            ...)
          (let ((name1 value1))
            (let* ((name2 value2) ...)
              body1
              body2
              ...)))))

    (define-syntax letrec
      (syntax-rules ()
        ((_ ((name value) ...) body1 body2 ...)
          (letrec* ((name value) ...) body1 body2 ...))))

    (define-syntax letrec*
      (syntax-rules ()
        ((_ ((name value) ...) body1 body2 ...)
          (let ((name #f) ...)
            (set! name value)
            ...
            (let ()
              body1
              body2
              ...)))))

    ;; Conditional

    (define-syntax if
      (syntax-rules ()
        ((_ test clause1 clause2)
          ($$if test clause1 clause2))

        ((_ test clause)
          (if test clause #f))))

    (define-syntax cond
      (syntax-rules (else =>)
        ((_ (else result1 result2 ...))
          (begin result1 result2 ...))

        ((_ (test => result) clause ...)
          (let ((temp test))
            (if temp
              (result temp)
              (cond clause ...))))

        ((_ (test) clause ...)
          (or
            test
            (cond clause ...)))

        ((_ (test result1 result2 ...) clause ...)
          (if test
            (begin result1 result2 ...)
            (cond clause ...)))

        ((_)
          #f)))

    (define-syntax case
      (syntax-rules (else =>)
        ((_ (key ...) clause ...)
          (let ((value (key ...)))
            (case value clause ...)))

        ((_ key (else => result))
          (result key))

        ((_ key (else result1 result2 ...))
          (begin result1 result2 ...))

        ((_ key ((atom ...) => result) clause ...)
          (if (case-match key (atom ...))
            (result key)
            (case key clause ...)))

        ((_ key ((atom ...) result1 result2 ...) clause ...)
          (if (case-match key (atom ...))
            (begin result1 result2 ...)
            (case key clause ...)))

        ((_ key)
          #f)))

    (define-syntax case-match
      (syntax-rules ()
        ((_ key (atom))
          (eqv? key 'atom))

        ((_ key (atom ...))
          (memv key '(atom ...)))))

    (define-syntax and
      (syntax-rules ()
        ((_)
          #t)

        ((_ test)
          test)

        ((_ test1 test2 ...)
          (if test1 (and test2 ...) #f))))

    (define-syntax or
      (syntax-rules ()
        ((_)
          #f)

        ((_ test)
          test)

        ((_ test1 test2 ...)
          (let ((x test1))
            (if x x (or test2 ...))))))

    (define-syntax boolean-or
      (syntax-rules ()
        ((_)
          #f)

        ((_ test)
          test)

        ((_ test1 test2 ...)
          (if test1 #t (boolean-or test2 ...)))))

    (define-syntax when
      (syntax-rules ()
        ((_ test result1 result2 ...)
          (if test
            (begin result1 result2 ...)))))

    (define-syntax unless
      (syntax-rules ()
        ((_ test result1 result2 ...)
          (when (not test) result1 result2 ...))))

    (define-syntax do
      (syntax-rules ()
        ((_ ((name initial step ...) ...)
            (test expression ...)
            command
            ...)
          (let loop ((name initial) ...)
            (if test
              (begin #f expression ...)
              (begin
                command
                ...
                (loop (do "step" name step ...) ...)))))

        ((_ "step" x)
          x)

        ((_ "step" x y)
          y)))

    ; Type IDs

    (define pair-type 0)
    (define null-type 1)
    (define boolean-type 2)
    (define procedure-type 3)
    (define symbol-type 4)
    (define string-type 5)
    (define char-type 6)
    (define vector-type 7)
    (define bytevector-type 8)
    (define record-type 9)

    ; Primitives

    (define (primitive id)
      ($$rib id '() procedure-type))

    (define rib $$rib)
    (define close (primitive 1))
    (define rib? (primitive 2))
    (define car (primitive 3))
    (define cdr (primitive 4))
    (define rib-tag (primitive 5))
    (define set-car! (primitive 6))
    (define set-cdr! (primitive 7))
    (define eq? (primitive 8))
    (define $< (primitive 9))
    (define $+ (primitive 10))
    (define $- (primitive 11))
    (define $* (primitive 12))
    (define $/ (primitive 13))
    (define remainder (primitive 14))
    (define $halt (primitive 40))
    (define null? (primitive 50))
    (define pair? (primitive 51))
    (define assq (primitive 60))
    (define cons (primitive 61))
    (define memq (primitive 62))
    (define eqv? (primitive 70))
    (define equal-inner? (primitive 71))
    (define exp (primitive 500))
    (define $log (primitive 501))
    (define infinite? (primitive 502))
    (define nan? (primitive 503))
    (define sqrt (primitive 504))

    (define (data-rib type car cdr)
      (rib car cdr type))

    (define (apply f x . xs)
      ($$apply
        f
        (let loop ((x x) (xs xs))
          (if (null? xs)
            x
            (cons x (loop (car xs) (cdr xs)))))))

    (define (comparison-operator f)
      (lambda xs
        (boolean-or
          (null? xs)
          (let loop ((x (car xs))
                     (xs (cdr xs)))
            (boolean-or
              (null? xs)
              (let ((y (car xs)))
                (and (f x y) (loop y (cdr xs)))))))))

    ; Basic types

    (define (instance? type)
      (lambda (x)
        (and
          (rib? x)
          (eq? (rib-tag x) type))))

    (define (equal? x y)
      (boolean-or
        (eq? x y)
        (and
          (equal-inner? x y)
          (equal? (car x) (car y))
          (equal? (cdr x) (cdr y)))))

    ;; Procedure

    (define procedure? (instance? procedure-type))

    ;; Boolean

    (define boolean? (instance? boolean-type))

    (define (not x)
      (eq? x #f))

    (define-optimizer not
      (syntax-rules ()
        ((_ x)
          (eq? x #f))))

    (define boolean=? (comparison-operator eq?))

    ;; Number

    (define (number? x)
      (not (rib? x)))

    (define complex? number?)
    (define real? complex?)
    (define rational? real?)
    (define (integer? x)
      (and
        (number? x)
        (zero? (remainder x 1))))

    (define exact? integer?)
    (define (inexact? x)
      (not (exact? x)))
    (define (exact-integer? x)
      (and (exact? x) (integer? x)))

    (define (zero? x) (eq? x 0))
    (define (positive? x) (> x 0))
    (define (negative? x) (< x 0))
    (define (even? x) (zero? (modulo x 2)))
    (define (odd? x) (not (even? x)))

    (define-optimizer zero?
      (syntax-rules ()
        ((_ x)
          (eq? x 0))))

    (define (arithmetic-operator f y)
      (lambda xs (fold f y xs)))

    (define (inverse-arithmetic-operator f y)
      (lambda (x . xs)
        (if (null? xs)
          (f y x)
          (fold f x xs))))

    (define + (arithmetic-operator $+ 0))
    (define - (inverse-arithmetic-operator $- 0))
    (define * (arithmetic-operator $* 1))
    (define / (inverse-arithmetic-operator $/ 1))

    (define-optimizer +
      (syntax-rules ()
        ((_ x y)
          ($+ x y))))

    (define-optimizer -
      (syntax-rules ()
        ((_ x y)
          ($- x y))))

    (define-optimizer *
      (syntax-rules ()
        ((_ x y)
          ($* x y))))

    (define-optimizer /
      (syntax-rules ()
        ((_ x y)
          ($/ x y))))

    (define (square x)
      (* x x))

    (define (quotient x y)
      (/ (- x (remainder x y)) y))

    (define truncate-quotient quotient)
    (define truncate-remainder remainder)

    (define (truncate/ x y)
      (values
        (truncate-quotient x y)
        (truncate-remainder x y)))

    (define (truncate x)
      (quotient x 1))

    (define (floor x)
      (floor-quotient x 1))

    (define (floor-quotient x y)
      (/ (- x (floor-remainder x y)) y))

    (define (floor-remainder x y)
      (+
        (remainder x y)
        (if (or
             (zero? (remainder x y))
             (eq? (negative? x) (negative? y)))
          0
          y)))

    (define (floor/ x y)
      (values
        (floor-quotient x y)
        (floor-remainder x y)))

    (define modulo floor-remainder)

    (define (ceiling x)
      (- (floor (- x))))

    (define (round x)
      (let* ((x (* x 2))
             (y (floor (/ (+ x 1) 2))))
        (if (= (modulo x 2) 1)
          (- y (modulo y 2))
          y)))

    (define exact round)
    (define (inexact x)
      x)

    (define (abs x)
      (if (negative? x)
        (- x)
        x))

    (define (log x . xs)
      (if (null? xs)
        ($log x)
        (/ ($log x) ($log (car xs)))))

    (define (expt x y)
      (exp (* (log x) y)))

    (define (exact-integer-sqrt x)
      (let ((y (floor (sqrt x))))
        (values y (- x (square y)))))

    (define (gcd x y)
      (if (zero? y)
        (abs x)
        (gcd y (remainder x y))))

    (define (lcm x y)
      (/
        (abs (* x y))
        (gcd x y)))

    (define = (comparison-operator eq?))
    (define < (comparison-operator $<))
    (define > (comparison-operator (lambda (x y) ($< y x))))
    (define <= (comparison-operator (lambda (x y) (not ($< y x)))))
    (define >= (comparison-operator (lambda (x y) (not ($< x y)))))

    (define-optimizer =
      (syntax-rules ()
        ((_ x y)
          (eq? x y))))

    (define-optimizer <
      (syntax-rules ()
        ((_ x y)
          ($< x y))))

    (define-optimizer >
      (syntax-rules ()
        ((_ x y)
          ($< y x))))

    (define (extremum f)
      (lambda (x . xs)
        (fold (lambda (x y) (if (f x y) x y)) x xs)))
    (define min (extremum $<))
    (define max (extremum (lambda (x y) ($< y x))))

    ; TODO Set a true machine epsilon.
    ;
    ; Currently, we have a precision limitation due to compression of floating point number in a compiler.
    (define epsilon
      ; Variadic arguments to arithmetic operators are not available at this point.
      (let ((x (/ 1000000000)))
        (if (zero? x) 1 x)))

    ;; Character

    (define char? (instance? char-type))

    (define (integer->char x)
      (data-rib char-type x '()))

    (define char->integer car)

    (define (char-compare compare)
      (lambda xs (apply compare (map char->integer xs))))

    (define char=? (char-compare =))
    (define char<? (char-compare <))
    (define char<=? (char-compare <=))
    (define char>? (char-compare >))
    (define char>=? (char-compare >=))

    ;; List

    (define (list? x)
      (boolean-or
        (null? x)
        (and
          (pair? x)
          (list? (cdr x)))))

    (define (caar x) (car (car x)))
    (define (cadr x) (car (cdr x)))
    (define (cdar x) (cdr (car x)))
    (define (cddr x) (cdr (cdr x)))

    (define (list . xs) xs)

    (define (make-list length . rest)
      (define fill (if (null? rest) #f (car rest)))

      (let loop ((length length))
        (if (zero? length)
          '()
          (cons fill (loop (- length 1))))))

    (define (length xs)
      (do ((xs xs (cdr xs)) (y 0 (+ y 1)))
        ((null? xs)
          y)))

    (define (map* f xs)
      (if (null? xs)
        xs
        (cons
          (f (car xs))
          (map* f (cdr xs)))))

    (define (map f x . xs)
      (if (null? xs)
        (map* f x)
        (let loop ((xs (cons x xs)))
          (if (memq #t (map* null? xs))
            '()
            (cons
              (apply f (map* car xs))
              (loop (map* cdr xs)))))))

    (define (for-each f x . xs)
      (let ((xs (cons x xs)))
        (if (memq #t (map* null? xs))
          #f
          (begin
            (apply f (map* car xs))
            (apply for-each f (map* cdr xs))))))

    (define (list-ref xs index)
      (car (list-tail xs index)))

    (define (list-set! xs index value)
      (set-car! (list-tail xs index) value))

    (define (list-head xs index)
      (if (zero? index)
        '()
        (cons
          (car xs)
          (list-head (cdr xs) (- index 1)))))

    (define (list-tail xs index)
      (if (boolean-or (zero? index) (not (pair? xs)))
        xs
        (list-tail (cdr xs) (- index 1))))

    (define (member x xs . rest)
      (define eq?
        (if (null? rest)
          equal?
          (car rest)))

      (let loop ((xs xs))
        (cond
          ((null? xs)
            #f)

          ((eq? x (car xs))
            xs)

          (else
            (loop (cdr xs))))))

    (define (memv x xs) (member x xs eqv?))

    (define (assoc x xs . rest)
      (define eq?
        (if (null? rest)
          equal?
          (car rest)))

      (let loop ((xs xs))
        (if (null? xs)
          #f
          (let ((pair (car xs)))
            (if (eq? x (car pair))
              pair
              (loop (cdr xs)))))))

    (define (assv x xs) (assoc x xs eqv?))

    (define (append . lists)
      (reduce-right append-lists '() lists))

    (define (append-lists ys xs)
      (if (null? xs)
        ys
        (cons (car xs) (append-lists ys (cdr xs)))))

    (define (reverse xs)
      (do ((xs xs (cdr xs)) (ys '() (cons (car xs) ys)))
        ((null? xs)
          ys)))

    (define (fold f y xs)
      (if (null? xs)
        y
        (fold
          f
          (f y (car xs))
          (cdr xs))))

    (define (reduce-right f y xs)
      (if (null? xs)
        y
        (let loop ((xs xs))
          (if (null? (cdr xs))
            (car xs)
            (f (loop (cdr xs)) (car xs))))))

    (define (list-copy xs . rest)
      (define start (if (null? rest) 0 (car rest)))
      (define end (if (or (null? rest) (null? (cdr rest))) #f (cadr rest)))

      (let ((xs (list-tail xs start)))
        (if end
          (list-head xs (- end start))
          xs)))

    ;; Sequence

    (define sequence-length car)
    (define sequence->list cdr)

    (define (list->sequence type)
      (lambda (xs)
        (data-rib type (length xs) xs)))

    (define (sequence-ref xs index)
      (list-ref (sequence->list xs) index))

    (define (sequence-set! xs index value)
      (list-set! (sequence->list xs) index value))

    (define (make-sequence list->sequence)
      (lambda (length . rest)
        (list->sequence (apply make-list (cons length rest)))))

    (define (sequence-append list->sequence)
      (lambda xs
        (list->sequence (apply append (map sequence->list xs)))))

    (define (sequence-copy list->sequence)
      (lambda (xs . rest)
        (list->sequence (apply list-copy (sequence->list xs) rest))))

    (define (sequence-copy! to at from . rest)
      (define start (if (null? rest) 0 (car rest)))
      (define end
        (if (or (null? rest) (null? (cdr rest)))
          (sequence-length from)
          (cadr rest)))

      (do ((xs
             (list-copy
               (sequence->list from)
               start
               (min end (+ start (- (sequence-length to) at))))
             (cdr xs))
           (ys
             (list-tail (sequence->list to) at)
             (cdr ys)))
        ((null? xs)
          #f)
        (set-car! ys (car xs))))

    ;; Vector

    (define vector? (instance? vector-type))

    (define (vector . xs)
      (list->vector xs))

    (define vector-length sequence-length)
    (define vector->list sequence->list)
    (define list->vector (list->sequence vector-type))
    (define vector-ref sequence-ref)
    (define vector-set! sequence-set!)
    (define make-vector (make-sequence list->vector))
    (define vector-append (sequence-append list->vector))
    (define vector-copy (sequence-copy list->vector))
    (define vector-copy! sequence-copy!)

    (define (vector-for-each f xs)
      (for-each f (vector->list xs)))

    (define (vector-map f xs)
      (list->vector (map f (vector->list xs))))

    (define (vector-fill! xs fill . rest)
      (define start (if (null? rest) 0 (car rest)))
      (define end
        (if (or (null? rest) (null? (cdr rest)))
          (vector-length xs)
          (cadr rest)))

      (do ((xs (list-tail (vector->list xs) start) (cdr xs)) (count (- end start) (- count 1)))
        ((or (null? xs) (<= count 0))
          #f)
        (set-car! xs fill)))

    (define (string->vector xs . rest)
      (apply vector-copy (list->vector (string->list xs)) rest))

    (define (vector->string xs . rest)
      (list->string (vector->list (apply vector-copy xs rest))))

    ;; Bytevector

    (define bytevector? (instance? bytevector-type))

    (define (bytevector . xs)
      (list->bytevector xs))

    (define bytevector-length sequence-length)
    (define bytevector->list sequence->list)
    (define list->bytevector (list->sequence bytevector-type))
    (define bytevector-u8-ref sequence-ref)
    (define bytevector-u8-set! sequence-set!)
    (define make-bytevector (make-sequence list->bytevector))
    (define bytevector-append (sequence-append list->bytevector))
    (define bytevector-copy (sequence-copy list->bytevector))
    (define bytevector-copy! sequence-copy!)

    ;; String

    (define string? (instance? string-type))

    (define (string-rib codes length)
      (data-rib string-type length codes))

    (define (string . xs)
      (list->string xs))

    (define string-length sequence-length)
    (define string->code-points sequence->list)
    (define code-points->string (list->sequence string-type))
    (define string-append (sequence-append code-points->string))
    (define string-copy (sequence-copy code-points->string))
    (define string-copy! sequence-copy!)
    (define substring string-copy)

    (define (list->string x)
      (code-points->string (map char->integer x)))

    (define (string->list x)
      (map integer->char (string->code-points x)))

    (define (string-ref x index)
      (integer->char (sequence-ref x index)))

    (define (make-string length . rest)
      ((make-sequence code-points->string)
        length
        (if (null? rest) 0 (char->integer (car rest)))))

    (define (string-for-each f xs)
      (for-each f (string->list xs)))

    (define (string-map f xs)
      (list->string (map f (string->list xs))))

    (define string=? (comparison-operator equal?))

    (define string<?
      (comparison-operator
        (lambda (x y)
          (integer-list<?
            (string->code-points x)
            (string->code-points y)))))

    (define (integer-list<? x y)
      (and
        (not (null? y))
        (boolean-or
          (null? x)
          (< (car x) (car y))
          (and
            (= (car x) (car y))
            (integer-list<? (cdr x) (cdr y))))))

    (define (string>? x y) (string<? y x))

    ;;; Number

    (define (number->string x . rest)
      (define radix (if (null? rest) 10 (car rest)))

      (define (format-digit x)
        (integer->char
          (if (< 9 x)
            (+ (char->integer #\a) (- x 10))
            (+ (char->integer #\0) x))))

      (define (format-point x)
        (if (< x epsilon)
          '()
          (cons
            #\.
            (let loop ((x x) (d epsilon) (ys '()))
              (if (< x d)
                '()
                (let* ((x (* x radix))
                       (r (remainder x 1))
                       (q (quotient x 1))
                       (d (* d radix)))
                  (if (< (- 1 r) d)
                    (cons
                      (format-digit (+ q 1))
                      '())
                    (cons
                      (format-digit q)
                      (loop r d ys)))))))))

      (cond
        ((infinite? x)
          (string-append
            (if (negative? x) "-" "")
            "infinity"))
        ((nan? x)
          "nan")
        (else
          (list->string
            (append
              (if (negative? x)
                (list #\-)
                '())
              (let loop ((x (abs x)) (ys '()))
                (let* ((q (quotient x radix))
                       (ys
                         (cons
                           (format-digit (quotient (remainder x radix) 1))
                           ys)))
                  (if (positive? q)
                    (loop q ys)
                    ys)))
              (format-point (remainder (abs x) 1)))))))

    (define (string->number x . rest)
      (define radix (if (null? rest) 10 (car rest)))

      (define digit-characters
        (map
          (lambda (pair)
            (cons
              (cons
                (char->integer (caar pair))
                (char->integer (cdar pair)))
              (cdr pair)))
          '(((#\0 . #\9) . 0)
            ((#\A . #\Z) . 10)
            ((#\a . #\z) . 10))))

      (define (convert-digit x)
        (let* ((x (char->integer x))
               (y
                 (member
                   x
                   digit-characters
                   (lambda (x pair) (<= (caar pair) x (cdar pair))))))
          (and
            y
            (let* ((y (car y))
                   (y (+ (- x (caar y)) (cdr y))))
              (and (< y radix) y)))))

      (define (convert-point xs)
        (let loop ((xs xs) (y 0) (d 1))
          (if (null? xs)
            (/ y d)
            (let ((x (convert-digit (car xs))))
              (and
                x
                (loop
                  (cdr xs)
                  (+ (* radix y) x)
                  (* d radix)))))))

      (define (convert xs)
        (and
          (pair? xs)
          (let loop ((initial #t) (xs xs) (y 0))
            (cond
              ((null? xs)
                y)

              ((and
                  (not initial)
                  (eqv? (car xs) #\.))
                (+ y (convert-point (cdr xs))))

              (else
                (let ((x (convert-digit (car xs))))
                  (and x (loop #f (cdr xs) (+ (* radix y) x)))))))))

      (let ((xs (string->list x)))
        (if (and (pair? xs) (eqv? (car xs) #\-))
          (let ((x (convert (cdr xs))))
            (and x (- x)))
          (convert xs))))

    ;; Symbol

    (define symbol? (instance? symbol-type))

    (define symbol=? (comparison-operator eq?))

    (define symbol->string cdr)

    (define (string->uninterned-symbol x)
      (data-rib symbol-type #f x))

    ;; Record

    ; We use record types only for certain built-in types not to degrade space
    ; efficiency of their values.
    (define-syntax define-record-type
      (syntax-rules ()
        ((_ id
            (constructor field1 ...)
            predicate
            (field2 accessor ...)
            ...)
          (begin
            (define id (cons 0 0))
            (define constructor (record-constructor id))
            (define predicate (record-predicate id))
            (define-record-fields 0 (accessor ...) ...)))))

    (define-syntax define-record-fields
      (syntax-rules ()
        ((_ index)
          #f)

        ((_ index (accessor1 ...) (accessor2 ...) ...)
          (begin
            (define-record-field index accessor1 ...)
            (define-record-fields (+ index 1) (accessor2 ...) ...)))))

    (define-syntax define-record-field
      (syntax-rules ()
        ((_ index getter)
          (define getter (record-getter index)))

        ((_ index getter setter)
          (begin
            (define-record-field index getter)
            (define setter (record-setter index))))))

    (define record? (instance? record-type))

    (define (record-constructor id)
      (lambda xs
        (data-rib record-type id xs)))

    (define (record-predicate id)
      (lambda (x)
        (and
          (record? x)
          (eq? (car x) id))))

    (define (record-getter index)
      (lambda (record)
        (list-ref (cdr record) index)))

    (define (record-setter index)
      (lambda (record value)
        (list-set! (cdr record) index value)))

    ;; Tuple

    ; A tuple is primarily used to represent multiple values.
    (define-record-type tuple
      (make-tuple values)
      tuple?
      (values tuple-values))

    ; Control

    ;; Multi-value

    (define-syntax define-values
      (syntax-rules ()
        ((_ () value)
          value)

        ((_ (name) value)
          (define name (call-with-values (lambda () value) (lambda (x) x))))

        ((_ (name1 name2 ... last-name) value)
          (begin
            (define name1 (call-with-values (lambda () value) list))
            (define name2
              (let ((x (cadr name1)))
                (set-cdr! name1 (cddr name1))
                x))
            ...
            (define last-name
              (let ((x (cadr name1)))
                (set! name1 (car name1))
                x))))

        ((_ (name1 name2 ... . last-name) value)
          (begin
            (define name1 (call-with-values (lambda () value) list))
            (define name2
              (let ((x (cadr name1)))
                (set-cdr! name1 (cddr name1))
                x))
            ...
            (define last-name
              (let ((x (cdr name1)))
                (set! name1 (car name1))
                x))))

        ((_ name value)
          (define name (call-with-values (lambda () value) list)))))

    (define-syntax let-values
      (syntax-rules ()
        ((_ (binding ...) body1 body2 ...)
          (let-values "multiple" (binding ...) () (let () body1 body2 ...)))

        ((_ "multiple" () singles body)
          (let singles body))

        ((_ "multiple" ((names value) binding ...) singles body)
          (let-values "single" names value () (binding ...) singles body))

        ((_ "single" () value arguments bindings singles body)
          (call-with-values
            (lambda () value)
            (lambda arguments
              (let-values "multiple" bindings singles body))))

        ((_ "single" (name . names) value (argument ...) bindings (single ...) body)
          (let-values "single"
            names
            value
            (argument ... x)
            bindings
            (single ... (name x))
            body))

        ((_ "single" name value (argument ...) bindings (single ...) body)
          (call-with-values
            (lambda () value)
            (lambda (argument ... . x)
              (let-values "multiple" bindings (single ... (name x)) body))))))

    (define-syntax let*-values
      (syntax-rules ()
        ((_ () body1 body2 ...)
          (let () body1 body2 ...))

        ((_ (binding1 binding2 ...) body1 body2 ...)
          (let-values (binding1)
            (let*-values (binding2 ...) body1 body2 ...)))))

    ; TODO Implement multiple values based on continuations as described in R7RS.
    (define (values . xs)
      (if (and (pair? xs) (null? (cdr xs)))
        (car xs)
        (make-tuple xs)))

    (define (call-with-values producer consumer)
      (let ((xs (producer)))
        (if (tuple? xs)
          (apply consumer (tuple-values xs))
          (consumer xs))))

    (define (error message . xs)
      (write-message message)
      ($halt))

    ; Dummy implementation
    (define (write-message . xs)
      #f)))

(define-library (scheme cxr)
  (export
    caaar
    caadr
    cadar
    caddr
    cdaar
    cdadr
    cddar
    cdddr
    caaaar
    caaadr
    caadar
    caaddr
    cadaar
    cadadr
    caddar
    cadddr
    cdaaar
    cdaadr
    cdadar
    cdaddr
    cddaar
    cddadr
    cdddar
    cddddr)

  (import (stak base))

  (begin
    (define (caaar x) (car (caar x)))
    (define (caadr x) (car (cadr x)))
    (define (cadar x) (car (cdar x)))
    (define (caddr x) (car (cddr x)))
    (define (cdaar x) (cdr (caar x)))
    (define (cdadr x) (cdr (cadr x)))
    (define (cddar x) (cdr (cdar x)))
    (define (cdddr x) (cdr (cddr x)))
    (define (caaaar x) (car (caaar x)))
    (define (caaadr x) (car (caadr x)))
    (define (caadar x) (car (cadar x)))
    (define (caaddr x) (car (caddr x)))
    (define (cadaar x) (car (cdaar x)))
    (define (cadadr x) (car (cdadr x)))
    (define (caddar x) (car (cddar x)))
    (define (cadddr x) (car (cdddr x)))
    (define (cdaaar x) (cdr (caaar x)))
    (define (cdaadr x) (cdr (caadr x)))
    (define (cdadar x) (cdr (cadar x)))
    (define (cdaddr x) (cdr (caddr x)))
    (define (cddaar x) (cdr (cdaar x)))
    (define (cddadr x) (cdr (cdadr x)))
    (define (cdddar x) (cdr (cddar x)))
    (define (cddddr x) (cdr (cdddr x)))))

(define-library (srfi 1)
  (export
    iota

    reduce
    fold-right
    append-map

    filter

    find
    find-tail
    any
    every
    list-index

    delete-duplicates

    ; Re-exports
    fold
    reduce-right

    cons
    list
    null?
    pair?
    car
    cdr
    caar
    cadr
    cdar
    cddr
    caaar
    caadr
    cadar
    caddr
    cdaar
    cdadr
    cddar
    cdddr
    caaaar
    caaadr
    caadar
    caaddr
    cadaar
    cadadr
    caddar
    cadddr
    cdaaar
    cdaadr
    cdadar
    cdaddr
    cddaar
    cddadr
    cdddar
    cddddr
    list-ref
    length
    append
    reverse
    map
    for-each
    member
    memq
    memv
    assoc
    assq
    assv
    set-car!
    set-cdr!)

  (import (stak base) (scheme cxr))

  (begin
    (define (append-map f xs)
      (apply append (map f xs)))

    (define (delete-duplicates xs)
      (if (null? xs)
        '()
        (let ((ys (delete-duplicates (cdr xs))))
          (if (memq (car xs) ys)
            ys
            (cons (car xs) ys)))))

    (define (filter f xs)
      (if (null? xs)
        '()
        (let ((x (car xs))
              (xs (filter f (cdr xs))))
          (if (f x)
            (cons x xs)
            xs))))

    (define (fold-right f y xs)
      (if (null? xs)
        y
        (f (fold-right f y (cdr xs)) (car xs))))

    (define (iota count . rest)
      (define start (if (null? rest) 0 (car rest)))
      (define step (if (or (null? rest) (null? (cdr rest))) 1 (cadr rest)))

      (let loop ((count count) (x start))
        (if (> count 0)
          (cons x (loop (- count 1) (+ x step)))
          '())))

    (define (find f xs)
      (cond
        ((find-tail f xs) =>
          car)
        (else
          #f)))

    (define (find-tail f xs)
      (let loop ((xs xs))
        (cond
          ((null? xs)
            #f)
          ((f (car xs))
            xs)
          (else
            (loop (cdr xs))))))

    (define (list-index f x . xs)
      (let loop ((xs (cons x xs)) (i 0))
        (cond
          ((find-tail null? xs)
            #f)
          ((apply f (map car xs))
            i)
          (else
            (loop (map cdr xs) (+ i 1))))))

    (define (any f x . xs)
      (and (apply list-index f x xs) #t))

    (define (every f x . xs)
      (not (apply any (lambda (y) (not (f y))) x xs)))

    (define (reduce f y xs)
      (if (null? xs)
        y
        (fold f (car xs) (cdr xs))))))

(define-library (stak parameter)
  (export make-parameter)

  (import (stak base))

  (begin
    (define (make-parameter x . rest)
      (define convert (if (pair? rest) (car rest) (lambda (x) x)))
      (set! x (convert x))

      (lambda rest
        (if (null? rest)
          x
          (set! x (convert (car rest))))))))

(define-library (stak io)
  (export
    eof-object
    eof-object?

    make-port
    make-input-port
    make-output-port
    port?
    input-port?
    output-port?
    textual-port?
    binary-port?

    current-input-port
    current-output-port
    current-error-port

    close-port
    close-input-port
    close-output-port
    call-with-port

    input-port-open?
    output-port-open?

    read-u8
    peek-u8
    u8-ready?
    read-char
    peek-char
    char-ready?
    read-string
    read-line
    read-bytevector
    read-bytevector!

    write-u8
    write-char
    write-string
    write-bytevector
    newline

    flush-output-port

    open-input-string
    open-output-string
    get-output-string
    open-input-bytevector
    open-output-bytevector
    get-output-bytevector)

  (import (stak base) (stak parameter))

  (begin
    (define $read-input (primitive 100))
    (define $write-output (primitive 101))
    (define $write-error (primitive 102))

    ; EOF object

    (define-record-type eof-object
      (make-eof-object)
      eof-object?)

    (define eof-object
      (let ((eof (make-eof-object)))
        (lambda () eof)))

    ; Port

    (define-record-type port
      (make-port read write flush close data)
      port?
      (read port-read port-set-read!)
      (write port-write port-set-write!)
      (flush port-flush port-set-flush!)
      (close port-close port-set-close!)
      (data port-data port-set-data!))

    (define input-port? port-read)
    (define output-port? port-write)
    (define textual-port? port?)
    (define binary-port? port?)

    (define (make-input-port read close)
      (make-port read #f #f close '()))

    (define (make-output-port write flush close . rest)
      (make-port
        #f
        write
        flush
        close
        (if (null? rest) #f (car rest))))

    (define current-input-port
      (make-parameter
        (make-input-port
          $read-input
          (lambda () #f))))

    (define current-output-port
      (make-parameter
        (make-output-port
          $write-output
          (lambda () #f)
          (lambda () #f))))

    (define current-error-port
      (make-parameter
        (make-output-port
          $write-error
          (lambda () #f)
          (lambda () #f))))

    ; Close

    (define (close-port port)
      (let ((close (port-close port)))
        (unless close
          (error "cannot close port"))
        (close)
        (for-each
          (lambda (set-field!)
            (set-field! port #f))
          (list
            port-set-read!
            port-set-write!
            port-set-flush!
            port-set-close!
            port-set-data!))))

    (define close-input-port close-port)
    (define close-output-port close-port)

    (define (call-with-port port f)
      (let ((x (f port)))
        (close-port port)
        x))

    (define input-port-open? port-close)
    (define output-port-open? port-close)

    ; Read

    (define (get-input-port rest)
      (if (null? rest) (current-input-port) (car rest)))

    (define (read-u8 . rest)
      (let* ((port (get-input-port rest))
             (buffer (port-data port)))
        (if (pair? buffer)
          (begin
            (port-set-data! port (cdr buffer))
            (car buffer))
          (let ((read (port-read port)))
            (unless read
              (error "cannot read from port"))
            (or (read) (eof-object))))))

    (define (peek-u8 . rest)
      (let* ((port (get-input-port rest))
             (x (read-u8 port)))
        (port-set-data! port (append (port-data port) (list x)))
        x))

    (define (u8-ready? . rest)
      ; TODO Fix this cheating!
      (apply peek-u8 rest)
      #t)

    (define (read-char-bytes port)
      (let ((byte (read-u8 port)))
        (cond
          ((eof-object? byte)
            '())
          ((zero? (quotient byte 128))
            (list byte))
          (else
            (let* ((count
                     (cond
                       ((= (quotient byte 32) 6)
                         1)
                       ((= (quotient byte 16) 14)
                         2)
                       (else
                         3)))
                   (bytes
                     (let loop ((count count))
                       (if (zero? count)
                         '()
                         (let ((x (read-u8 port)))
                           (and
                             (number? x)
                             (let ((xs (loop (- count 1))))
                               (and xs (cons x xs)))))))))
              (if bytes
                (cons byte bytes)
                '()))))))

    (define (parse-char-bytes bytes)
      (cond
        ((null? bytes)
          (eof-object))
        ((null? (cdr bytes))
          (integer->char (car bytes)))
        (else
          (integer->char
            (let loop ((bytes (cdr bytes)) (code (car bytes)) (size 64))
              (if (null? bytes)
                (remainder code size)
                (loop
                  (cdr bytes)
                  (+ (* 64 code) (- (car bytes) 128))
                  (* size 32))))))))

    (define (read-char . rest)
      (let ((xs (read-char-bytes (get-input-port rest))))
        (if (null? xs)
          (eof-object)
          (parse-char-bytes xs))))

    (define (peek-char . rest)
      (let* ((port (get-input-port rest))
             (bytes (read-char-bytes port)))
        (if (null? bytes)
          (eof-object)
          (begin
            (port-set-data! port (append (port-data port) bytes))
            (parse-char-bytes bytes)))))

    (define (char-ready? . rest)
      (let ((port (get-input-port rest)))
        (or
          (not (eof-object? (peek-char port)))
          (eof-object? (peek-u8 port)))))

    (define (read-string count . rest)
      (define port (get-input-port rest))

      (list->string
        (let loop ((count count))
          (let ((x (read-char port)))
            (if (or (eof-object? x) (zero? count))
              '()
              (cons x (loop (- count 1))))))))

    (define (read-line . rest)
      (define port (get-input-port rest))

      (list->string
        (let loop ()
          (let ((x (read-char port)))
            (if (or (eof-object? x) (eqv? x #\newline))
              '()
              (cons x (loop)))))))

    (define (read-bytevector count . rest)
      (define port (get-input-port rest))

      (list->bytevector
        (let loop ((count count))
          (let ((x (read-u8 port)))
            (if (or (eof-object? x) (zero? count))
              '()
              (cons x (loop (- count 1))))))))

    (define (read-bytevector! xs . rest)
      (define port (get-input-port rest))
      (define start
        (if (or
             (null? rest)
             (null? (cdr rest)))
          0
          (cadr rest)))
      (define end
        (if (or
             (null? rest)
             (null? (cdr rest))
             (null? (cddr rest)))
          #f
          (car (cddr rest))))

      (do ((start start (+ start 1))
           (xs (list-tail (bytevector->list xs) start) (cdr xs))
           (x (peek-u8 port) (peek-u8 port)))
        ((or
            (null? xs)
            (eof-object? x)
            (and end (>= start end))))
        (set-car! xs (read-u8 port))))

    ; Write

    (define (get-output-port rest)
      (if (null? rest) (current-output-port) (car rest)))

    (define (write-u8 byte . rest)
      (let ((write (port-write (get-output-port rest))))
        (unless write
          (error "cannot write to port"))
        (write byte)))

    (define (write-trailing-bytes integer port)
      (let ((upper (quotient integer 64)))
        (unless (zero? upper)
          (write-trailing-bytes upper port))
        (write-u8 (+ 128 (remainder integer 64)) port)))

    (define (write-char x . rest)
      (let ((port (get-output-port rest))
            (integer (char->integer x)))
        (if (zero? (quotient integer 128))
          (write-u8 integer port)
          (let loop ((head 32) (offset 64) (mask 192))
            (if (zero? (quotient integer (* head offset)))
              (begin
                ; TODO Use `floor/`?
                (write-u8 (+ mask (quotient integer offset)) port)
                (write-trailing-bytes (remainder integer offset) port))
              (loop (/ head 2) (* offset 64) (+ mask head)))))))

    (define (write-string x . rest)
      (let ((port (get-output-port rest)))
        (for-each
          (lambda (x) (write-char x port))
          (string->list x))))

    (define (write-bytevector xs . rest)
      (let ((port (get-output-port rest)))
        (do ((index 0 (+ index 1)))
          ((= index (bytevector-length xs))
            #f)
          (write-u8 (bytevector-u8-ref xs index) port))))

    (define (newline . rest)
      (write-char #\newline (get-output-port rest)))

    (set! write-message
      (lambda (x)
        (write-string x (current-error-port))))

    ; Flush

    (define (flush-output-port . rest)
      (let ((flush (port-flush (get-output-port rest))))
        (unless flush
          (error "cannot flush port"))
        (flush)))

    ; In-memory ports

    (define (open-input-string xs)
      (let ((xs (string->code-points xs))
            (ys '()))
        (make-input-port
          (lambda ()
            (when (and
                   (null? ys)
                   (not (null? xs)))
              (let ((port (open-output-bytevector)))
                (write-char (integer->char (car xs)) port)
                (set! xs (cdr xs))
                (set! ys (bytevector->list (get-output-bytevector port)))))
            (and
              (pair? ys)
              (let ((y (car ys)))
                (set! ys (cdr ys))
                y)))
          (lambda () #f))))

    (define (open-output-string)
      (let* ((xs (string))
             (tail xs)
             (port (open-output-bytevector)))
        (make-output-port
          (lambda (x)
            (write-u8 x port)
            (let ((x (read-char (open-input-bytevector (get-output-bytevector port)))))
              (when (char? x)
                (set! port (open-output-bytevector))
                (set-car! xs (+ 1 (string-length xs)))
                (set-cdr! tail (list (char->integer x)))
                (set! tail (cdr tail)))))
          (lambda () #f)
          (lambda () #f)
          xs)))

    (define get-output-string port-data)

    (define (open-input-bytevector xs)
      (let ((xs (bytevector->list xs)))
        (make-input-port
          (lambda ()
            (and
              (pair? xs)
              (let ((x (car xs)))
                (set! xs (cdr xs))
                x)))
          (lambda () #f))))

    (define (open-output-bytevector)
      (let* ((xs (bytevector))
             (tail xs))
        (make-output-port
          (lambda (x)
            (set-car! xs (+ (bytevector-length xs) 1))
            (set-cdr! tail (list x))
            (set! tail (cdr tail)))
          (lambda () #f)
          (lambda () #f)
          xs)))

    (define get-output-bytevector port-data)))

(define-library (stak unicode)
  (export string->utf8 utf8->string)

  (import (stak base) (stak io))

  (begin
    ; TODO Use the `expt` procedure.
    (define limit (* 1024 1024 1024 1024))

    (define (string->utf8 xs)
      (read-bytevector limit (open-input-string xs)))

    (define (utf8->string xs)
      (read-string limit (open-input-bytevector xs)))))

(define-library (stak continue)
  (export
    call/cc
    call-with-current-continuation
    dynamic-wind
    parameterize)

  (import (stak base) (stak parameter) (stak io))

  (begin
    ; Continuation

    (define dummy-procedure (lambda () #f))

    (define (call/cc receiver)
      (let ((continuation (cadr (cddr (close dummy-procedure))))
            (point current-point))
        (receiver
          (lambda (argument)
            (travel-to-point! current-point point)
            (set-current-point! point)
            (set-car!
              (cddr (close dummy-procedure)) ; frame
              continuation)
            argument))))

    (define call-with-current-continuation call/cc)

    ; Dynamic wind

    (define-record-type point
      (make-point depth before after parent)
      point?
      (depth point-depth)
      (before point-before)
      (after point-after)
      (parent point-parent))

    (define current-point (make-point 0 #f #f #f))

    (define (set-current-point! x)
      (set! current-point x))

    (define (dynamic-wind before thunk after)
      (before)
      (let ((point current-point))
        (set-current-point! (make-point (+ (point-depth point) 1) before after point))
        (let ((value (thunk)))
          (set-current-point! point)
          (after)
          value)))

    (define (travel-to-point! from to)
      (cond
        ((eq? from to)
          #f)

        ((< (point-depth from) (point-depth to))
          (travel-to-point! from (point-parent to))
          ((point-before to)))

        (else
          ((point-after from))
          (travel-to-point! (point-parent from) to))))

    ; Parameter

    (define-syntax parameterize
      (syntax-rules ()
        ((_ () body ...)
          (begin body ...))

        ((_ ((parameter1 value1) (parameter2 value2) ...) body ...)
          (let* ((parameter parameter1)
                 (old (parameter)))
            (dynamic-wind
              (lambda () (parameter value1))
              (lambda () (parameterize ((parameter2 value2) ...) body ...))
              (lambda () (parameter old)))))))))

(define-library (stak exception)
  (export
    error-object?
    error-object-message
    error-object-irritants
    with-exception-handler
    raise
    raise-continuable
    read-error
    file-error
    read-error?
    file-error?
    guard

    unwind

    write-irritant)

  (import (stak base) (stak parameter) (stak io) (stak continue))

  (begin
    (define $halt (primitive 40))

    (define-record-type error-object
      (make-error-object type message irritants)
      error-object?
      (type error-object-type)
      (message error-object-message)
      (irritants error-object-irritants))

    (define (convert-exception-handler handler)
      (lambda (pair)
        (let* ((exception (cdr pair))
               (value (handler exception)))
          (unless (car pair)
            (error "exception handler returned on non-continuable exception" exception))
          value)))

    (define current-exception-handler
      (make-parameter
        (convert-exception-handler
          (lambda (exception)
            (unwind
              (lambda ()
                (parameterize ((current-output-port (current-error-port)))
                  (if (error-object? exception)
                    (begin
                      (write-string (error-object-message exception))
                      (for-each
                        (lambda (value)
                          (write-char #\space)
                          (write-irritant value))
                        (error-object-irritants exception)))
                    (write-irritant exception))
                  (newline)
                  ($halt))))))
        (lambda (handler)
          ; Set an exception handler for runtime errors.
          (set-cdr!
            '()
            (lambda (message)
              (handler (cons #f (make-error-object 'runtime (code-points->string message) '())))))
          handler)))

    (define (with-exception-handler handler thunk)
      (let ((new (convert-exception-handler handler))
            (old (current-exception-handler)))
        (parameterize ((current-exception-handler
                         (lambda (exception)
                           (parameterize ((current-exception-handler old))
                             (new exception)))))
          (thunk))))

    (define (raise-value continuable)
      (lambda (value)
        ((current-exception-handler) (cons continuable value))))

    (define raise (raise-value #f))
    (define raise-continuable (raise-value #t))

    (define (error-type type)
      (lambda (message . rest)
        (raise (make-error-object type message rest))))

    (define (error-type? type)
      (lambda (error)
        (eq? (error-object-type error) type)))

    (set! error (error-type #f))
    (define read-error (error-type 'read))
    (define file-error (error-type 'file))

    (define read-error? (error-type? 'read))
    (define file-error? (error-type? 'file))

    (define-syntax guard
      (syntax-rules ()
        ((_ (name clause ...) body1 body2 ...)
          ((call/cc
              (lambda (continue-guard)
                (with-exception-handler
                  (lambda (exception)
                    ((call/cc
                        (lambda (continue-handler)
                          (continue-guard
                            (lambda ()
                              (let ((name exception))
                                (guard*
                                  (continue-handler
                                    (lambda () (raise-continuable name)))
                                  clause
                                  ...))))))))
                  (lambda ()
                    (let ((x (begin body1 body2 ...)))
                      (continue-guard (lambda () x)))))))))))

    (define-syntax guard*
      (syntax-rules (else =>)
        ((_ re-raise (else result1 result2 ...))
          (begin result1 result2 ...))

        ((_ re-raise (test => result))
          (let ((temp test))
            (if temp
              (result temp)
              re-raise)))

        ((_ re-raise (test => result) clause1 clause2 ...)
          (let ((temp test))
            (if temp
              (result temp)
              (guard* re-raise clause1 clause2 ...))))

        ((_ re-raise (test))
          (or test re-raise))

        ((_ re-raise (test) clause1 clause2 ...)
          (let ((temp test))
            (if temp
              temp
              (guard* re-raise clause1 clause2 ...))))

        ((_ re-raise (test result1 result2 ...))
          (if test
            (begin result1 result2 ...)
            re-raise))

        ((_ re-raise (test result1 result2 ...) clause1 clause2 ...)
          (if test
            (begin result1 result2 ...)
            (guard* re-raise clause1 clause2 ...)))))

    ; Unwind

    (define unwind #f)

    ((call/cc
        (lambda (continuation)
          (set! unwind continuation)
          (lambda () #f))))

    ; Dummy implementation
    (define (write-irritant value . rest)
      (write-string "<unknown>" (get-output-port rest)))))

(define-library (scheme base)
  (export
    syntax-rules
    define-syntax
    syntax-error
    _
    ...
    define
    lambda
    let-syntax
    letrec-syntax
    begin
    quasiquote
    unquote
    unquote-splicing
    quote
    set!
    let
    let*
    letrec
    letrec*
    define-values
    let-values
    let*-values
    if
    cond
    case
    else
    =>
    and
    or
    when
    unless
    do

    cond-expand
    features

    base
    library
    r7rs
    scheme
    stak

    rib
    cons
    close
    rib?
    car
    cdr
    rib-tag
    set-car!
    set-cdr!
    eq?

    apply
    data-rib

    eqv?
    equal?

    procedure?

    boolean?
    not
    boolean=?

    integer?
    rational?
    real?
    complex?
    number?
    exact?
    inexact?
    exact-integer?
    zero?
    positive?
    negative?
    even?
    odd?
    +
    -
    *
    /
    remainder
    quotient
    modulo
    truncate
    truncate-quotient
    truncate-remainder
    truncate/
    floor
    floor-quotient
    floor-remainder
    floor/
    ceiling
    round
    exact
    inexact
    abs
    expt
    square
    exact-integer-sqrt
    gcd
    lcm
    =
    <
    >
    <=
    >=
    min
    max

    char?
    integer->char
    char->integer
    char=?
    char<?
    char<=?
    char>?
    char>=?

    null?
    pair?
    list?
    caar
    cadr
    cdar
    cddr
    list
    make-list
    length
    map
    for-each
    list-ref
    list-set!
    list-tail
    member
    memq
    memv
    assoc
    assq
    assv
    append
    reverse
    list-copy

    vector?
    vector
    make-vector
    vector-append
    vector-copy
    vector-copy!
    vector-fill!
    vector-for-each
    vector-length
    vector-map
    vector-ref
    vector-set!
    list->vector
    vector->list
    string->vector
    vector->string
    string->utf8
    utf8->string

    bytevector?
    bytevector
    make-bytevector
    bytevector-append
    bytevector-copy
    bytevector-copy!
    bytevector-length
    bytevector-u8-ref
    bytevector-u8-set!
    list->bytevector
    bytevector->list

    string?
    string
    list->string
    string->list
    string-append
    string-length
    string-ref
    number->string
    string->number
    string-copy
    string-copy!
    substring
    make-string
    string-for-each
    string-map
    string=?
    string<?
    string>?

    symbol?
    symbol=?
    symbol->string
    string->uninterned-symbol
    string->symbol
    make-symbol-table

    define-record-type
    record?

    values
    call-with-values

    call/cc
    call-with-current-continuation

    dynamic-wind

    make-parameter
    parameterize

    error-object?
    error-object-message
    error-object-irritants
    with-exception-handler
    raise
    raise-continuable
    error
    read-error
    file-error
    read-error?
    file-error?
    guard

    unwind

    eof-object
    eof-object?

    make-port
    make-input-port
    make-output-port
    port?
    input-port?
    output-port?
    textual-port?
    binary-port?

    current-input-port
    current-output-port
    current-error-port

    close-port
    close-input-port
    close-output-port
    call-with-port

    input-port-open?
    output-port-open?

    read-u8
    peek-u8
    u8-ready?
    read-char
    peek-char
    char-ready?
    read-string
    read-line
    read-bytevector
    read-bytevector!

    write-u8
    write-char
    write-string
    write-bytevector
    newline

    flush-output-port

    open-input-string
    open-output-string
    get-output-string
    open-input-bytevector
    open-output-bytevector
    get-output-bytevector

    write-irritant)

  (import
    (stak base)
    (stak parameter)
    (stak io)
    (stak unicode)
    (stak continue)
    (stak exception))

  (begin
    ; Symbol table

    (define-record-type symbol-table
      (make-symbol-table symbols)
      symbol-table?
      (symbols symbol-table-symbols symbol-table-set-symbols!))

    (define string->symbol
      (let ((global-table (make-symbol-table ($$symbols))))
        (lambda (name . rest)
          (define table (if (null? rest) global-table (car rest)))

          (cond
            ((member
                name
                (symbol-table-symbols table)
                (lambda (name symbol) (equal? name (symbol->string symbol))))
              =>
              car)

            (else
              (let ((name (string->uninterned-symbol name)))
                (symbol-table-set-symbols! table (cons name (symbol-table-symbols table)))
                name))))))))

(define-library (scheme inexact)
  (export
    exp
    log
    finite?
    infinite?
    nan?
    sqrt
    cos
    sin
    tan
    acos
    asin
    atan)

  (import
    (scheme base)
    (only (stak base)
      primitive
      exp
      log
      infinite?
      nan?))

  (begin
    (define sqrt (primitive 504))
    (define cos (primitive 505))
    (define sin (primitive 506))
    (define tan (primitive 507))
    (define acos (primitive 508))
    (define asin (primitive 509))
    (define atan (primitive 510))

    (define (finite? x)
      (not (or (infinite? x) (nan? x))))))

(define-library (scheme complex)
  (export
    make-rectangular
    make-polar
    real-part
    imag-part
    magnitude
    angle)

  (import (scheme base) (scheme inexact))

  (begin
    (define (make-rectangular x y) x)
    (define (make-polar x y) (* x (cos y)))
    (define (real-part x) x)
    (define (imag-part x) 0)
    (define magnitude abs)
    (define (angle x)
      (if (negative? x) (acos -1) 0))))

(define-library (scheme case-lambda)
  (export case-lambda)

  (import (scheme base))

  (begin
    (define-syntax case-lambda
      (syntax-rules ()
        ((_ (parameters outer-body ...) ...)
          (lambda arguments
            (let ((arity (length arguments)))
              (letrec-syntax ((clause
                                (syntax-rules ::: ()
                                  ((_)
                                    (error "no matching clause"))
                                  ((_ ((parameter :::) . body) . rest)
                                    (if (= arity (length '(parameter :::)))
                                      (apply
                                        (lambda (parameter :::) . body)
                                        arguments)
                                      (clause . rest)))
                                  ((_ ((parameter ::: . tail) . body) . rest)
                                    (if (>= arity (length '(parameter :::)))
                                      (apply
                                        (lambda (parameter ::: . tail) . body)
                                        arguments)
                                      (clause . rest))))))
                (clause (parameters outer-body ...) ...)))))))))

(define-library (scheme char)
  (export
    char-ci=?
    char-ci<?
    char-ci>?
    char-ci<=?
    char-ci>=?
    char-alphabetic?
    char-numeric?
    char-whitespace?
    char-lower-case?
    char-upper-case?
    char-downcase
    char-foldcase
    char-upcase
    string-downcase
    string-foldcase
    string-upcase
    digit-value

    special-chars)

  (import (scheme base))

  ; TODO Support Unicode.
  (begin
    (define special-chars
      '(("alarm" . #\alarm)
        ("backspace" . #\backspace)
        ("delete" . #\delete)
        ("escape" . #\escape)
        ("newline" . #\newline)
        ("null" . #\null)
        ("return" . #\return)
        ("space" . #\space)
        ("tab" . #\tab)))

    (define (char-compare-ci compare)
      (lambda xs
        (apply compare (map char-downcase xs))))

    (define char-ci=? (char-compare-ci char=?))
    (define char-ci<? (char-compare-ci char<?))
    (define char-ci>? (char-compare-ci char>?))
    (define char-ci<=? (char-compare-ci char<=?))
    (define char-ci>=? (char-compare-ci char>=?))

    (define (char-alphabetic? x)
      (or (char-lower-case? x) (char-upper-case? x)))

    (define (char-numeric? x)
      (char<=? #\0 x #\9))

    (define (char-whitespace? x)
      (memv x '(#\newline #\return #\space #\tab)))

    (define (char-lower-case? x)
      (char<=? #\a x #\z))

    (define (char-upper-case? x)
      (char<=? #\A x #\Z))

    (define (char-upcase x)
      (if (char-lower-case? x)
        (integer->char (- (char->integer x) 32))
        x))

    (define (char-downcase x)
      (if (char-upper-case? x)
        (integer->char (+ (char->integer x) 32))
        x))

    (define char-foldcase char-downcase)

    (define (string-case f)
      (lambda (xs)
        (list->string (map f (string->list xs)))))

    (define string-downcase (string-case char-downcase))
    (define string-foldcase (string-case char-foldcase))
    (define string-upcase (string-case char-upcase))

    (define (digit-value x)
      (- (char->integer x) (char->integer #\0)))))

(define-library (scheme read)
  (export read)

  (import
    (scheme base)
    (scheme char)
    (only (stak base) boolean-or))

  (begin
    (define (read . rest)
      (define (read-raw)
        (let ((char (peek-non-whitespace-char)))
          (cond
            ((eof-object? char)
              char)

            ((eqv? char #\()
              (read-list))

            ((eqv? char #\#)
              (read-char)
              (case (peek-char)
                ((#\f)
                  (read-char)
                  #f)

                ((#\t)
                  (read-char)
                  #t)

                ((#\\)
                  (read-char)
                  (let ((char (peek-char)))
                    (if (char-whitespace? char)
                      (read-char)
                      (let ((x (read-symbol-chars)))
                        (cond
                          ((null? x)
                            (read-char))

                          ((eq? (length x) 1)
                            (car x))

                          (else
                            (cdr (assoc (list->string x) special-chars))))))))

                ((#\u)
                  (read-char)
                  (read-char)
                  (list->bytevector (read-list)))

                (else
                  (list->vector (read-list)))))

            ((eqv? char #\')
              (read-char)
              (list 'quote (read-raw)))

            ((eqv? char #\`)
              (read-char)
              (list 'quasiquote (read-raw)))

            ((eqv? char #\,)
              (read-char)
              (if (eqv? (peek-char) #\@)
                (begin
                  (read-char)
                  (list 'unquote-splicing (read-raw)))
                (list 'unquote (read-raw))))

            ((eqv? char #\")
              (read-string))

            (else
              (let ((x (list->string (read-symbol-chars))))
                (or (string->number x) (string->symbol x)))))))

      (define (read-list)
        (define (read-tail)
          (let ((char (peek-non-whitespace-char)))
            (cond
              ((eof-object? char)
                (error ") expected"))

              ((eqv? char #\))
                (read-char)
                '())

              (else
                (let ((x (read-raw)))
                  (if (and (symbol? x) (equal? (symbol->string x) "."))
                    (let ((x (read-raw)))
                      (read-char)
                      x)
                    (cons x (read-tail))))))))

        (unless (eqv? (read-char) #\()
          (error "( expected"))
        (read-tail))

      (define (read-symbol-chars)
        (let ((char (peek-char)))
          (if (boolean-or
               (memv char '(#\( #\)))
               (eof-object? char)
               (char-whitespace? char))
            '()
            (cons (read-char) (read-symbol-chars)))))

      (define (read-string)
        (unless (eqv? (read-char) #\")
          (error "opening \" expected"))
        (let loop ((xs '()))
          (let ((char (read-char)))
            (cond
              ((eof-object? char)
                (error "closing \" expected"))

              ((eqv? char #\")
                (list->string (reverse xs)))

              ((eqv? char #\\)
                (let ((char (read-char)))
                  (loop
                    (cons
                      (case char
                        ((#\n)
                          #\newline)

                        ((#\r)
                          #\return)

                        ((#\t)
                          #\tab)

                        (else
                          char))
                      xs))))

              (else
                (loop (cons char xs)))))))

      (define (peek-non-whitespace-char)
        (let ((char (peek-char)))
          (cond
            ((char-whitespace? char)
              (begin
                (read-char)
                (peek-non-whitespace-char)))

            ((eqv? char #\;)
              (skip-comment))

            (else
              char))))

      (define (skip-comment)
        (let ((char (read-char)))
          (cond
            ((eof-object? char)
              char)

            ((eqv? char #\newline)
              (peek-non-whitespace-char))

            (else
              (skip-comment)))))

      (parameterize ((current-input-port
                       (if (null? rest) (current-input-port) (car rest))))
        (read-raw)))))

(define-library (scheme write)
  (export
    display
    write
    write-shared
    write-simple)

  (import (scheme base) (scheme char) (srfi 1))

  (begin
    (define (get-output-port rest)
      (if (null? rest) (current-output-port) (car rest)))

    (define-record-type write-context
      (make-write-context display indices referenced)
      write-context?
      (display write-context-display)
      (indices write-context-indices)
      (referenced write-context-referenced write-context-set-referenced!))

    (define (write-context-share-index context x)
      (cond
        ((assq x (write-context-indices context)) =>
          cdr)
        (else
          #f)))

    (define (write-context-reference! context x)
      (write-context-set-referenced!
        context
        (cons x (write-context-referenced context))))

    (define (write-value context x)
      (define escaped-chars
        '((#\newline . #\n)
          (#\tab . #\t)
          (#\return . #\r)
          (#\" . #\")
          (#\\ . #\\)))

      (define special-char-names
        (map
          (lambda (pair) (cons (cdr pair) (car pair)))
          special-chars))

      (define (write-escaped-char x)
        (let ((pair (assoc x escaped-chars)))
          (if pair
            (begin
              (write-char #\\)
              (write-char (cdr pair)))
            (write-char x))))

      (cond
        ((not x)
          (write-string "#f"))

        ((eq? x #t)
          (write-string "#t"))

        ((bytevector? x)
          (write-string "#u8")
          (write-sequence context (bytevector->list x)))

        ((char? x)
          (if (write-context-display context)
            (write-char x)
            (begin
              (write-char #\#)
              (write-char #\\)
              (let ((pair (assoc x special-char-names)))
                (if pair
                  (write-string (cdr pair))
                  (write-char x))))))

        ((null? x)
          (write-string "()"))

        ((number? x)
          (write-string (number->string x)))

        ((pair? x)
          (write-reference context write-list x))

        ((procedure? x)
          (write-string "#procedure"))

        ((record? x)
          (write-string "#record"))

        ((string? x)
          (if (write-context-display context)
            (write-string x)
            (begin
              (write-char #\")
              (for-each write-escaped-char (string->list x))
              (write-char #\"))))

        ((symbol? x)
          (let ((string (symbol->string x)))
            (write-string (if (zero? (string-length string)) "||" string))))

        ((vector? x)
          (write-reference context write-vector x))

        (else
          (error "unknown type to write"))))

    (define (write-reference context f x)
      (cond
        ((write-context-share-index context x) =>
          (lambda (index)
            (write-char #\#)
            (write-string (number->string index))
            (if (memq x (write-context-referenced context))
              (write-char #\#)
              (begin
                (write-context-reference! context x)
                (write-char #\=)
                (f context x)))))
        (else
          (f context x))))

    (define (write-list context xs)
      (define quotes
        '((quote . #\')
          (quasiquote . #\`)
          (unquote . #\,)))

      (define (write-quote char x)
        (write-char char)
        (write-value context x))

      (if (or (null? xs) (null? (cdr xs)))
        (write-sequence context xs)
        (cond
          ((and
              (pair? (cdr xs))
              (null? (cddr xs))
              (assq (car xs) quotes))
            =>
            (lambda (pair)
              (write-quote (cdr pair) (cadr xs))))
          (else
            (write-sequence context xs)))))

    (define (write-sequence context xs)
      (write-char #\()

      (when (pair? xs)
        (write-value context (car xs))
        (let loop ((xs (cdr xs)))
          (cond
            ((null? xs)
              #f)
            ((and (pair? xs) (not (write-context-share-index context xs)))
              (write-char #\space)
              (write-value context (car xs))
              (loop (cdr xs)))
            (else
              (write-char #\space)
              (write-char #\.)
              (write-char #\space)
              (write-value context xs)))))

      (write-char #\)))

    (define (write-vector context xs)
      (write-char #\#)
      (write-sequence context (vector->list xs)))

    (define (collect-cycles f)
      (lambda (x)
        (define (collect x xs)
          (cond
            ((memq x (car xs))
              (list x))
            ((pair? x)
              (let ((xs (f x xs)))
                (delete-duplicates
                  (append
                    (collect (car x) xs)
                    (collect (cdr x) xs)))))
            ((vector? x)
              (let ((xs (f x xs)))
                (delete-duplicates
                  (append-map
                    (lambda (x)
                      (collect x xs))
                    (vector->list x)))))
            (else
              '())))

        (collect x (list '()))))

    (define collect-recursive
      (collect-cycles
        (lambda (x xs)
          (list (cons x (car xs))))))

    (define (write-root display collect-cycles)
      (lambda (x . rest)
        (parameterize ((current-output-port (get-output-port rest)))
          (write-value
            (make-write-context
              display
              (let ((xs (collect-cycles x)))
                (map cons xs (iota (length xs))))
              '())
            x))))

    (define write (write-root #f collect-recursive))

    (define write-shared
      (write-root
        #f
        (collect-cycles
          (lambda (x xs)
            (set-car! xs (delete-duplicates (cons x (car xs))))
            xs))))

    (define write-simple (write-root #f (lambda (x) '())))

    (define display (write-root #t collect-recursive))

    (set! write-irritant write)))

(define-library (scheme lazy)
  (export delay delay-force force promise? make-promise)

  (import (scheme base))

  (begin
    (define-syntax delay
      (syntax-rules ()
        ((_ body)
          (let ((done #f)
                (value #f))
            (lambda ()
              (if done
                value
                (begin
                  (set! value body)
                  (set! done #t)
                  value)))))))

    (define-syntax delay-force
      (syntax-rules ()
        ((_ promise)
          (lambda () (force promise)))))

    (define (force x)
      (x))

    (define promise? procedure?)

    (define (make-promise x)
      (if (promise? x)
        x
        (lambda () x)))))

(define-library (scheme process-context)
  (export
    command-line
    emergency-exit
    exit
    get-environment-variable
    get-environment-variables)

  (import
    (scheme base)
    (scheme lazy)
    (only (stak base) data-rib code-points->string primitive procedure-type))

  (begin
    (define $halt (primitive 40))
    (define $command-line (primitive 300))
    (define $get-environment-variables (primitive 301))

    (define command-line (delay (map code-points->string ($command-line))))
    (define get-environment-variables
      (delay
        (map
          (lambda (pair)
            (cons
              (code-points->string (car pair))
              (code-points->string (cdr pair))))
          ($get-environment-variables))))

    (define (get-environment-variable name)
      (cond
        ((assoc name (get-environment-variables)) =>
          cdr)

        (else
          #f)))

    (define (emergency-exit . rest)
      (if (or (null? rest) (eq? (car rest) #t))
        (begin
          (set-car! (car (cddr (close (lambda () #f)))) '(0))
          ((lambda () #f)))
        ($halt)))

    (define (exit . rest)
      (unwind (lambda () (apply emergency-exit rest))))))

(define-library (scheme file)
  (export
    call-with-input-file
    call-with-output-file
    delete-file
    file-exists?
    open-binary-input-file
    open-binary-output-file
    open-input-file
    open-output-file
    with-input-from-file
    with-output-to-file)

  (import
    (scheme base)
    (only (stak base) primitive string->code-points))

  (begin
    (define $open-file (primitive 200))
    (define $close-file (primitive 201))
    (define $read-file (primitive 202))
    (define $write-file (primitive 203))
    (define $delete-file (primitive 204))
    (define $exists-file (primitive 205))
    (define $flush-file (primitive 206))

    (define (call-with-input-file path f)
      (call-with-port (open-input-file path) f))

    (define (call-with-output-file path f)
      (call-with-port (open-output-file path) f))

    (define (delete-file path)
      ($delete-file (string->code-points path)))

    (define (file-exists? path)
      ($exists-file (string->code-points path)))

    (define (open-file output)
      (lambda (path)
        (let ((descriptor ($open-file (string->code-points path) output)))
          (make-port
            (lambda () ($read-file descriptor))
            (lambda (byte) ($write-file descriptor byte))
            (lambda () ($flush-file descriptor))
            (lambda () ($close-file descriptor))
            '()))))

    (define open-input-file (open-file #f))
    (define open-output-file (open-file #t))
    (define open-binary-input-file open-input-file)
    (define open-binary-output-file open-output-file)

    (define (with-port-from-file open-file current-port)
      (lambda (path thunk)
        (let ((file #f))
          (dynamic-wind
            (lambda () (set! file (open-file path)))
            (lambda () (parameterize ((current-port file)) (thunk)))
            (lambda () (close-port file))))))

    (define with-input-from-file (with-port-from-file open-input-file current-input-port))
    (define with-output-to-file (with-port-from-file open-output-file current-output-port))))

(define-library (scheme time)
  (export
    current-jiffy
    current-second
    jiffies-per-second)

  (import (stak base))

  (begin
    (define current-jiffy (primitive 400))
    (define jiffies-per-second (primitive 401))

    (define (current-second)
      (/ (current-jiffy) (jiffies-per-second)))))

($$compiler)

(define-library (scheme eval)
  (export environment eval make-environment)

  (import (scheme base) (stak compile))

  (begin
    (define-record-type environment
      (make-environment symbol-table imports)
      environment?
      (symbol-table environment-symbol-table)
      (imports environment-imports environment-set-imports!))

    (define (environment . imports)
      (make-environment (make-symbol-table '()) imports))

    (define (eval expression environment)
      (let-values (((thunk imports)
                     (compile
                       (environment-imports environment)
                       (environment-symbol-table environment)
                       expression)))
        (environment-set-imports! environment imports)
        (thunk)))))

(define-library (scheme repl)
  (export interaction-environment)

  (import (scheme base) (scheme eval))

  (begin
    (define interaction-environment
      (let ((environment (make-environment (make-symbol-table '()) '())))
        (lambda () environment)))))

(define-library (scheme load)
  (export load)

  (import
    (scheme base)
    (scheme eval)
    (scheme file)
    (scheme read)
    (scheme repl))

  (begin
    (define (load path . rest)
      (eval
        (cons
          'begin
          (with-input-from-file path
            (lambda ()
              (let loop ()
                (let ((value (read)))
                  (if (eof-object? value)
                    '()
                    (cons value (loop))))))))
        (if (null? rest)
          (interaction-environment)
          (car rest))))))

(define-library (scheme r5rs)
  (import
    (scheme base)
    (scheme char)
    (scheme complex)
    (scheme cxr)
    (scheme eval)
    (scheme file)
    (scheme inexact)
    (scheme lazy)
    (scheme read)
    (scheme repl)
    (scheme write))

  (export
    *
    +
    -
    ...
    /
    <
    <=
    =
    =>
    >
    >=
    abs
    acos
    and
    angle
    append
    apply
    asin
    assoc
    assq
    assv
    atan
    begin
    boolean?
    caaaar
    caaadr
    caaar
    caadar
    caaddr
    caadr
    caar
    cadaar
    cadadr
    cadar
    caddar
    cadddr
    caddr
    cadr
    call-with-current-continuation
    call-with-input-file
    call-with-output-file
    call-with-values
    car
    case
    cdaaar
    cdaadr
    cdaar
    cdadar
    cdaddr
    cdadr
    cdar
    cddaar
    cddadr
    cddar
    cdddar
    cddddr
    cdddr
    cddr
    cdr
    ceiling
    char->integer
    char-alphabetic?
    char-ci<=?
    char-ci<?
    char-ci=?
    char-ci>=?
    char-ci>?
    char-downcase
    char-lower-case?
    char-numeric?
    char-ready?
    char-upcase
    char-upper-case?
    char-whitespace?
    char<=?
    char<?
    char=?
    char>=?
    char>?
    char?
    close-input-port
    close-output-port
    complex?
    cond
    cons
    cos
    current-input-port
    current-output-port
    define
    define-syntax
    delay
    denominator
    display
    do
    dynamic-wind
    else
    eof-object?
    eq?
    equal?
    eqv?
    eval
    even?
    exact->inexact
    exact?
    exp
    expt
    floor
    for-each
    force
    gcd
    if
    imag-part
    inexact->exact
    inexact?
    input-port?
    integer->char
    integer?
    interaction-environment
    lambda
    lcm
    length
    let
    let*
    let-syntax
    letrec
    letrec-syntax
    list
    list->string
    list->vector
    list-ref
    list-tail
    list?
    load
    log
    magnitude
    make-polar
    make-rectangular
    make-string
    make-vector
    map
    max
    member
    memq
    memv
    min
    modulo
    negative?
    newline
    not
    null-environment
    null?
    number->string
    number?
    numerator
    odd?
    open-input-file
    open-output-file
    or
    output-port?
    pair?
    peek-char
    positive?
    procedure?
    quasiquote
    quote
    quotient
    rational?
    rationalize
    read
    read-char
    real-part
    real?
    remainder
    reverse
    round
    scheme-report-environment
    set!
    set-car!
    set-cdr!
    sin
    sqrt
    string
    string->list
    string->number
    string->symbol
    string-append
    string-ci<=?
    string-ci<?
    string-ci=?
    string-ci>=?
    string-ci>?
    string-copy
    string-fill!
    string-length
    string-ref
    string-set!
    string<=?
    string<?
    string=?
    string>=?
    string>?
    string?
    substring
    symbol->string
    symbol?
    syntax-rules
    tan
    truncate
    values
    vector
    vector->list
    vector-fill!
    vector-length
    vector-ref
    vector-set!
    vector?
    with-input-from-file
    with-output-to-file
    write
    write-char
    zero?)

  (begin
    (define (scheme-report-environment version)
      (unless (= version 5)
        (error "unsupported version for scheme report environment" version))
      (environment '(scheme r5rs)))))

(define-library (stak rust)
  (import (stak base) (scheme base))

  (begin
    (do ((names ((primitive 1000)) (cdr names))
         (index 1 (+ index 1)))
      ((null? names))
      (let ((name (car names)))
        (set-car!
          (car
            (member
              (data-rib string-type (length name) name)
              ($$dynamic-symbols)
              (lambda (x y) (equal? x (symbol->string y)))))
          (primitive (+ 1000 index)))))))

; TODO Implement this as SRFI-146.
(define-library (stak aa-tree)
  (export
    aa-tree-empty
    aa-tree?
    aa-tree-find
    aa-tree-insert!
    aa-tree->list
    list->aa-tree)

  (import (stak base))

  (begin
    (define-record-type aa-tree
      (make-aa-tree root less)
      aa-tree?
      (root aa-tree-root aa-tree-set-root!)
      (less aa-tree-less aa-tree-set-less!))

    (define-record-type aa-node
      (make-aa-node value level left right)
      aa-node?
      (value aa-node-value aa-node-set-value!)
      (level aa-node-level aa-node-set-level!)
      (left aa-node-left aa-node-set-left!)
      (right aa-node-right aa-node-set-right!))

    (define (aa-tree-empty less)
      (make-aa-tree #f less))

    (define (aa-tree-find tree value)
      (aa-node-find (aa-tree-root tree) value (aa-tree-less tree)))

    (define (aa-node-find node value less?)
      (and
        node
        (let ((node-value (aa-node-value node)))
          (cond
            ((less? value node-value)
              (aa-node-find (aa-node-left node) value less?))

            ((less? node-value value)
              (aa-node-find (aa-node-right node) value less?))

            (else
              node-value)))))

    (define (aa-tree-insert! tree value)
      (aa-tree-set-root!
        tree
        (aa-node-insert!
          (aa-tree-root tree)
          value
          (aa-tree-less tree))))

    (define (list->aa-tree xs less?)
      (define tree (aa-tree-empty less?))
      (for-each (lambda (x) (aa-tree-insert! tree x)) xs)
      tree)

    (define (aa-tree->list tree)
      (aa-node->list (aa-tree-root tree) '()))

    (define (aa-node->list node xs)
      (if node
        (aa-node->list
          (aa-node-left node)
          (cons
            (aa-node-value node)
            (aa-node->list (aa-node-right node) xs)))
        xs))

    (define (aa-node-insert! node value less?)
      (if node
        (let ((node-value (aa-node-value node)))
          (cond
            ((less? value node-value)
              (aa-node-set-left!
                node
                (aa-node-insert! (aa-node-left node) value less?))
              (aa-node-balance! node))

            ((less? node-value value)
              (aa-node-set-right!
                node
                (aa-node-insert! (aa-node-right node) value less?))
              (aa-node-balance! node))

            (else
              node)))
        (make-aa-node value 0 #f #f)))

    (define (aa-node-balance! node)
      (aa-node-split! (aa-node-skew! node)))

    (define (aa-node-skew! node)
      (let ((left (and node (aa-node-left node))))
        (if (and
             left
             (= (aa-node-level node) (aa-node-level left)))
          (begin
            (aa-node-set-left! node (aa-node-right left))
            (aa-node-set-right! left node)
            left)
          node)))

    (define (aa-node-split! node)
      (let* ((right (and node (aa-node-right node)))
             (right-right (and right (aa-node-right right))))
        (if (and
             right-right
             (= (aa-node-level node) (aa-node-level right-right)))
          (begin
            (aa-node-set-right! node (aa-node-left right))
            (aa-node-set-left! right node)
            (aa-node-set-level! right (+ (aa-node-level right) 1))
            right)
          node)))))
