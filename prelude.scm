; Libraries

(define-library (stak base)
  (export
    syntax-rules
    define-syntax
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
    cond-expand
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

    integer?
    rational?
    real?
    complex?
    number?
    exact?
    inexact?
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
    truncate-remainder
    truncate-quotient
    modulo
    floor-remainder
    truncate
    floor
    ceiling
    round
    exact
    inexact
    abs
    exp
    expt
    log
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
    filter
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
    fold-left
    fold-right
    reduce-right
    memq-position
    memv-position
    member-position
    list-copy

    bytevector?
    bytevector-length
    bytevector-u8-ref
    list->bytevector
    bytevector->list

    vector?
    vector
    make-vector
    vector-length
    vector-ref
    vector-set!
    list->vector
    vector->list

    string?
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
    substring
    make-string
    string=?
    string<?
    string>?

    symbol?
    symbol->string
    string->uninterned-symbol

    define-record-type
    record?

    values
    call-with-values)

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

    (define-syntax cond-expand
      (syntax-rules (and or not else r7rs library scheme base stak)
        ((_ (else body ...))
          (relaxed-begin body ...))

        ((_ ((and) body ...) clause ...)
          (relaxed-begin body ...))

        ((_ ((and requirement1 requirement2 ...) body ...) clause ...)
          (cond-expand
            (requirement1
              (cond-expand
                ((and requirement2 ...) body ...)
                clause
                ...))
            clause
            ...))

        ((_ ((or) body ...) clause ...)
          (cond-expand clause ...))

        ((_ ((or requirement1 requirement2 ...) body ...) clause ...)
          (cond-expand
            (requirement1 body ...)
            ((or requirement2 ...) body ...)
            clause
            ...))

        ((_ ((not requirement) body ...) clause ...)
          (cond-expand
            (requirement
              (cond-expand
                clause
                ...))
            (else body ...)))

        ((_ ((library (scheme base)) body ...) clause ...)
          (relaxed-begin body ...))

        ((_ ((library (name ...)) body ...) clause ...)
          (cond-expand clause ...))

        ((_ (r7rs body ...) clause ...)
          (relaxed-begin body ...))

        ((_ (stak body ...) clause ...)
          (relaxed-begin body ...))

        ((_ (feature body ...) clause ...)
          (cond-expand clause ...))))

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
    (define exp (primitive 15))
    (define $log (primitive 16))
    (define null? (primitive 50))
    (define pair? (primitive 51))
    (define assq (primitive 60))
    (define cons (primitive 61))
    (define memq (primitive 62))
    (define eqv? (primitive 70))
    (define equal-inner? (primitive 71))

    (define (data-rib type car cdr)
      (rib car cdr type))

    (define (apply f x . xs)
      ($$apply
        f
        (let loop ((x x) (xs xs))
          (if (null? xs)
            x
            (cons x (loop (car xs) (cdr xs)))))))

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
      (lambda xs (fold-left f y xs)))

    (define (inverse-arithmetic-operator f y)
      (lambda (x . xs)
        (if (null? xs)
          (f y x)
          (fold-left f x xs))))

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

    (define (quotient x y)
      (/ (- x (remainder x y)) y))

    (define truncate-remainder remainder)
    (define truncate-quotient quotient)

    (define (modulo x y)
      (let ((r (remainder x y)))
        (if (or (zero? r) (eq? (negative? x) (negative? y)))
          r
          (+ r y))))

    (define floor-remainder modulo)

    (define (truncate x)
      (quotient x 1))

    (define (floor x)
      (let ((y (quotient x 1)))
        (if (negative? (remainder x 1))
          (- y 1)
          y)))

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
        (fold-left (lambda (x y) (if (f x y) x y)) x xs)))
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

    (define (filter f xs)
      (if (null? xs)
        '()
        (let ((x (car xs))
              (xs (filter f (cdr xs))))
          (if (f x)
            (cons x xs)
            xs))))

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

    (define (fold-left f y xs)
      (if (null? xs)
        y
        (fold-left
          f
          (f y (car xs))
          (cdr xs))))

    (define (fold-right f y xs)
      (if (null? xs)
        y
        (f (fold-right f y (cdr xs)) (car xs))))

    (define (reduce-right f y xs)
      (if (null? xs)
        y
        (let loop ((xs xs))
          (if (null? (cdr xs))
            (car xs)
            (f (loop (cdr xs)) (car xs))))))

    (define (member-position x xs . rest)
      (define eq?
        (if (null? rest)
          equal?
          (car rest)))

      (let loop ((xs xs) (index 0))
        (cond
          ((null? xs)
            #f)

          ((eq? x (car xs))
            index)

          (else
            (loop (cdr xs) (+ index 1))))))

    (define (memq-position x xs)
      (member-position x xs eq?))

    (define (memv-position x xs)
      (member-position x xs eqv?))

    (define (list-copy xs . rest)
      (define start (if (null? rest) 0 (car rest)))
      (define end (if (or (null? rest) (null? (cdr rest))) #f (cadr rest)))

      (let ((xs (list-tail xs start)))
        (if end
          (list-head xs (- end start))
          xs)))

    ;; Bytevector

    (define bytevector? (instance? bytevector-type))

    (define bytevector-length car)

    (define bytevector->list cdr)

    (define (list->bytevector x)
      (data-rib bytevector-type (length x) x))

    (define (bytevector-u8-ref vector index)
      (list-ref (bytevector->list vector) index))

    ;; Vector

    (define vector? (instance? vector-type))

    (define (vector . rest)
      (list->vector rest))

    (define (make-vector length . rest)
      (list->vector (apply make-list (cons length rest))))

    (define vector-length car)

    (define vector->list cdr)

    (define (vector-ref vector index)
      (list-ref (vector->list vector) index))

    (define (vector-set! vector index value)
      (list-set! (vector->list vector) index value))

    (define (list->vector x)
      (data-rib vector-type (length x) x))

    ;; String

    (define string? (instance? string-type))

    (define (string-rib codes length)
      (data-rib string-type length codes))

    (define (code-points->string x)
      (string-rib x (length x)))

    (define string-length car)

    (define string->code-points cdr)

    (define (list->string x)
      (string-rib (map char->integer x) (length x)))

    (define (string->list x)
      (map integer->char (string->code-points x)))

    (define (string-ref x index)
      (integer->char (list-ref (string->code-points x) index)))

    (define (string-append . xs)
      (code-points->string (apply append (map string->code-points xs))))

    (define (string-copy x . rest)
      (code-points->string (apply list-copy (cons (string->code-points x) rest))))

    (define substring string-copy)

    (define (make-string length . rest)
      (code-points->string
        (make-list
          length
          (if (null? rest) 0 (char->integer (car rest))))))

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
          (format-point (remainder (abs x) 1)))))

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
            ; TODO Fix performance.
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

    (define symbol->string cdr)

    (define (string->uninterned-symbol x)
      (data-rib symbol-type #f x))

    ;; Record

    ; We use record types only for certain built-in types not to degrade space
    ; efficiency of their values.
    (define-syntax define-record-type
      (syntax-rules ()
        ((_ id
            (constructor field ...)
            predicate
            (field getter . rest)
            ...)
          (begin
            (define id (cons 'id '(field ...)))
            (define constructor (record-constructor id))
            (define predicate (record-predicate id))

            (define-record-field id field getter . rest)
            ...))))

    (define-syntax define-record-field
      (syntax-rules ()
        ((_ type field getter)
          (define getter (record-getter type 'field)))

        ((_ type field getter setter)
          (begin
            (define-record-field type field getter)
            (define setter (record-setter type 'field))))))

    (define record? (instance? record-type))

    (define (record-constructor type)
      (lambda xs
        (data-rib record-type type xs)))

    (define (record-predicate type)
      (lambda (x)
        (and
          (record? x)
          (eq? (car x) type))))

    (define (record-getter type field)
      (let ((index (field-index type field)))
        (lambda (record)
          (list-ref (cdr record) index))))

    (define (record-setter type field)
      (let ((index (field-index type field)))
        (lambda (record value)
          (list-set! (cdr record) index value))))

    (define (field-index type field)
      (memq-position field (cdr type)))

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
      (make-tuple xs))

    (define (call-with-values producer consumer)
      (let ((xs (producer)))
        (if (tuple? xs)
          (apply consumer (tuple-values xs))
          (consumer xs))))))

(define-library (scheme base)
  (export
    syntax-rules
    define-syntax
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
    cond-expand
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

    integer?
    rational?
    real?
    complex?
    number?
    exact?
    inexact?
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
    truncate-remainder
    truncate-quotient
    modulo
    floor-remainder
    truncate
    floor
    ceiling
    round
    exact
    inexact
    abs
    expt
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
    fold-left
    fold-right
    reduce-right
    list-copy

    bytevector?
    bytevector-length
    bytevector-u8-ref
    list->bytevector
    bytevector->list

    vector?
    vector
    make-vector
    vector-length
    vector-ref
    vector-set!
    list->vector
    vector->list

    string?
    list->string
    string->list
    string-append
    string-length
    string-ref
    number->string
    string->number
    string-copy
    substring
    make-string
    string=?
    string<?
    string>?

    symbol?
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

    make-point
    point?
    point-depth
    point-before
    point-after
    point-parent
    current-point
    set-current-point!

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

    read-u8
    peek-u8
    read-char
    peek-char

    write-u8
    write-char
    write-string
    write-bytevector
    newline

    write-value)

  (import (stak base))

  (begin
    (define $halt (primitive 40))
    (define $read-input (primitive 100))
    (define $write-output (primitive 101))
    (define $write-error (primitive 102))

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
                name))))))

    ; Control

    ;; Continuation

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

    ;; Dynamic wind

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

    ;; Parameter

    (define (make-parameter x . rest)
      (define convert (if (pair? rest) (car rest) (lambda (x) x)))
      (set! x (convert x))

      (lambda rest
        (if (null? rest)
          x
          (set! x (convert (car rest))))))

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
              (lambda () (parameter old)))))))

    ;; Exception

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
                          (write-value value))
                        (error-object-irritants exception)))
                    (write-value exception))
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

    (define error (error-type #f))
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

    ;; Unwind

    (define unwind #f)

    ((call/cc
        (lambda (continuation)
          (set! unwind continuation)
          (lambda () #f))))

    ; Derived types

    ;; EOF object

    (define-record-type eof-object
      (make-eof-object)
      eof-object?)

    (define eof-object
      (let ((eof (make-eof-object)))
        (lambda () eof)))

    ;; Port

    ; TODO Support multiple bytes.
    (define-record-type port
      (make-port* read write close last-byte)
      port?
      (read port-read)
      (write port-write)
      (close port-close)
      (last-byte port-last-byte port-set-last-byte!))

    (define input-port? port-read)
    (define output-port? port-write)
    (define textual-port? port?)
    (define binary-port? port?)

    (define (make-port read write close)
      (make-port* read write close #f))

    (define (make-input-port read close)
      (make-port read #f close))

    (define (make-output-port write close)
      (make-port #f write close))

    (define current-input-port (make-parameter (make-input-port $read-input #f)))
    (define current-output-port (make-parameter (make-output-port $write-output #f)))
    (define current-error-port (make-parameter (make-output-port $write-error #f)))

    ; Close

    (define (close-port port)
      (let ((close (port-close port)))
        (unless close
          (error "cannot close port"))
        (close)))

    (define close-input-port close-port)
    (define close-output-port close-port)

    (define (call-with-port port f)
      (let ((x (f port)))
        (close-port port)
        x))

    ; Read

    (define (get-input-port rest)
      (if (null? rest) (current-input-port) (car rest)))

    (define (input-byte->char x)
      (if (number? x) (integer->char x) x))

    (define (read-u8 . rest)
      (let* ((port (get-input-port rest))
             (x (port-last-byte port)))
        (if x
          (begin
            (port-set-last-byte! port #f)
            x)
          (let ((read (port-read port)))
            (unless read
              (error "cannot read from port"))
            (or (read) (eof-object))))))

    (define (peek-u8 . rest)
      (let* ((port (get-input-port rest))
             (x (read-u8 port)))
        (port-set-last-byte! port x)
        x))

    (define (read-char . rest)
      (input-byte->char (read-u8 (get-input-port rest))))

    (define (peek-char . rest)
      (input-byte->char (peek-u8 (get-input-port rest))))

    ; Write

    (define (get-output-port rest)
      (if (null? rest) (current-output-port) (car rest)))

    (define (write-u8 byte . rest)
      (let ((write (port-write (get-output-port rest))))
        (unless write
          (error "cannot write to port"))
        (write byte)))

    (define sub-byte 64)

    (define (write-sub-bytes integer port)
      (let ((upper (quotient integer sub-byte)))
        (unless (zero? upper)
          (write-sub-bytes upper port))
        (write-u8 (+ 128 (remainder integer sub-byte)) port)))

    (define (write-char x . rest)
      (let ((port (get-output-port rest))
            (integer (char->integer x)))
        (cond
          ((zero? (quotient integer 128))
            (write-u8 integer port))
          ((zero? (quotient integer (* 32 sub-byte)))
            (write-u8 (+ 192 (quotient integer sub-byte)) port)
            (write-sub-bytes (remainder integer sub-byte) port))
          ((zero? (quotient integer (* 16 sub-byte sub-byte)))
            (let ((byte (quotient integer (* sub-byte sub-byte))))
              (write-u8 (+ 224 byte) port)
              (write-sub-bytes (- integer (* byte sub-byte sub-byte)) port)))
          (else
            (let ((byte (quotient integer (* sub-byte sub-byte sub-byte))))
              (write-u8 (+ 240 byte) port)
              (write-sub-bytes (- integer (* byte sub-byte sub-byte sub-byte))))))))

    (define (write-string x . rest)
      (parameterize ((current-output-port (get-output-port rest)))
        (for-each write-char (string->list x))))

    (define (write-bytevector xs . rest)
      (parameterize ((current-output-port (get-output-port rest)))
        (do ((index 0 (+ index 1)))
          ((= index (bytevector-length xs))
            #f)
          (write-u8 (bytevector-u8-ref xs index)))))

    (define (newline . rest)
      (write-char #\newline (get-output-port rest)))

    ; Dummy implementation
    (define (write-value value . rest)
      (write-string "<unknown>" (get-output-port rest)))))

(define-library (scheme inexact)
  (export exp log)

  (import (only (stak base) exp log)))

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

  (import (scheme base))

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
  (export char-whitespace? special-chars)

  (import (scheme base))

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

    (define (char-whitespace? x)
      (memv x '(#\newline #\return #\space #\tab)))))

(define-library (scheme read)
  (export read)

  (import (scheme base) (scheme char) (only (stak base) boolean-or))

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
  (export display write)

  (import (scheme base) (scheme char))

  (begin
    (define (get-output-port rest)
      (if (null? rest) (current-output-port) (car rest)))

    (define (write x . rest)
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

      (parameterize ((current-write write)
                     (current-output-port (get-output-port rest)))
        (cond
          ((char? x)
            (write-char #\#)
            (write-char #\\)
            (let ((pair (assoc x special-char-names)))
              (if pair
                (display (cdr pair))
                (write-char x))))

          ((pair? x)
            (write-list x))

          ((string? x)
            (write-char #\")
            (for-each write-escaped-char (string->list x))
            (write-char #\"))

          ((vector? x)
            (write-vector x))

          (else
            (display x)))))

    (define (display x . rest)
      (parameterize ((current-write display)
                     (current-output-port (get-output-port rest)))
        (cond
          ((not x)
            (write-string "#f"))

          ((eq? x #t)
            (write-string "#t"))

          ((bytevector? x)
            (write-string "#u8")
            (write-sequence (bytevector->list x)))

          ((char? x)
            (write-char x))

          ((null? x)
            (write-sequence x))

          ((number? x)
            (display (number->string x)))

          ((pair? x)
            (write-list x))

          ((procedure? x)
            (write-string "#procedure"))

          ((record? x)
            (write-string "#record"))

          ((string? x)
            (write-string x))

          ((symbol? x)
            (let ((string (symbol->string x)))
              (display (if (zero? (string-length string)) "||" string))))

          ((vector? x)
            (write-vector x))

          (else
            (error "unknown type to display")))))

    (define current-write (make-parameter write))

    (define (write-list xs)
      (define quotes
        '((quote . #\')
          (quasiquote . #\`)
          (unquote . #\,)))

      (define (write-quote char value)
        (write-char char)
        ((current-write) value))

      (if (or (null? xs) (null? (cdr xs)))
        (write-sequence xs)
        (cond
          ((and
              (pair? (cdr xs))
              (null? (cddr xs))
              (assq (car xs) quotes))
            =>
            (lambda (pair)
              (write-quote (cdr pair) (cadr xs))))

          (else
            (write-sequence xs)))))

    (define (write-sequence xs)
      (define write (current-write))

      (write-char #\()

      (when (pair? xs)
        (write (car xs))
        (let loop ((xs (cdr xs)))
          (cond
            ((pair? xs)
              (write-char #\space)
              (write (car xs))
              (loop (cdr xs)))

            ((null? xs)
              #f)

            (else
              (write-char #\space)
              (write-char #\.)
              (write-char #\space)
              (write xs)))))

      (write-char #\)))

    (define (write-vector xs)
      (write-char #\#)
      (write-sequence (vector->list xs)))

    (set! write-value write)))

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
            (lambda () ($close-file descriptor))))))

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

    (define (jiffies-per-second)
      1000000000)

    (define (current-second)
      (/ (current-jiffy) (jiffies-per-second)))))

(define-library (scheme eval)
  (export environment eval make-environment)

  (import
    (scheme base)
    (scheme cxr)
    (only (stak base) fold-left rib string->uninterned-symbol))

  (begin
    (define-record-type environment
      (make-environment symbol-table imports)
      environment?
      (symbol-table environment-symbol-table)
      (imports environment-imports environment-set-imports!))

    (define (environment . imports)
      (make-environment (make-symbol-table '()) imports))

    (define eval
      (let ((compile ($$compiler)))
        (lambda (expression environment)
          ((compile expression environment)))))))

(define-library (scheme repl)
  (export interaction-environment)

  (import (scheme base) (scheme eval))

  (begin
    (define interaction-environment
      (let ((environment (make-environment (make-symbol-table '()) '())))
        (lambda () environment)))))

(define-library (scheme r5rs)
  (import
    (scheme base)
    (scheme char)
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
    /
    <
    <=
    =
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
