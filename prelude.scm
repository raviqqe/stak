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
    procedure-type

    primitive
    rib
    cons
    close
    rib?
    rib-car
    rib-cdr
    rib-type
    rib-tag
    rib-set-car!
    rib-set-cdr!
    eq?
    $$<
    $$+
    $$-
    $$*
    $$/
    $$remainder

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
    car
    cdr
    set-car!
    set-cdr!
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
    member-position
    memv-position
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
          ($$syntax-rules ... (literal $$...) (pattern body) $$...))))

    ($$define-syntax define-syntax
      (syntax-rules ()
        ((_ name value)
          ($$define-syntax name value))))

    (define-syntax define
      (syntax-rules ()
        ((_ (name argument ... . rest) body1 body2 ...)
          (define name (lambda (argument ... . rest) body1 body2 ...)))

        ((_ name value)
          ($$define name value))))

    (define-syntax lambda
      (syntax-rules (define define-values define-syntax)
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

        ((_ arguments (define-values names value) body1 body2 ...)
          (lambda arguments (let-values ((names value)) body1 body2 ...)))

        ((_ arguments (define-syntax name value) body1 body2 ...)
          (lambda "syntax" arguments ((name value)) body1 body2 ...))

        ((_ "syntax" arguments ((name value) ...) (define-syntax new-name new-value) body1 body2 ...)
          (lambda "syntax" arguments ((name value) ... (new-name new-value)) body1 body2 ...))

        ((_ "syntax" arguments ((name value) ...) body1 body2 ...)
          (lambda arguments (letrec-syntax ((name value) ...) body1 body2 ...)))

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
      (syntax-rules (define define-syntax)
        ((_ () (define content ...) body1 body2 ...)
          ((lambda () (define content ...) body1 body2 ...)))

        ((_ () (define-syntax content ...) body1 body2 ...)
          ((lambda () (define-syntax content ...) body1 body2 ...)))

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
            body1
            body2
            ...))))

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
      ($$rib procedure-type '() id 0))

    (define rib $$rib)
    (define cons (primitive 1))
    (define close (primitive 2))
    (define rib? (primitive 3))
    (define rib-car (primitive 4))
    (define rib-cdr (primitive 5))
    (define rib-type (primitive 6))
    (define rib-tag (primitive 7))
    (define rib-set-car! (primitive 8))
    (define rib-set-cdr! (primitive 9))
    (define eq? (primitive 10))
    (define $$< (primitive 11))
    (define $$+ (primitive 12))
    (define $$- (primitive 13))
    (define $$* (primitive 14))
    (define $$/ (primitive 15))
    (define $$remainder (primitive 16))
    (define $$exp (primitive 17))
    (define $$log (primitive 18))
    (define $$read-input (primitive 19))
    (define $$write-output (primitive 20))
    (define $$write-error (primitive 21))
    (define $$halt (primitive 22))
    (define null? (primitive 23))
    (define pair? (primitive 24))

    (define (data-rib type car cdr)
      (rib type car cdr 0))

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
          (eq? (rib-type x) type))))

    (define (eqv? x y)
      (boolean-or
        (eq? x y)
        (and
          (char? x)
          (char? y)
          (eq? (char->integer x) (char->integer y)))))

    (define (equal? x y)
      (boolean-or
        (eq? x y)
        (and
          (rib? x)
          (rib? y)
          (eq? (rib-type x) (rib-type y))
          ; Optimize for the cases of strings and vectors where `cdr`s are integers.
          (boolean-or
            (rib? (rib-cdr x))
            (eq? (rib-cdr x) (rib-cdr y)))
          (equal? (rib-car x) (rib-car y))
          (equal? (rib-cdr x) (rib-cdr y)))))

    ;; Procedure

    (define procedure? (instance? procedure-type))

    ;; Boolean

    (define boolean? (instance? boolean-type))

    (define (not x)
      (eq? x #f))

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

    (define (arithmetic-operator f y)
      (lambda xs (fold-left f y xs)))

    (define (inverse-arithmetic-operator f y)
      (lambda (x . xs)
        (if (null? xs)
          (f y x)
          (fold-left f x xs))))

    (define + (arithmetic-operator $$+ 0))
    (define - (inverse-arithmetic-operator $$- 0))
    (define * (arithmetic-operator $$* 1))
    (define / (inverse-arithmetic-operator $$/ 1))

    (define remainder $$remainder)
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

    (define exp $$exp)

    (define (log x . xs)
      (if (null? xs)
        ($$log x)
        (/ ($$log x) ($$log (car xs)))))

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
    (define < (comparison-operator $$<))
    (define > (comparison-operator (lambda (x y) ($$< y x))))
    (define <= (comparison-operator (lambda (x y) (not ($$< y x)))))
    (define >= (comparison-operator (lambda (x y) (not ($$< x y)))))

    ; TODO Set a true machine epsilon.
    ;
    ; Currently, we have a precision limitation due to compression of floating point number in a compiler.
    (define epsilon
      ; Variadic arguments to arithmetic operators are not available at this point.
      (let ((x (/ (/ 1 10000000) 100000000)))
        (if (zero? x) 1 x)))

    ;; Character

    (define char? (instance? char-type))

    (define (integer->char x)
      (data-rib char-type '() x))

    (define char->integer rib-cdr)

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

    (define car rib-car)
    (define cdr rib-cdr)
    (define set-car! rib-set-car!)
    (define set-cdr! rib-set-cdr!)
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

    (define (memq x xs) (member x xs eq?))
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

    (define (assq x xs) (assoc x xs eq?))
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

    (define bytevector-length rib-cdr)

    (define (bytevector-u8-ref vector index)
      (list-ref (rib-car vector) index))

    (define (list->bytevector x)
      (data-rib bytevector-type x (length x)))

    (define bytevector->list rib-car)

    ;; Vector

    (define vector? (instance? vector-type))

    (define (vector . rest)
      (list->vector rest))

    (define (make-vector length . rest)
      (list->vector (apply make-list (cons length rest))))

    (define vector-length rib-cdr)

    (define (vector-ref vector index)
      (list-ref (vector->list vector) index))

    (define (vector-set! vector index value)
      (list-set! (vector->list vector) index value))

    (define (list->vector x)
      (data-rib vector-type x (length x)))

    (define vector->list rib-car)

    ;; String

    (define string? (instance? string-type))

    (define (string-rib codes length)
      (data-rib string-type codes length))

    (define (code-points->string x)
      (string-rib x (length x)))

    (define string->code-points rib-car)

    (define string-length rib-cdr)

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

    (define (format-digit x)
      (integer->char
        (if (< 9 x)
          (+ (char->integer #\a) (- x 10))
          (+ (char->integer #\0) x))))

    (define (format-point x radix)
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

    (define (number->string x . rest)
      (let ((radix (if (null? rest) 10 (car rest))))
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
            (format-point (remainder (abs x) 1) radix)))))

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

    (define (convert-digit x radix)
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

    (define (string->number x . rest)
      (define radix (if (null? rest) 10 (car rest)))

      (define (convert-point xs)
        (let loop ((xs xs) (y 0) (d 1))
          (if (null? xs)
            (/ y d)
            (let ((x (convert-digit (car xs) radix)))
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
                (let ((x (convert-digit (car xs) radix)))
                  (and x (loop #f (cdr xs) (+ (* radix y) x)))))))))

      (let ((xs (string->list x)))
        (if (and (pair? xs) (eqv? (car xs) #\-))
          (let ((x (convert (cdr xs))))
            (and x (- x)))
          (convert xs))))

    ;; Symbol

    (define symbol? (instance? symbol-type))

    (define symbol->string rib-car)

    (define (string->uninterned-symbol x)
      (data-rib symbol-type x #f))

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
        (data-rib record-type xs type)))

    (define (record-predicate type)
      (lambda (x)
        (and
          (record? x)
          (eq? (rib-cdr x) type))))

    (define (record-getter type field)
      (let ((index (field-index type field)))
        (lambda (record)
          (list-ref (rib-car record) index))))

    (define (record-setter type field)
      (let ((index (field-index type field)))
        (lambda (record value)
          (list-set! (rib-car record) index value))))

    (define (field-index type field)
      (memv-position field (cdr type)))

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
          (let-values "multiple" (binding ...) () (begin body1 body2 ...)))

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
    rib-car
    rib-cdr
    rib-type
    rib-tag
    rib-set-car!
    rib-set-cdr!
    eq?
    $$<
    $$+
    $$-
    $$*
    $$/

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
    car
    cdr
    set-car!
    set-cdr!
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
    member-position
    memv-position
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
    string=?
    string<?
    string>?

    symbol?
    symbol->string
    string->uninterned-symbol
    string->symbol

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
    travel-to-point!

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

  (import (shake (stak base)))

  (begin
    ; Symbol table

    (define symbols (rib-car $$rib))
    ; Allow garbage collection for a symbol table.
    (rib-set-car! $$rib #f)

    (define (string->symbol x)
      (cond
        ((member x symbols (lambda (x y) (equal? x (symbol->string y)))) =>
          car)

        (else
          (let ((x (string->uninterned-symbol x)))
            (set! symbols (cons x symbols))
            x))))

    ; Control

    ;; Continuation

    (define dummy-procedure (lambda () #f))

    (define (call/cc receiver)
      (let ((continuation (rib-car (rib-cdr (rib-cdr (rib-car (close dummy-procedure))))))
            (point current-point))
        (receiver
          (lambda (argument)
            (travel-to-point! current-point point)
            (set-current-point! point)
            (rib-set-car!
              (rib-cdr (rib-car (close dummy-procedure))) ; frame
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
            (error "exception handler returned on a non-continuable exception" exception))
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
                  ($$halt))))))))

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

    (define current-input-port (make-parameter (make-input-port $$read-input #f)))
    (define current-output-port (make-parameter (make-output-port $$write-output #f)))
    (define current-error-port (make-parameter (make-output-port $$write-error #f)))

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

    (define (write-char x . rest)
      (write-u8 (char->integer x) (get-output-port rest)))

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

  (import (shake (scheme base)))

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

(define-library (scheme char)
  (export char-whitespace? special-chars)

  (import (shake (scheme base)))

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

  (import (shake (scheme base)) (scheme char) (only (stak base) boolean-or))

  (begin
    (define (get-input-port rest)
      (if (null? rest) (current-input-port) (car rest)))

    (define (read . rest)
      (parameterize ((current-input-port (get-input-port rest)))
        (read-raw)))

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
        (if (eqv? (peek-non-whitespace-char) #\))
          (begin
            (read-char)
            '())
          (let ((x (read-raw)))
            (if (and (symbol? x) (equal? (symbol->string x) "."))
              (let ((x (read-raw)))
                (read-char)
                x)
              (cons x (read-tail))))))

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
        (error "\" expected"))
      (let loop ((xs '()))
        (let ((char (read-char)))
          (cond
            ((eof-object? char)
              (error "unexpected end of input"))

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
            (skip-comment)))))))

(define-library (scheme write)
  (export display write)

  (import (shake (scheme base)) (scheme char))

  (begin
    (define (get-output-port rest)
      (if (null? rest) (current-output-port) (car rest)))

    (define special-char-names
      (map
        (lambda (pair) (cons (cdr pair) (car pair)))
        special-chars))

    (define escaped-chars
      '((#\newline . #\n)
        (#\tab . #\t)
        (#\return . #\r)
        (#\" . #\")
        (#\\ . #\\)))

    (define (write-escaped-char x)
      (let ((pair (assoc x escaped-chars)))
        (if pair
          (begin
            (write-char #\\)
            (write-char (cdr pair)))
          (write-char x))))

    (define (write x . rest)
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
            (display (symbol->string x)))

          ((vector? x)
            (write-vector x))

          (else
            (error "unknown type")))))

    (define current-write (make-parameter write))

    (define quotes
      '((quote . #\')
        (quasiquote . #\`)
        (unquote . #\,)))

    (define (write-list xs)
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

    (define (write-quote char value)
      (write-char char)
      ((current-write) value))

    (define (write-vector xs)
      (write-char #\#)
      (write-sequence (vector->list xs)))

    (set! write-value write)))

(define-library (scheme lazy)
  (export delay delay-force force promise? make-promise)

  (import (shake (scheme base)))

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
    (shake (scheme base))
    (scheme lazy)
    (only (stak base) data-rib code-points->string primitive procedure-type))

  (begin
    (define $$halt (primitive 22))
    (define $$command-line (primitive 31))
    (define $$get-environment-variables (primitive 32))

    (define command-line (delay (map code-points->string ($$command-line))))
    (define get-environment-variables
      (delay
        (map
          (lambda (pair)
            (cons
              (code-points->string (car pair))
              (code-points->string (cdr pair))))
          ($$get-environment-variables))))

    (define (get-environment-variable name)
      (cond
        ((assoc name (get-environment-variables)) =>
          cdr)

        (else
          #f)))

    (define exit-success (data-rib procedure-type '() (cons 0 '())))

    (define (emergency-exit . rest)
      (if (or (null? rest) (eq? (car rest) #t))
        (exit-success)
        ($$halt)))

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
    (shake (scheme base))
    (only (stak base) primitive string->code-points))

  (begin
    (define $$open-file (primitive 25))
    (define $$close-file (primitive 26))
    (define $$read-file (primitive 27))
    (define $$write-file (primitive 28))
    (define $$delete-file (primitive 29))
    (define $$exists-file (primitive 30))

    (define (call-with-input-file path f)
      (call-with-port (open-input-file path) f))

    (define (call-with-output-file path f)
      (call-with-port (open-output-file path) f))

    (define (delete-file path)
      (unless ($$delete-file (string->code-points path))
        (error "cannot delete file")))

    (define (file-exists? path)
      ($$exists-file (string->code-points path)))

    (define (open-file output)
      (lambda (path)
        (let ((descriptor ($$open-file (string->code-points path) output)))
          (unless descriptor
            (error "cannot open file"))
          (make-port
            (lambda () ($$read-file descriptor))
            (lambda (byte) ($$write-file descriptor byte))
            (lambda () ($$close-file descriptor))))))

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

(define-library (scheme repl)
  (export interaction-environment)

  (import (only (scheme base) define make-parameter quote))

  (begin
    (define interaction-environment (make-parameter '()))))

(define-library (scheme eval)
  (export environment eval)

  (import
    (shake (scheme base))
    (shake (scheme cxr))
    (scheme repl)
    (only (stak base) data-rib filter list-head memv-position pair-type procedure-type rib))

  (begin
    ; Utilities

    (define (last-cdr xs)
      (if (pair? xs)
        (last-cdr (cdr xs))
        xs))

    (define (set-last-cdr! xs x)
      (if (pair? (cdr xs))
        (set-last-cdr! (cdr xs) x)
        (set-cdr! xs x)))

    (define (relaxed-length xs)
      (do ((xs xs (cdr xs)) (y 0 (+ y 1)))
        ((not (pair? xs))
          y)))

    (define (relaxed-deep-map f xs)
      (cond
        ((null? xs)
          '())

        ((pair? xs)
          (cons
            (relaxed-deep-map f (car xs))
            (relaxed-deep-map f (cdr xs))))

        (else
          (f xs))))

    (define (map-values f xs)
      (map (lambda (pair) (cons (car pair) (f (cdr pair)))) xs))

    (define (filter-values f xs)
      (filter (lambda (pair) (f (cdr pair))) xs))

    (define (predicate expression)
      (and (pair? expression) (car expression)))

    (define (symbol-append . xs)
      (string->symbol (apply string-append (map symbol->string xs))))

    (define (id->string id)
      (number->string id 32))

    (define library-symbol-separator #\%)

    (define (build-library-symbol id name)
      (string->symbol
        (string-append
          (id->string id)
          (list->string (list library-symbol-separator))
          (symbol->string name))))

    (define (resolve-library-symbol name)
      (let* ((string (symbol->string name))
             (position (memv-position library-symbol-separator (string->list string))))
        (if position
          (string->symbol (string-copy string (+ position 1)))
          name)))

    ; Macro system

    ;; Types

    (define-record-type macro-state
      (make-macro-state id)
      macro-state?
      (id macro-state-id macro-state-set-id!))

    (define-record-type macro-context
      (make-macro-context state environment)
      macro-context?
      (state macro-context-state)
      (environment macro-context-environment macro-context-set-environment!))

    (define (macro-context-append context pairs)
      (make-macro-context
        (macro-context-state context)
        (append pairs (macro-context-environment context))))

    (define (macro-context-set! context name denotation)
      (let* ((environment (macro-context-environment context))
             (pair (assq name environment)))
        (when pair (set-cdr! pair denotation))
        pair))

    (define (macro-context-set-last! context name denotation)
      (unless (macro-context-set! context name denotation)
        (let ((environment (macro-context-environment context))
              (tail (list (cons name denotation))))
          (if (null? environment)
            (macro-context-set-environment! context tail)
            (set-last-cdr! environment tail)))))

    (define (macro-context-generate-id! context)
      (let* ((state (macro-context-state context))
             (id (macro-state-id state)))
        (macro-state-set-id! state (+ id 1))
        id))

    (define-record-type rule-context
      (make-rule-context definition-context use-context ellipsis literals)
      rule-context?
      (definition-context rule-context-definition-context)
      (use-context rule-context-use-context)
      (ellipsis rule-context-ellipsis)
      (literals rule-context-literals))

    ;; Procedures

    (define primitive-procedures
      (map
        (lambda (x)
          (cons
            ; `0` is always the library ID of `(stak base)`.
            (symbol->string (build-library-symbol 0 x))
            (symbol-append '$$ x)))
        '(+ - * / <)))

    (define (resolve-denotation context expression)
      (cond
        ((assq expression (macro-context-environment context)) =>
          cdr)

        (else
          expression)))

    (define (rename-variable context name)
      ; Share tails when appending strings.
      (string->symbol
        (string-append
          (id->string (macro-context-generate-id! context))
          "$"
          (symbol->string name))))

    (define (find-pattern-variables ellipsis bound-variables pattern)
      (define excluded-variables (cons ellipsis bound-variables))

      (let loop ((pattern pattern) (variables '()))
        (cond
          ((pair? pattern)
            (loop
              (car pattern)
              (loop
                (cdr pattern)
                variables)))

          ((and (symbol? pattern) (not (memq pattern excluded-variables)))
            (cons pattern variables))

          (else
            variables))))

    (define-record-type ellipsis-match
      (make-ellipsis-match value)
      ellipsis-match?
      (value ellipsis-match-value))

    (define-record-type ellipsis-pattern
      (make-ellipsis-pattern element variables)
      ellipsis-pattern?
      (element ellipsis-pattern-element)
      (variables ellipsis-pattern-variables))

    (define (compile-pattern context ellipsis literals pattern)
      (define (compile pattern)
        (compile-pattern context ellipsis literals pattern))

      (cond
        ((not (pair? pattern))
          pattern)

        ((and
            (pair? (cdr pattern))
            (eq? ellipsis (resolve-denotation context (cadr pattern))))
          (cons
            (make-ellipsis-pattern
              (compile (car pattern))
              (find-pattern-variables ellipsis literals (car pattern)))
            (compile (cddr pattern))))

        (else
          (cons
            (compile (car pattern))
            (compile (cdr pattern))))))

    (define (match-ellipsis-pattern context pattern expression)
      (map-values
        make-ellipsis-match
        (apply
          map
          list
          (ellipsis-pattern-variables pattern)
          (map
            (lambda (expression)
              (match-pattern context (ellipsis-pattern-element pattern) expression))
            expression))))

    (define (match-pattern context pattern expression)
      (define (match pattern expression)
        (match-pattern context pattern expression))

      (cond
        ((and
            (symbol? pattern)
            (memq pattern (rule-context-literals context)))
          (unless (eq?
                   (resolve-denotation (rule-context-use-context context) expression)
                   (resolve-denotation (rule-context-definition-context context) pattern))
            (raise #f))
          '())

        ((symbol? pattern)
          (list (cons pattern expression)))

        ((pair? pattern)
          (cond
            ((ellipsis-pattern? (car pattern))
              (let ((length (- (relaxed-length expression) (relaxed-length (cdr pattern)))))
                (when (negative? length)
                  (raise #f))
                (append
                  (match-ellipsis-pattern context (car pattern) (list-head expression length))
                  (match (cdr pattern) (list-tail expression length)))))

            ((pair? expression)
              (append
                (match (car pattern) (car expression))
                (match (cdr pattern) (cdr expression))))

            (else
              (raise #f))))

        ((equal? pattern expression)
          '())

        (else
          (raise #f))))

    (define (fill-ellipsis-template context matches template)
      (let* ((variables (ellipsis-pattern-variables template))
             (template (ellipsis-pattern-element template))
             (matches (filter (lambda (pair) (memq (car pair) variables)) matches))
             (singleton-matches (filter-values (lambda (match) (not (ellipsis-match? match))) matches))
             (ellipsis-matches (filter-values ellipsis-match? matches)))
        (when (null? ellipsis-matches)
          (error "no ellipsis pattern variables" template))
        (apply
          map
          (lambda matches (fill-template context (append matches singleton-matches) template))
          (map (lambda (pair) (ellipsis-match-value (cdr pair))) ellipsis-matches))))

    (define (fill-template context matches template)
      (define (fill template)
        (fill-template context matches template))

      (cond
        ((and (symbol? template) (assq template matches)) =>
          cdr)

        ((pair? template)
          (append
            (let ((first (car template)))
              (if (ellipsis-pattern? first)
                (fill-ellipsis-template context matches first)
                (list (fill first))))
            (fill (cdr template))))

        (else
          template)))

    (define (make-transformer definition-context transformer)
      (let-values (((transformer definition-context) (expand-outer-macro definition-context transformer)))
        (case (resolve-denotation definition-context (predicate transformer))
          (($$syntax-rules)
            (let* ((ellipsis (resolve-denotation definition-context (cadr transformer)))
                   (literals (caddr transformer))
                   (rules
                     (map
                       (lambda (rule)
                         (map
                           (lambda (pattern)
                             (compile-pattern definition-context ellipsis literals pattern))
                           rule))
                       (cdddr transformer))))
              (lambda (use-context expression)
                (let loop ((rules rules))
                  (unless (pair? rules)
                    (error "invalid syntax" expression))
                  (let ((rule (car rules))
                        (rule-context (make-rule-context definition-context use-context ellipsis literals)))
                    (guard (value
                            ((not value)
                              (loop (cdr rules))))
                      (let* ((matches (match-pattern rule-context (car rule) expression))
                             (template (cadr rule))
                             (names
                               (map
                                 (lambda (name) (cons name (rename-variable use-context name)))
                                 (find-pattern-variables ellipsis (append literals (map car matches)) template))))
                        (values
                          (fill-template rule-context (append names matches) template)
                          (macro-context-append
                            use-context
                            (map
                              (lambda (pair)
                                (cons
                                  (cdr pair)
                                  (resolve-denotation definition-context (car pair))))
                              names))))))))))

          (else
            (error "unsupported macro transformer" transformer)))))

    (define (expand-outer-macro context expression)
      (if (pair? expression)
        (let ((value (resolve-denotation context (car expression))))
          (if (procedure? value)
            (let-values (((expression context) (value context expression)))
              (expand-outer-macro context expression))
            (values expression context)))
        (values expression context)))

    ; https://www.researchgate.net/publication/220997237_Macros_That_Work
    (define (expand-macro context expression)
      (define (expand expression)
        (expand-macro context expression))

      (define (resolve name)
        (resolve-denotation context name))

      (cond
        ((symbol? expression)
          (let ((value (resolve expression)))
            (when (procedure? value)
              (error "invalid syntax" expression))
            value))

        ((pair? expression)
          (case (resolve (car expression))
            (($$alias)
              (macro-context-set-last!
                context
                (cadr expression)
                (resolve (caddr expression)))
              #f)

            (($$define)
              (let ((name (cadr expression)))
                (macro-context-set! context name name)
                (expand (cons '$$set! (cdr expression)))))

            (($$define-syntax)
              (macro-context-set-last!
                context
                (cadr expression)
                (make-transformer context (caddr expression)))
              #f)

            (($$lambda)
              (let* ((parameters (cadr expression))
                     (context
                       (macro-context-append
                         context
                         (map
                           (lambda (name) (cons name (rename-variable context name)))
                           (parameter-names parameters))))
                     ; We need to resolve parameter denotations before expanding a body.
                     (parameters
                       (relaxed-deep-map
                         (lambda (name) (resolve-denotation context name))
                         parameters)))
                (list
                  '$$lambda
                  parameters
                  (expand-macro context (caddr expression)))))

            (($$let-syntax)
              (expand-macro
                (macro-context-append
                  context
                  (map-values
                    (lambda (transformer)
                      (make-transformer context (car transformer)))
                    (cadr expression)))
                (caddr expression)))

            (($$letrec-syntax)
              (let* ((bindings (cadr expression))
                     (context
                       (macro-context-append
                         context
                         (map-values
                           (lambda (value) #f)
                           bindings))))
                (for-each
                  (lambda (pair)
                    (macro-context-set!
                      context
                      (car pair)
                      (make-transformer context (cadr pair))))
                  bindings)
                (expand-macro context (caddr expression))))

            (($$quote)
              (cons
                '$$quote
                (relaxed-deep-map
                  (lambda (value)
                    (if (symbol? value)
                      (resolve-library-symbol value)
                      value))
                  (cdr expression))))

            (else =>
              (lambda (value)
                (if (procedure? value)
                  (let-values (((expression context) (value context expression)))
                    (expand-macro context expression))
                  (map expand expression))))))

        (else
          expression)))

    ; Compilation

    ;; Types

    ;;; Context

    (define-record-type compilation-context
      (make-compilation-context environment globals)
      compilation-context?
      (environment compilation-context-environment)
      (globals compilation-context-globals))

    (define (compilation-context-append-locals context variables)
      (make-compilation-context
        (append variables (compilation-context-environment context))
        (compilation-context-globals context)))

    (define (compilation-context-push-local context variable)
      (compilation-context-append-locals context (list variable)))

    ; If a variable is not in environment, it is considered to be global.
    (define (compilation-context-resolve context variable)
      (or
        (memv-position variable (compilation-context-environment context))
        (cond
          ((assq variable (compilation-context-globals context)) =>
            cdr)

          (else
            variable))))

    ;; Procedures

    (define constant-instruction 0)
    (define get-instruction 1)
    (define set-instruction 2)
    (define if-instruction 3)
    (define call-instruction 5)

    (define (code-rib tag car cdr)
      (rib pair-type car cdr tag))

    (define (call-rib arity procedure continuation)
      (code-rib (+ call-instruction arity) procedure continuation))

    (define (make-procedure arity code environment)
      (data-rib procedure-type environment (cons arity code)))

    (define (compile-arity argument-count variadic)
      (+
        (* 2 argument-count)
        (if variadic 1 0)))

    (define (parameter-names parameters)
      (cond
        ((pair? parameters)
          (cons (car parameters) (parameter-names (cdr parameters))))

        ((symbol? parameters)
          (list parameters))

        ((null? parameters)
          '())

        (else
          (error "invalid variadic parameter" parameters))))

    (define (count-parameters parameters)
      (if (pair? parameters)
        (+ 1 (count-parameters (cdr parameters)))
        0))

    (define (compile-constant constant continuation)
      (code-rib constant-instruction constant continuation))

    (define (compile-primitive-call name continuation)
      (call-rib
        (compile-arity
          (case name
            (($$close $$car)
              1)

            (($$cons $$-)
              2)

            (($$rib)
              4)

            (else
              (error "unknown primitive" name)))
          #f)
        name
        continuation))

    (define (drop? codes)
      (and
        (pair? codes)
        (eq? (rib-tag codes) set-instruction)
        (eq? (rib-car codes) 0)))

    (define (compile-unspecified continuation)
      (if (drop? continuation)
        ; Skip a "drop" instruction.
        (rib-cdr continuation)
        (compile-constant #f continuation)))

    (define (compile-drop continuation)
      (if (null? continuation)
        continuation
        (code-rib set-instruction 0 continuation)))

    (define (compile-sequence context expressions continuation)
      (compile-expression
        context
        (car expressions)
        (if (null? (cdr expressions))
          continuation
          (compile-drop (compile-sequence context (cdr expressions) continuation)))))

    (define (compile-raw-call context procedure arguments arity continuation)
      (if (null? arguments)
        (call-rib
          arity
          (compilation-context-resolve context procedure)
          continuation)
        (compile-expression
          context
          (car arguments)
          (compile-raw-call
            (compilation-context-push-local context #f)
            procedure
            (cdr arguments)
            arity
            continuation))))

    (define (compile-call context expression variadic continuation)
      (let* ((procedure (car expression))
             (arguments (cdr expression))
             (continue
               (lambda (context procedure continuation)
                 (compile-raw-call
                   context
                   procedure
                   arguments
                   (compile-arity
                     (- (length arguments) (if variadic 1 0))
                     variadic)
                   continuation))))
        (if (symbol? procedure)
          (continue context procedure continuation)
          (compile-expression
            context
            procedure
            (continue
              (compilation-context-push-local context '$procedure)
              '$procedure
              (compile-unbind continuation))))))

    (define (compile-unbind continuation)
      (if (null? continuation)
        continuation
        (code-rib set-instruction 1 continuation)))

    (define (compile-expression context expression continuation)
      (cond
        ((symbol? expression)
          (code-rib
            get-instruction
            (compilation-context-resolve context expression)
            continuation))

        ((pair? expression)
          (case (car expression)
            (($$apply)
              (compile-call context (cdr expression) #t continuation))

            (($$begin)
              (compile-sequence context (cdr expression) continuation))

            (($$if)
              (compile-expression
                context
                (cadr expression)
                (code-rib
                  if-instruction
                  (compile-expression context (caddr expression) continuation)
                  (compile-expression context (cadddr expression) continuation))))

            (($$lambda)
              (let ((parameters (cadr expression)))
                (compile-constant
                  (make-procedure
                    (compile-arity
                      (count-parameters parameters)
                      (symbol? (last-cdr parameters)))
                    (compile-sequence
                      (compilation-context-append-locals
                        context
                        ; #f is for a frame.
                        (reverse (cons #f (parameter-names parameters))))
                      (cddr expression)
                      '())
                    '())
                  (compile-primitive-call '$$close continuation))))

            (($$quote)
              (compile-constant (cadr expression) continuation))

            (($$set!)
              (compile-expression
                context
                (caddr expression)
                (code-rib
                  set-instruction
                  (compilation-context-resolve
                    (compilation-context-push-local context #f)
                    (cadr expression))
                  (compile-unspecified continuation))))

            (else
              (compile-call context expression #f continuation))))

        (else
          (compile-constant expression continuation))))

    (define (merge-environments one other)
      (fold-left
        (lambda (names name)
          (if (member name names)
            names
            (cons name names)))
        one
        other))

    (define eval
      (let ((libraries ($$libraries))
            (macro-context (make-macro-context (make-macro-state 0) '())))
        (for-each
          (lambda (pair)
            (macro-context-set-last!
              macro-context
              (car pair)
              (if (symbol? (cdr pair))
                (resolve-denotation macro-context (cdr pair))
                (make-transformer macro-context (cdr pair)))))
          ($$macros))

        (lambda (expression environment)
          (case (predicate expression)
            ((import)
              (unless (eq? environment (interaction-environment))
                (error "invalid import in eval"))
              (interaction-environment
                (merge-environments (interaction-environment) (cdr expression))))

            (else
              ((make-procedure
                  (compile-arity 0 #f)
                  (compile-expression
                    (make-compilation-context '() '())
                    (expand-macro
                      macro-context
                      (let ((names
                              (apply
                                append
                                (map
                                  (lambda (name)
                                    (let ((pair (assoc name libraries)))
                                      (unless pair
                                        (error "unknown library" name))
                                      (let* ((library (cdr pair))
                                             (id (car library)))
                                        (append
                                          (map
                                            (lambda (name)
                                              (cons name (build-library-symbol id name)))
                                            (cadr library))
                                          (caddr library)))))
                                  environment))))
                        (relaxed-deep-map
                          (lambda (x)
                            (cond
                              ((assq x names) =>
                                cdr)

                              (else
                                x)))
                          expression)))
                    '())
                  '())))))))

    (define environment list)))
