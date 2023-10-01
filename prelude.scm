; Syntax
;
; Those syntax definitions are mostly ported from https://small.r7rs.org/attachment/r7rs.pdf.

;; Base

($$define-syntax define-syntax
  (syntax-rules ()
    ((_ name value)
      ($$define-syntax name value))))

(define-syntax define
  (syntax-rules ()
    ((_ (name argument ...) body1 body2 ...)
      (define name (lambda (argument ...) body1 body2 ...)))

    ((_ (name argument ... . rest) body1 body2 ...)
      (define name (lambda (argument ... . rest) body1 body2 ...)))

    ((_ name value)
      ($$define name value))))

(define-syntax lambda
  (syntax-rules (define define-syntax)
    ((_ arguments (define content ...) body1 body2 ...)
      (lambda "value" arguments () (define content ...) body1 body2 ...))

    ((_ "value" arguments ((name value) ...)
        (define (new-name argument ...) body1 body2 ...)
        body3
        body4
        ...)
      (lambda "value" arguments ((name value) ...)
        (define new-name (lambda (argument ...) body1 body2 ...))
        body3
        body4
        ...))

    ((_ "value" arguments ((name value) ...)
        (define (new-name argument ... . rest) body1 body2 ...)
        body3
        body4
        ...)
      (lambda "value" arguments ((name value) ...)
        (define new-name (lambda (argument ... . rest) body1 body2 ...))
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

(define-syntax quasiquote
  (syntax-rules ()
    ((_ value)
      ($$quasiquote value))))

(define-syntax quote
  (syntax-rules ()
    ((_ value)
      ($$quote value))))

(define-syntax set!
  (syntax-rules ()
    ((_ name value)
      ($$set! name value))))

;; Library system

; TODO Implement an import statement.
(define-syntax import
  (syntax-rules ()
    ((_ x ...)
      #f)))

;; Binding

(define-syntax let
  (syntax-rules ()
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
      ((letrec ((tag (lambda (name ...) body1 body2 ...))) tag)
        value
        ...))))

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

    ((_ (test => result))
      (let ((temp test))
        (if temp (result temp))))

    ((_ (test => result) clause1 clause2 ...)
      (let ((temp test))
        (if temp
          (result temp)
          (cond clause1 clause2 ...))))

    ((_ (test))
      test)

    ((_ (test) clause1 clause2 ...)
      (let ((temp test))
        (if temp
          temp
          (cond clause1 clause2 ...))))

    ((_ (test result1 result2 ...))
      (if test (begin result1 result2 ...)))

    ((_ (test result1 result2 ...) clause1 clause2 ...)
      (if test
        (begin result1 result2 ...)
        (cond clause1 clause2 ...)))))

(define-syntax case
  (syntax-rules (else =>)
    ((_ (key ...)
        clause
        ...)
      (let ((atom-key (key ...)))
        (case atom-key clause ...)))

    ((_ key
        (else => result))
      (result key))

    ((_ key
        (else result1 result2 ...))
      (begin result1 result2 ...))

    ((_ key
        ((atoms ...) result1 result2 ...))
      (if (memv key '(atoms ...))
        (begin result1 result2 ...)))

    ((_ key
        ((atoms ...) => result))
      (if (memv key '(atoms ...))
        (result key)))

    ((_ key
        ((atoms ...) => result)
        clause1
        clause2
        ...)
      (if (memv key '(atoms ...))
        (result key)
        (case key clause1 clause2 ...)))

    ((_ key
        ((atoms ...) result1 result2 ...)
        clause1
        clause2
        ...)
      (if (memv key '(atoms ...))
        (begin result1 result2 ...)
        (case key clause1 clause2 ...)))))

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

(define-syntax when
  (syntax-rules ()
    ((_ test result1 result2 ...)
      (if test
        (begin result1 result2 ...)))))

(define-syntax unless
  (syntax-rules ()
    ((_ test result1 result2 ...)
      (when (not test) result1 result2 ...))))

;; Record

(define-syntax define-record-type
  (syntax-rules ()
    ((define-record-type type
        (constructor field ...)
        predicate
        (field getter . rest)
        ...)
      (begin
        (define type (list 'type))

        (define constructor
          (record-constructor type '(constructor-tag ...)))

        (define predicate (record-predicate type))

        (define-record-field type field accessor . more)
        ...))))

(define record? (instance? record-type))

(define (record-predicate type)
  (lambda (x)
    (and
      (record? x)
      (eq? (rib-cdr x)))))

; Type IDs

(define pair-type 0)
(define procedure-type 1)
(define symbol-type 2)
(define string-type 3)
(define char-type 4)
(define vector-type 5)
(define bytevector-type 6)
(define eof-object-type 7)
(define port-type 8)
(define record-type 9)

; Primitives

(define (primitive id) ($$rib id '() procedure-type))

(define rib $$rib)
(define cons (primitive 1))
(define close (primitive 2))
(define rib? (primitive 3))
(define rib-car (primitive 4))
(define rib-cdr (primitive 5))
(define rib-tag (primitive 6))
(define rib-set-car! (primitive 7))
(define rib-set-cdr! (primitive 8))
(define rib-set-tag! (primitive 9))
(define eq? (primitive 10))
(define $$< (primitive 11))
(define $$+ (primitive 12))
(define $$- (primitive 13))
(define $$* (primitive 14))
(define $$/ (primitive 15))
(define $$read-u8 (primitive 16))
(define $$write-u8 (primitive 17))
(define dump (primitive 18))

(define (apply f xs)
  ($$apply f xs))

; Continuation

(define dummy-function (lambda () #f))

(define (call/cc receiver)
  (let ((continuation (rib-car (rib-cdr (rib-cdr (rib-cdr (close dummy-function)))))))
    (receiver
      (lambda (argument)
        (rib-set-car!
          (rib-cdr (rib-cdr (close dummy-function))) ; frame
          continuation)
        argument))))

(define unwind #f)

((call/cc
    (lambda (k)
      (set! unwind k)
      dummy-function)))

; Error

(define (error message . rest)
  (unwind
    (lambda ()
      (rib-set-car!
        (rib-cdr (close dummy-function)) ; frame
        (cons '() '()))
      (write-string message (current-error-port))
      (write-char #\space (current-error-port))
      (when (pair? rest)
        (write (car rest) (current-error-port)))
      #f)))

; Types

(define (singleton? x)
  (or
    (null? x)
    (boolean? x)))

(define (instance? type)
  (lambda (x)
    (and
      (not (singleton? x))
      (rib? x)
      (eqv? (rib-tag x) type))))

(define (eqv? x y)
  (if (and (char? x) (char? y))
    (eqv? (char->integer x) (char->integer y))
    (eq? x y)))

(define (equal? x y)
  (or
    (eq? x y)
    (and
      (not (singleton? x))
      (not (singleton? y))
      (rib? x)
      (rib? y)
      (eq? (rib-tag x) (rib-tag y))
      (equal? (rib-car x) (rib-car y))
      (equal? (rib-cdr x) (rib-cdr y)))))

;; Boolean

(define (boolean? x)
  (or
    (eq? x #f)
    (eq? x #t)))

(define (not x)
  (eq? x #f))

;; Bytevector

(define bytevector? (instance? bytevector-type))

(define bytevector-length rib-car)

(define (bytevector-u8-ref vector index)
  (list-ref (rib-cdr vector) index))

;; Character

(define char? (instance? char-type))

(define (char-whitespace? x)
  (pair? (memv x '(#\newline #\return #\space #\tab))))

(define (integer->char x)
  (rib x '() char-type))

(define char->integer rib-car)

;; EOF object

(define eof (rib 0 '() eof-object-type))

(define eof-object? (instance? eof-object-type))

(define (eof-object) eof)

;; List

(define pair? (instance? pair-type))

(define (null? x)
  (eq? x '()))

(define car rib-car)
(define cdr rib-cdr)
(define set-car! rib-set-car!)
(define set-cdr! rib-set-cdr!)
(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
(define (caaar x) (car (caar x)))
(define (caddr x) (car (cddr x)))

(define (list . xs) xs)

(define (length xs)
  (let loop ((xs xs) (y 0))
    (if (null? xs)
      y
      (loop (cdr xs) (+ y 1)))))

(define (map function list)
  (if (null? list)
    list
    (cons
      (function (car list))
      (map function (cdr list)))))

(define for-each map)

(define (list-ref list index)
  (if (eqv? index 0)
    (car list)
    (list-ref (cdr list) (- index 1))))

(define (member x xs . rest)
  (define eq?
    (if (null? rest)
      equal?
      (car rest)))

  (cond
    ((null? xs)
      #f)

    ((eq? x (car xs))
      xs)

    (else
      (member x (cdr xs) eq?))))

(define (memq x xs) (member x xs eq?))
(define (memv x xs) (member x xs eqv?))

(define (assoc x xs . rest)
  (define eq?
    (if (null? rest)
      equal?
      (car rest)))

  (if (null? xs)
    #f
    (let ((pair (car xs)))
      (if (eq? x (car pair))
        pair
        (assoc x (cdr xs) eq?)))))

(define (assq x xs) (assoc x xs eq?))
(define (assv x xs) (assoc x xs eqv?))

(define (append . lists)
  (reduce-right append-lists '() lists))

(define (append-lists ys xs)
  (if (null? xs)
    ys
    (cons (car xs) (append-lists ys (cdr xs)))))

(define (reverse xs)
  (let loop ((xs xs) (ys '()))
    (if (null? xs)
      ys
      (loop (cdr xs) (cons (car xs) ys)))))

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
  (cond
    ((null? xs)
      y)

    ((null? (cdr xs))
      (car xs))

    (else
      (f (reduce-right f y (cdr xs)) (car xs)))))

;; Number

(define (integer? x)
  (not (rib? x)))

(define rational? integer?)
(define real? rational?)
(define complex? real?)
(define number? complex?)

(define (exact? x) #t)
(define (inexact? x) #f)

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

(define (comparison-operator f)
  (lambda xs
    (if (null? xs)
      #t
      (let loop (
          (x (car xs))
          (xs (cdr xs)))
        (if (null? xs)
          #t
          (let ((y (car xs)))
            (and (f x y) (loop y (cdr xs)))))))))

(define = (comparison-operator eqv?))
(define < (comparison-operator $$<))
(define > (comparison-operator (lambda (x y) ($$< y x))))
(define <= (comparison-operator (lambda (x y) (not ($$< y x)))))
(define >= (comparison-operator (lambda (x y) (not ($$< x y)))))

(define (abs x)
  (if (< x 0)
    (- 0 x)
    x))

(define (number->string x . rest)
  (let ((radix (if (null? rest) 10 (car rest))))
    (list->string
      (append
        (if (< x 0)
          (list #\-)
          '())
        (let loop ((x (abs x)) (ys '()))
          (let* (
              (q (/ x radix))
              (d (- x (* q radix)))
              (ys
                (cons
                  (integer->char
                    (if (< 9 d)
                      (+ (char->integer #\a) (- d 10))
                      (+ (char->integer #\0) d)))
                  ys)))
            (if (< 0 q)
              (loop q ys)
              ys)))))))

(define digit-characters
  (map
    (lambda (pair)
      (cons
        (cons
          (char->integer (caar pair))
          (char->integer (cdar pair)))
        (cdr pair)))
    '(
      ((#\0 . #\9) . 0)
      ((#\A . #\F) . 10)
      ((#\a . #\f) . 10))))

(define (string->number x . rest)
  (define radix (if (null? rest) 10 (car rest)))

  (define (convert-digit x)
    (let* (
        (x (char->integer x))
        (y
          (member
            x
            digit-characters
            (lambda (x pair) (<= (caar pair) x (cdar pair))))))
      (and y (+ (- x (caaar y)) (cdar y)))))

  (define (convert xs)
    (let loop ((xs xs) (y 0))
      (if (null? xs)
        y
        (let ((x (convert-digit (car xs))))
          (and x (loop (cdr xs) (+ (* radix y) x)))))))

  (let ((xs (string->list x)))
    (cond
      ((null? xs)
        #f)

      ((eqv? (car xs) #\-)
        (let ((x (convert (cdr xs))))
          (and x (- x))))

      (else
        (convert xs)))))

;; Port

(define port? (instance? port-type))

; TODO Use a record type.
(define (make-port name)
  (rib #f name port-type))

; TODO Support multiple bytes.
(define (port-last-byte port)
  (rib-car port))

(define (port-set-last-byte! port byte)
  (rib-set-car! port byte))

(define stdin-port (make-port 'stdin))

(define stdout-port (make-port 'stdout))

(define stderr-port (make-port 'stderr))

(define (current-input-port) stdin-port)

(define (current-output-port) stdout-port)

(define (current-error-port) stderr-port)

;; Procedure

(define procedure? (instance? procedure-type))

;; String

(define string? (instance? string-type))

(define (list->string x)
  (rib (length x) (map char->integer x) string-type))

(define (string->list x)
  (map integer->char (rib-cdr x)))

(define (string-append . xs)
  (list->string (apply append (map string->list xs))))

;; Symbol

(define symbol? (instance? symbol-type))

(define symbol-table (rib-cdr $$rib))
; Allow garbage collection for a symbol table.
(rib-set-cdr! $$rib #f)

(define symbol->string rib-cdr)

(define (string->symbol x)
  (let ((pair (member x symbol-table (lambda (x y) (equal? x (symbol->string y))))))
    (if pair
      (car pair)
      (let ((x (rib #f (string-append x) symbol-type)))
        (set! symbol-table (cons x symbol-table))
        x))))

;; Vector

(define vector? (instance? vector-type))

(define (vector-length xs)
  (length (vector->list xs)))

(define (list->vector x)
  (rib (length x) x vector-type))

(define vector->list rib-cdr)

; Read

(define special-chars
  '(
    ("alarm" . #\alarm)
    ("backspace" . #\backspace)
    ("delete" . #\delete)
    ("escape" . #\escape)
    ("newline" . #\newline)
    ("null" . #\null)
    ("return" . #\return)
    ("space" . #\space)
    ("tab" . #\tab)))

(define (get-input-port rest)
  (if (null? rest) stdin-port (car rest)))

(define (input-byte->char x)
  (if (number? x) (integer->char x) x))

(define (read-u8 . rest)
  (let* (
      (port (get-input-port rest))
      (x (port-last-byte port)))
    (if x
      (begin
        (port-set-last-byte! port #f)
        x)
      (or ($$read-u8) eof))))

(define (peek-u8 . rest)
  (let* (
      (port (get-input-port rest))
      (x (read-u8 port)))
    (port-set-last-byte! port x)
    x))

(define (read-char . rest)
  (input-byte->char (read-u8 (get-input-port rest))))

(define (peek-char . rest)
  (input-byte->char (peek-u8 (get-input-port rest))))

(define (read . rest)
  (let* (
      (port (get-input-port rest))
      (char (peek-non-whitespace-char port)))
    (cond
      ((eof-object? char)
        char)

      ((eqv? char #\()
        (read-list port))

      ((eqv? char #\#)
        (read-char port)
        (let ((char (peek-char port)))
          (cond
            ((eqv? char #\f)
              (read-char port)
              #f)

            ((eqv? char #\t)
              (read-char port)
              #t)

            ((eqv? char #\\)
              (read-char port)
              (let ((char (peek-char port)))
                (if (char-whitespace? char)
                  (read-char port)
                  (let ((x (read-symbol-chars port)))
                    (cond
                      ((null? x)
                        (read-char port))

                      ((eqv? (length x) 1)
                        (car x))

                      (else
                        (cdr (assoc (list->string x) special-chars))))))))

            (else
              (list->vector (read-list port))))))

      ((eqv? char #\')
        (read-char port)
        (list 'quote (read port)))

      ((eqv? char #\`)
        (read-char port)
        (list 'quasiquote (read port)))

      ((eqv? char #\,)
        (read-char port)
        (if (eqv? (peek-char port) #\@)
          (begin
            (read-char port)
            (list 'unquote-splicing (read port)))
          (list 'unquote (read port))))

      ((eqv? char #\")
        (read-string port))

      (else
        (let ((x (list->string (read-symbol-chars port))))
          (or (string->number x) (string->symbol x)))))))

(define (read-list port)
  (define (read-tail)
    (let ((char (peek-non-whitespace-char port)))
      (cond
        ((eqv? char #\))
          (read-char port)
          '())

        (else
          (let ((x (read port)))
            (if (and (symbol? x) (equal? (symbol->string x) "."))
              (let ((x (read port)))
                (read-char port)
                x)
              (cons x (read-tail))))))))

  (unless (eqv? (read-char port) #\()
    (error "( expected"))
  (read-tail))

(define (read-symbol-chars port)
  (let ((char (peek-char port)))
    (if (or
        (memv char '(#\( #\)))
        (eof-object? char)
        (char-whitespace? char))
      '()
      (cons (read-char port) (read-symbol-chars port)))))

(define (read-string port)
  (unless (eqv? (read-char port) #\")
    (error "\" expected"))
  (let loop ((xs '()))
    (let ((char (read-char port)))
      (cond
        ((eof-object? char)
          (error "unexpected end of port"))

        ((eqv? char #\")
          (list->string (reverse xs)))

        ((eqv? char #\\)
          (let ((char (read-char port)))
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

(define (peek-non-whitespace-char port)
  (let ((char (peek-char port)))
    (if (eof-object? char)
      char
      (cond
        ((char-whitespace? char)
          (begin
            (read-char port)
            (peek-non-whitespace-char port)))

        ((eqv? char #\;)
          (skip-comment port))

        (else
          char)))))

(define (skip-comment port)
  (let ((char (read-char port)))
    (cond
      ((eof-object? char)
        char)

      ((eqv? char #\newline)
        (peek-non-whitespace-char port))

      (else
        (skip-comment port)))))

; Write

(define (get-output-port rest)
  (if (null? rest) stdout-port (car rest)))

(define special-char-names
  (map
    (lambda (pair) (cons (cdr pair) (car pair)))
    special-chars))

(define escaped-chars
  '(
    (#\newline . #\n)
    (#\tab . #\t)
    (#\return . #\r)))

(define (write-u8 byte . rest)
  ; TODO Use a port.
  ($$write-u8 byte))

(define (write-char x . rest)
  (write-u8 (char->integer x) (get-output-port rest)))

(define (write-escaped-char x . rest)
  (let (
      (port (get-output-port rest))
      (pair (assoc x escaped-chars)))
    (if pair
      (begin
        (write-char #\\ port)
        (write-char (cdr pair) port))
      (write-char x port))))

(define (write-string x . rest)
  (for-each
    (lambda (x) (write-char x port))
    (string->list x)))

(define (write-bytevector xs . rest)
  (define port (if (null? rest) stdout-port (car rest)))

  (let loop ((xs xs) (index 0))
    (if (< index (bytevector-length xs))
      (begin
        (write-u8 (bytevector-u8-ref xs index) port)
        (loop xs (+ index 1)))
      #f)))

(define (newline . rest)
  (write-char #\newline (get-output-port rest)))

(define (write x . rest)
  (define port (if (null? rest) stdout-port (car rest)))

  (cond
    ((char? x)
      (write-char #\# port)
      (write-char #\\ port)
      (let ((pair (assoc x special-char-names)))
        (if pair
          (display (cdr pair) port)
          (write-char x port))))

    ((pair? x)
      (write-list x write port))

    ((string? x)
      (write-char #\" port)
      (for-each
        (lambda (x) (write-escaped-char x port))
        (string->list x))
      (write-char #\" port))

    ((vector? x)
      (write-vector x write port))

    (else
      (display x))))

(define (display x . rest)
  (define port (if (null? rest) stdout-port (car rest)))

  (cond
    ((not x)
      (write-char #\# port)
      (write-char #\f port))

    ((eqv? x #t)
      (write-char #\# port)
      (write-char #\t port))

    ((char? x)
      (write-char x port))

    ((null? x)
      (write-list x display port))

    ((number? x)
      (display (number->string x) port))

    ((pair? x)
      (write-list x display port))

    ((procedure? x)
      (write-char #\# port)
      (write-string "procedure" port))

    ((string? x)
      (write-string x port))

    ((symbol? x)
      (display (symbol->string x) port))

    ((vector? x)
      (write-vector x display port))

    (else
      (error "unknown type"))))

(define (write-list xs write port)
  (write-char #\( port)

  (when (pair? xs)
    (write (car xs) port)
    (let loop ((xs (cdr xs)))
      (cond
        ((pair? xs)
          (write-char #\space port)
          (write (car xs) port)
          (loop (cdr xs)))

        ((null? xs)
          #f)

        (else
          (write-char #\space port)
          (write-char #\. port)
          (write-char #\space port)
          (write xs port)))))

  (write-char #\) port))

(define (write-vector xs write port)
  (write-char #\# port)
  (write-list (vector->list xs) write port))
