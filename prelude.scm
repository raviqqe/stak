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

(define (primitive id) ($$rib procedure-type '() id 0))

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
(define $$read-u8 (primitive 16))
(define $$write-u8 (primitive 17))
(define $$write-error-u8 (primitive 18))
(define $$halt (primitive 19))

(define (data-rib type car cdr)
  (rib type car cdr 0))

(define (apply f xs)
  ($$apply f xs))

; Basic types

(define (instance? type)
  (lambda (x)
    (and
      (rib? x)
      (eq? (rib-type x) type))))

(define (eqv? x y)
  (if (and (char? x) (char? y))
    (eqv? (char->integer x) (char->integer y))
    (eq? x y)))

(define (equal? x y)
  (or
    (eq? x y)
    (and
      (rib? x)
      (rib? y)
      (eq? (rib-type x) (rib-type y))
      (equal? (rib-car x) (rib-car y))
      (equal? (rib-cdr x) (rib-cdr y)))))

;; Record

; We use record types only for certain built-in types not to degrade space
; efficiency of their values
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
    (data-rib record-type (list->vector xs) type)))

(define (record-predicate type)
  (lambda (x)
    (and
      (record? x)
      (eq? (rib-cdr x) type))))

(define (record-getter type field)
  (let ((index (field-index type field)))
    (lambda (record)
      (vector-ref (rib-car record) index))))

(define (record-setter type field)
  (let ((index (field-index type field)))
    (lambda (record value)
      (vector-set! (rib-car record) index value))))

(define (field-index type field)
  (memv-position field (cdr type)))

;; Boolean

(define boolean? (instance? boolean-type))

(define (not x)
  (eq? x #f))

;; Number

(define (integer? x)
  (not (rib? x)))

(define rational? integer?)
(define real? rational?)
(define complex? real?)
(define number? complex?)

(define (exact? x) #t)
(define (inexact? x) #f)

(define (zero? x) (eqv? x 0))
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
(define quotient (inverse-arithmetic-operator $$/ 1))
(define / quotient)

(define (modulo x y)
  (let ((q (quotient x y)))
    (let ((r (- x (* y q))))
      (if (eqv? r 0)
        0
        (if (eqv? (< x 0) (< y 0))
          r
          (+ r y))))))

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

;; Bytevector

(define bytevector? (instance? bytevector-type))

(define bytevector-length rib-cdr)

(define (bytevector-u8-ref vector index)
  ; TODO Do not export `byte-vector-u8-ref`.
  ; We need to use `rib-car` instead of `bytevector->list` because we re-define
  ; the function in a compiler.
  (list-ref (rib-car vector) index))

(define (list->bytevector x)
  (data-rib bytevector-type x (length x)))

(define bytevector->list rib-car)

;; Character

(define char? (instance? char-type))

(define (char-whitespace? x)
  (pair? (memv x '(#\newline #\return #\space #\tab))))

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

(define null? (instance? null-type))
(define pair? (instance? pair-type))

(define (list? x)
  (or
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
(define (cddddr x) (cdr (cdddr x)))

(define (list . xs) xs)

(define (make-list length . rest)
  (define fill (if (null? rest) #f (car rest)))

  (let loop ((length length))
    (if (= length 0)
      '()
      (cons fill (loop (- length 1))))))

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
  (car (list-tail list index)))

(define (list-set! list index value)
  (set-car! (list-tail list index) value))

(define (list-tail list index)
  (if (zero? index)
    list
    (list-tail (cdr list) (- index 1))))

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

(define (list-position f xs)
  (let loop ((xs xs) (index 0))
    (cond
      ((null? xs)
        #f)

      ((f (car xs))
        index)

      (else
        (loop (cdr xs) (+ index 1))))))

(define (memv-position one xs)
  (list-position (lambda (other) (eqv? one other)) xs))

;; Procedure

(define procedure? (instance? procedure-type))

;; String

(define string? (instance? string-type))

(define (list->string x)
  (data-rib string-type (map char->integer x) (length x)))

(define (string->list x)
  (map integer->char (rib-car x)))

(define (string-append . xs)
  (list->string (apply append (map string->list xs))))

(define string-length rib-cdr)

(define (string-ref x index)
  (integer->char (list-ref (rib-car x) index)))

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
      ((#\A . #\Z) . 10)
      ((#\a . #\z) . 10))))

(define (convert-digit x radix)
  (let* (
      (x (char->integer x))
      (y
        (member
          x
          digit-characters
          (lambda (x pair) (<= (caar pair) x (cdar pair))))))
    (and
      y
      ; TODO Fix performance.
      (let ((y (+ (- x (caaar y)) (cdar y))))
        (and (< y radix) y)))))

(define (string->number x . rest)
  (define radix (if (null? rest) 10 (car rest)))

  (define (convert xs)
    (and
      (pair? xs)
      (let loop ((xs xs) (y 0))
        (if (null? xs)
          y
          (let ((x (convert-digit (car xs) radix)))
            (and x (loop (cdr xs) (+ (* radix y) x))))))))

  (let ((xs (string->list x)))
    (if (and (pair? xs) (eqv? (car xs) #\-))
      (let ((x (convert (cdr xs))))
        (and x (- x)))
      (convert xs))))

;; Symbol

(define symbol? (instance? symbol-type))

(define symbol-table (rib-car $$rib))
; Allow garbage collection for a symbol table.
(rib-set-car! $$rib #f)

(define symbol->string rib-car)

(define (string->symbol x)
  ; TODO Remove this hack.
  ; Currently, internal symbols do not have any string representation.
  (cond
    ((equal? x "$$false")
      '$$false)

    ((equal? x "$$true")
      '$$true)

    ((equal? x "$$null")
      '$$null)

    ((equal? x "$$rib")
      '$$rib)

    (else
      (let ((pair (member x symbol-table (lambda (x y) (equal? x (symbol->string y))))))
        (if pair
          (car pair)
          (let ((x (data-rib symbol-type (string-append x) #f)))
            (set! symbol-table (cons x symbol-table))
            x))))))

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

; Control

;; Multi-value

(define (values . xs)
  (make-tuple xs))

(define (call-with-values producer consumer)
  (let ((xs (producer)))
    (if (tuple? xs)
      (apply consumer (tuple-values xs))
      (consumer xs))))

;; Continuation

(define dummy-function (lambda () #f))

(define (call/cc receiver)
  (let (
      (continuation (rib-car (rib-cdr (rib-cdr (rib-car (close dummy-function))))))
      (point current-point))
    (receiver
      (lambda (argument)
        (travel-to-point! current-point point)
        (set-current-point! point)
        (rib-set-car!
          (rib-cdr (rib-car (close dummy-function))) ; frame
          continuation)
        argument))))

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
      (let* (
          (parameter parameter1)
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
    (let* (
        (exception (cdr pair))
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
                      (write value))
                    (error-object-irritants exception)))
                (write exception))
              (newline)
              ($$halt))))))))

(define (with-exception-handler handler thunk)
  (let (
      (new (convert-exception-handler handler))
      (old (current-exception-handler)))
    (parameterize (
        (current-exception-handler
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
    (eqv? (error-object-type error) type)))

(define error (error-type #f))
(define read-error (error-type 'read))
(define file-error (error-type 'file))

(define read-error? (error-type? 'read))
(define file-error? (error-type? 'file))

(define-syntax guard
  (syntax-rules ()
    ((_ (name clause ...) body1 body2 ...)
      ((call/cc
          (lambda (guard-continuation)
            (with-exception-handler
              (lambda (exception)
                ((call/cc
                    (lambda (handler-continuation)
                      (guard-continuation
                        (lambda ()
                          (let ((name exception))
                            (guard*
                              (handler-continuation
                                (lambda () (raise-continuable name)))
                              clause
                              ...))))))))
              (lambda ()
                (call-with-values
                  (lambda () body1 body2 ...)
                  (lambda arguments
                    (guard-continuation
                      (lambda ()
                        (apply values arguments)))))))))))))

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
      dummy-function)))

; Derived types

;; EOF object

(define-record-type eof-object
  (make-eof-object)
  eof-object?)

(define eof (make-eof-object))

(define (eof-object) eof)

;; Port

; TODO Support multiple bytes.
(define-record-type port
  (make-port* descriptor last-byte)
  port?
  (descriptor port-descriptor)
  (last-byte port-last-byte port-set-last-byte!))

(define (make-port descriptor)
  (make-port* descriptor #f))

(define current-input-port (make-parameter (make-port 'stdin)))
(define current-output-port (make-parameter (make-port 'stdout)))
(define current-error-port (make-parameter (make-port 'stderr)))

;; Tuple

; A tuple is primarily used to represent multiple values.
(define-record-type tuple
  (make-tuple values)
  tuple?
  (values tuple-values))

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
  (if (null? rest) (current-input-port) (car rest)))

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

                    ((eqv? (length x) 1)
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
    (if (or
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
        (skip-comment)))))

; Write

(define (get-output-port rest)
  (if (null? rest) (current-output-port) (car rest)))

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
  (case (port-descriptor (get-output-port rest))
    ((stdout)
      ($$write-u8 byte))

    ((stderr)
      ($$write-error-u8 byte))

    (else
      (error "invalid port"))))

(define (write-char x . rest)
  (write-u8 (char->integer x) (get-output-port rest)))

(define (write-escaped-char x)
  (let ((pair (assoc x escaped-chars)))
    (if pair
      (begin
        (write-char #\\)
        (write-char (cdr pair)))
      (write-char x))))

(define (write-string x . rest)
  (parameterize ((current-output-port (get-output-port rest)))
    (for-each write-char (string->list x))))

(define (write-bytevector xs . rest)
  (parameterize ((current-output-port (get-output-port rest)))
    (let loop ((xs xs) (index 0))
      (if (< index (bytevector-length xs))
        (begin
          (write-u8 (bytevector-u8-ref xs index))
          (loop xs (+ index 1)))
        #f))))

(define (newline . rest)
  (write-char #\newline (get-output-port rest)))

(define (write x . rest)
  (parameterize ((current-output-port (get-output-port rest)))
    (cond
      ((bytevector? x)
        (write-formatted-bytevector x write))

      ((char? x)
        (write-char #\#)
        (write-char #\\)
        (let ((pair (assoc x special-char-names)))
          (if pair
            (display (cdr pair))
            (write-char x))))

      ((pair? x)
        (write-list x write))

      ((string? x)
        (write-char #\")
        (for-each write-escaped-char (string->list x))
        (write-char #\"))

      ((vector? x)
        (write-vector x write))

      (else
        (display x)))))

(define (display x . rest)
  (parameterize ((current-output-port (get-output-port rest)))
    (cond
      ((not x)
        (write-string "#f"))

      ((eqv? x #t)
        (write-string "#t"))

      ((bytevector? x)
        (write-formatted-bytevector x display))

      ((char? x)
        (write-char x))

      ((null? x)
        (write-sequence x display))

      ((number? x)
        (display (number->string x)))

      ((pair? x)
        (write-list x display))

      ((procedure? x)
        (write-string "#procedure"))

      ((record? x)
        (write-string "#record"))

      ((string? x)
        (write-string x))

      ((symbol? x)
        (display (symbol->string x)))

      ((vector? x)
        (write-vector x display))

      (else
        (error "unknown type")))))

(define (write-list xs write)
  (if (null? xs)
    (write-sequence xs write)
    (case (car xs)
      ((quote)
        (write-quote #\' (cadr xs) write))

      ((quasiquote)
        (write-quote #\` (cadr xs) write))

      ((unquote)
        (write-quote #\, (cadr xs) write))

      (else
        (write-sequence xs write)))))

(define (write-sequence xs write)
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

(define (write-quote char value write)
  (write-char char)
  (write value))

(define (write-formatted-bytevector xs write)
  (write-string "#u8")
  (write-sequence (bytevector->list xs) write))

(define (write-vector xs write)
  (write-char #\#)
  (write-sequence (vector->list xs) write))

; Process context

(define exit-success (data-rib procedure-type #f (cons 0 '())))

(define (emergency-exit . rest)
  (if (or (null? rest) (eqv? (car rest) #t))
    (exit-success)
    ($$halt)))

(define (exit . rest)
  (unwind (lambda () (apply emergency-exit rest))))
