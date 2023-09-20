; Syntax
;
; Those syntax definitions are mostly ported from https://small.r7rs.org/attachment/r7rs.pdf.

;; Binding

(define-syntax let
  (syntax-rules ()
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

(define-syntax letrec*
  (syntax-rules ()
    ((_ ((name value) ...) body1 body2 ...)
      (let ((name #f) ...)
        (set! name value)
        ...
        body1
        body2
        ...))))

(define-syntax letrec
  (syntax-rules ()
    ((_ ((name value) ...) body1 body2 ...)
      (letrec* ((name value) ...) body1 body2 ...))))

;; Conditional

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
    ((_ (test)) test)
    ((_ (test) clause1 clause2 ...)
      (let ((temp test))
        (if temp
          temp
          (cond clause1 clause2 ...))))
    ((_ (test result1 result2 ...))
      (if test (begin result1 result2 ...)))
    ((_ (test result1 result2 ...)
        clause1
        clause2
        ...)
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
    ((_) #t)
    ((_ test) test)
    ((_ test1 test2 ...)
      (if test1 (and test2 ...) #f))))

(define-syntax or
  (syntax-rules ()
    ((_) #f)
    ((_ test) test)
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
(define procedure-type 1)
(define symbol-type 2)
(define string-type 3)
(define char-type 4)
(define vector-type 5)
(define bytevector-type 6)
(define eof-object-type 7)
(define port-type 8)

; Primitives

(define (primitive id) (rib id '() procedure-type))

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
(define $< (primitive 11))
(define $+ (primitive 12))
(define $- (primitive 13))
(define $* (primitive 14))
(define $/ (primitive 15))
(define read-u8 (primitive 16))
(define write-u8 (primitive 17))
(define dump (primitive 18))

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

(define (error message)
  (unwind
    (lambda ()
      (rib-set-car!
        (rib-cdr (close dummy-function)) ; frame
        (cons '() '()))
      (write-string message)
      #f)))

; Types

(define (instance? type)
  (lambda (x)
    (and
      (rib? x)
      (eqv? (rib-tag x) type))))

(define eqv? eq?)

;; Boolean

(define (not x)
  (eq? x #f))

;; Bytevector

(define bytevector? (instance? bytevector-type))

(define bytevector-length rib-car)

(define (bytevector-u8-ref vector index)
  (list-ref (rib-cdr vector) index))

;; Character

(define char? (instance? char-type))

(define (integer->char x)
  (rib x '() char-type))

(define char->integer rib-car)

;; List

(define pair? (instance? pair-type))

(define (null? x)
  (eq? x '()))

(define car rib-car)
(define cdr rib-cdr)

(define (length* xs y)
  (if (null? xs)
    y
    (length* (cdr xs) (+ y 1))))

(define (length xs)
  (length* xs 0))

(define (map function list)
  (if (null? list)
    list
    (cons
      (function (car list))
      (map function (cdr list)))))

(define (list-ref list index)
  (if (eqv? index 0)
    (car list)
    (list-ref (cdr list) (- index 1))))

(define (mem eq?)
  (lambda (x xs)
    (cond
      ((null? xs)
        #f)

      ((eq? x (car xs))
        xs)

      (else
        (memv x (cdr xs))))))

(define memq (mem eq?))
(define memv (mem eqv?))

(define (append . lists)
  (reduce-right append-lists '() lists))

(define (append-lists ys xs)
  (if (null? xs)
    ys
    (cons (car xs) (append-lists (cdr xs) ys))))

(define (reduce-right f y xs)
  (cond
    ((null? xs)
      y)

    ((null? (cdr xs))
      (car xs))

    (else
      (f
        (reduce-right f y (cdr xs))
        (car xs)))))

(define (fold-left f y xs)
  (if (null? xs)
    y
    (fold-left
      f
      (f y (car xs))
      (cdr xs))))

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

(define + (arithmetic-operator $+ 0))
(define - (inverse-arithmetic-operator $- 0))
(define * (arithmetic-operator $* 1))
(define / (inverse-arithmetic-operator $/ 1))

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
(define < (comparison-operator $<))
(define > (comparison-operator (lambda (x y) ($< y x))))
(define <= (comparison-operator (lambda (x y) (not ($< y x)))))
(define >= (comparison-operator (lambda (x y) (not ($< x y)))))

;; Procedure

(define procedure? (instance? procedure-type))

;; String

(define string? (instance? string-type))

(define (list->string x)
  (rib (length x) x string-type))

(define (string->list x)
  (map integer->char (rib-cdr x)))

;; Symbol

(define symbol->string rib-cdr)

;; Vector

(define vector? (instance? vector-type))

(define (list->vector x)
  (rib (length x) x vector-type))

(define vector->list rib-cdr)

; Write

(define (write-char x)
  (write-u8 (char->integer x)))

(define (write-string string)
  (map write-char (string->list string)))

(define (write-bytevector* vector index)
  (if (< index (bytevector-length vector))
    (begin
      (write-u8 (bytevector-u8-ref vector index))
      (write-bytevector* vector (+ index 1)))
    #f))

(define (write-bytevector vector)
  (write-bytevector* vector 0))

(define (newline)
  (write-char #\newline))
