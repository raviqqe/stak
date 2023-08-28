; Syntax
;
; Those syntax definitions are copied from https://small.r7rs.org/attachment/r7rs.pdf.

(define-syntax cond
  (syntax-rules (else =>)
    ((cond (else result1 result2 ...))
      (begin result1 result2 ...))
    ((cond (test => result))
      (let ((temp test))
        (if temp (result temp))))
    ((cond (test => result) clause1 clause2 ...)
      (let ((temp test))
        (if temp
          (result temp)
          (cond clause1 clause2 ...))))
    ((cond (test)) test)
    ((cond (test) clause1 clause2 ...)
      (let ((temp test))
        (if temp
          temp
          (cond clause1 clause2 ...))))
    ((cond (test result1 result2 ...))
      (if test (begin result1 result2 ...)))
    ((cond (test result1 result2 ...)
        clause1
        clause2
        ...)
      (if test
        (begin result1 result2 ...)
        (cond clause1 clause2 ...)))))

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
(define skip (primitive 2))
(define close (primitive 3))
(define rib? (primitive 4))
(define rib-car (primitive 5))
(define rib-cdr (primitive 6))
(define rib-tag (primitive 7))
(define rib-set-car! (primitive 8))
(define rib-set-cdr! (primitive 9))
(define rib-set-tag! (primitive 10))
(define eq? (primitive 11))
(define < (primitive 12))
(define + (primitive 13))
(define - (primitive 14))
(define * (primitive 15))
(define / (primitive 16))
(define read-u8 (primitive 17))
(define write-u8 (primitive 18))

; Continuation

(define dummy-function (lambda () #f))

(define (call/cc receiver)
  (let ((continuation (rib-car (rib-cdr (rib-cdr (close dummy-function))))))
    (receiver
      (lambda (argument)
        (let ((frame (rib-cdr (rib-cdr (close dummy-function)))))
          (rib-set-car! frame continuation)
          argument)))))

(define unwind #f)

((call/cc
    (lambda (k)
      (set! unwind k)
      dummy-function)))

; Error

(define (error message)
  (unwind
    (lambda ()
      (let ((frame (rib-cdr (close dummy-function))))
        (rib-set-car! frame (cons '() '()))
        (write-string message)
        #f))))

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

(define (char->integer x)
  (rib-car x))

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

;; Number

(define (integer? x)
  (not (rib? x)))

(define rational? integer?)
(define real? rational?)
(define complex? real?)
(define number? complex?)

(define (exact? x) #t)
(define (inexact? x) #f)

;; Procedure

(define procedure? (instance? procedure-type))

;; String

(define string? (instance? string-type))

(define (list->string x)
  (rib (length x) x string-type))

(define (string->list x)
  (map integer->char (rib-cdr x)))

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
