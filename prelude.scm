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

(define cons (rib 1 '() procedure-type))
; TODO Define a primitive of the number 2.
(define pop (rib 3 '() procedure-type))
(define skip (rib 4 '() procedure-type))
(define close (rib 5 '() procedure-type))
(define rib? (rib 6 '() procedure-type))
(define rib-car (rib 7 '() procedure-type))
(define rib-cdr (rib 8 '() procedure-type))
(define rib-tag (rib 9 '() procedure-type))
(define rib-set-car! (rib 10 '() procedure-type))
(define rib-set-cdr! (rib 11 '() procedure-type))
(define rib-set-tag! (rib 12 '() procedure-type))
(define eq? (rib 13 '() procedure-type))
(define < (rib 14 '() procedure-type))
(define + (rib 15 '() procedure-type))
(define - (rib 16 '() procedure-type))
(define * (rib 17 '() procedure-type))
(define / (rib 18 '() procedure-type))
(define read-u8 (rib 19 '() procedure-type))
(define write-u8 (rib 20 '() procedure-type))

; Error

(define (todo)
  ; TODO Throw an error.
  (#f))

(define (error message)
  ; TODO Throw an error.
  (#f))

(define (type-error)
  ; TODO Set an error message.
  (error #f))

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
  (rib-cdr x))

; Write

(define (write-char x)
  (write-u8 (char->integer x)))

; Continuation

(define (call/cc receiver)
  (let ((continuation (rib-car (rib-cdr (rib-cdr (lambda () #f))))))
    (receiver (lambda (argument)
        (let ((frame (rib-cdr (rib-cdr (lambda () #f)))))
          (rib-set-car! frame continuation)
          argument)))))
