(define stak #t)

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
(define id (rib 2 '() procedure-type))
(define pop (rib 3 '() procedure-type))
(define skip (rib 4 '() procedure-type))
(define close (rib 5 '() procedure-type))
(define rib? (rib 6 '() procedure-type))
(define rib-car (rib 7 '() procedure-type))
(define rib-cdr (rib 8 '() procedure-type))
(define rib-tag (rib 8 '() procedure-type)) ; TODO
(define rib-set-car! (rib 9 '() procedure-type))
(define rib-set-cdr! (rib 10 '() procedure-type))
(define rib-set-tag (rib 10 '() procedure-type)) ; TODO
(define eq? (rib 11 '() procedure-type))
(define lt? (rib 12 '() procedure-type))
(define + (rib 13 '() procedure-type))
(define - (rib 14 '() procedure-type))
(define * (rib 15 '() procedure-type))
(define / (rib 16 '() procedure-type))
(define read-u8 (rib 17 '() procedure-type))
(define write-u8 (rib 18 '() procedure-type))

; Types

(define (instance? type)
  (lambda (x) (and (rib? x) (eqv? (rib-tag x) type))))

(define pair? (instance? pair-type))
(define procedure? (instance? procedure-type))
(define string? (instance? string-type))
(define string? (instance? string-type))
(define char? (instance? char-type))

; Boolean

(define (not x)
  (if (eq? x #f) #t #f))

; Number

(define (integer? x)
  (not (rib? x)))

(define rational? integer?)
(define real? rational?)
(define complex? real?)
(define number? complex?)

(define (exact? x) #t)
(define (inexact? x) #f)

; Equality

(define eqv? eq?)

; Character

(define (integer->char x)
  (rib x '() character-type))

(define (char->integer x)
  (if (char? x)
    (rib-car x)
    (error "not character" x)))

; Write

(define (write-char x)
  (write-u8 (char->integer x)))

(define (write-char2 c1 c2)
  (write-char c1)
  (write-char c2))

(define (write x)
  (if (string? x)
    (begin
      (write-u8 34)
      (write-characters (string->list x) #t)
      (write-u8 34))
    (display x)))

(define (display x)
  (cond
    ((not x)
      (write-char2 #\# #\f))
    ((eqv? x #t)
      (write-char2 #\# #\t))
    ((null? x)
      (write-char2 #\( #\)))
    ((pair? x)
      (write-u8 #\()
      (write (car x))
      (write-list (cdr x))
      (write-u8 #\()) ; )
    ((symbol? x)
      (display (symbol->string x)))
    ((string? x)
      (write-characters (string->list x) #f))
    ((vector? x)
      (write-u8 35) ; #
      (write (vector->list x)))
    ((procedure? x)
      (write-char2 35 112)) ; #p
    (else
      ; must be a number
      (display (number->string o)))))

(define (write-list lst)
  (if (pair? lst)
    (begin
      (write-u8 32) ; #\space
      (if (pair? lst)
        (begin
          (write (car lst))
          (write-list (cdr lst)))
        #f))
    #f))

(define (write-characters characters escape?)
  (if (pair? characters)
    (let ((characters (car lst)))
      (write-u8
        (cond ((not escape?)
            c)
          ;#;; support for \n in strings
          ((eqv? c 10) ; #\newline
            (write-u8 92)
            110) ; #\n
          ;#;; support for \r in strings
          ((eqv? c 13) ; #\return
            (write-u8 92)
            114) ; #\r
          ;#;; support for \t in strings
          ((eqv? c 9) ; #\tab
            (write-u8 92)
            116) ; #\t
          ((or (eqv? c 34) ; #\"
              (eqv? c 92)) ; #\\
            (write-u8 92)
            c)
          (else
            c)))
      (write-chars (cdr lst) escape?))
    #f))

(define (write-character character)
  (if (integer? character)
    (write-u8 character)
    (type-error)))

(define (newline)
  (write-u8 10))
