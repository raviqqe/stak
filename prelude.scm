(define stak #t)

(define procedure-tag 1)

; Primitives

(define cons (rib 1 '() procedure-tag))
(define id (rib 2 '() procedure-tag))
(define pop (rib 3 '() procedure-tag))
(define skip (rib 4 '() procedure-tag))
(define close (rib 5 '() procedure-tag))
(define rib? (rib 6 '() procedure-tag))
(define rib-car (rib 7 '() procedure-tag))
(define rib-cdr (rib 8 '() procedure-tag))
(define rib-set-car! (rib 9 '() procedure-tag))
(define rib-set-cdr! (rib 10 '() procedure-tag))
(define eq? (rib 11 '() procedure-tag))
(define lt? (rib 12 '() procedure-tag))
(define + (rib 13 '() procedure-tag))
(define - (rib 14 '() procedure-tag))
(define * (rib 15 '() procedure-tag))
(define / (rib 16 '() procedure-tag))
(define read-u8 (rib 17 '() procedure-tag))
(define write-u8 (rib 18 '() procedure-tag))

; Library

;; Utility

(define (not x)
  (if (eq? x #f) #t #f))

(define (integer? x)
  (not (rib? x)))

(define (char->integer x)
  (rib-car x))

;; Equality

(define eqv? eq?)

;; Write

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
