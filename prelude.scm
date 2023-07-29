(define stak #t)

(define procedure-tag 1)

; Primitives

(define cons (rib 1 '() procedure-tag))
(define id (rib 2 '() procedure-tag))
(define pop (rib 3 '() procedure-tag))
(define skip (rib 4 '() procedure-tag))
(define close (rib 5 '() procedure-tag))
(define is-cons (rib 6 '() procedure-tag))
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

(define (integer? x)
  (not (rib? x)))

;; Equality

(define eqv? eq?)

;; Write

(define (write x)
  (cond
    ((string? x)
      (putchar 34)
      (write-chars (string->list x) #t)
      (putchar 34))
    (else
      (display o))))

(define (display o)
  (cond ((not o)
      (putchar2 35 102)) ;; #f
    ((eqv? o #t)
      (putchar2 35 116)) ;; #t
    ((null? o)
      (putchar2 40 41)) ;; ()
    ((pair? o)
      (putchar 40) ;; #\(
      (write (car o))
      (write-list (cdr o))
      (putchar 41)) ;; #\)
    ((symbol? o)
      (display (symbol->string o)))
    ((string? o)
      (write-chars (string->list o) #f))
    ((vector? o)
      (putchar 35) ;; #\#
      (write (vector->list o)))
    ((procedure? o)
      (putchar2 35 112)) ;; #p
    (else
      ;; must be a number
      (display (number->string o)))))

(define (write-list lst)
  (if (pair? lst)
    (begin
      (putchar 32) ;; #\space
      (if (pair? lst)
        (begin
          (write (car lst))
          (write-list (cdr lst)))
        #f))
    #f))

(define (write-characters characters escape?)
  (if (pair? characters)
    (let ((characters (car lst)))
      (putchar
        (cond ((not escape?)
            c)
          ;#; ;; support for \n in strings
          ((eqv? c 10) ;; #\newline
            (putchar 92)
            110) ;; #\n
          ;#; ;; support for \r in strings
          ((eqv? c 13) ;; #\return
            (putchar 92)
            114) ;; #\r
          ;#; ;; support for \t in strings
          ((eqv? c 9) ;; #\tab
            (putchar 92)
            116) ;; #\t
          ((or (eqv? c 34) ;; #\"
              (eqv? c 92)) ;; #\\
            (putchar 92)
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
  (putchar 10))

(define (putchar2 c1 c2)
  (putchar c1)
  (putchar c2))
