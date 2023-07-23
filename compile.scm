(define integer-base 128)

; Constants

(define false-index 0)
(define true-index 1)
(define null-index 2)
(define rib-index 3)
(define other-index 4)

; Instructions

(define call-instruction 0)
(define set-instruction 1)
(define get-instruction 2)
(define constant-instruction 3)
(define if-instruction 4)

;; Codes

(define return-call-code 0)
(define call-code 1)
(define close-code 2)
(define set-code 3)
(define get-code 4)
(define constant-code 5)
(define if-code 6)

; Utility

(cond-expand
  (gambit
    (define (rib tag car cdr)
      (cons (cons tag car) cdr))

    (define (rib-tag rib)
      (caar rib))

    (define (rib-car rib)
      (cdar rib))

    (define (rib-cdr rib)
      (cdr rib))

    (define (rib? value)
      (not (number? value))))

  (else))

(define (todo value) (error "not implemented" value))

(define (member-index value list)
  (if (null? list)
    (error "value not found" value)
    (if (eqv? value (car list))
      0
      (+ 1 (member-index value (cdr list))))))

; Source code reading

(define (read-all)
  (let ((x (read)))
    (if (eof-object? x)
      '()
      (cons x (read-all)))))

(define (read-source)
  (cons 'begin (read-all)))

; Target code writing

(define (write-target codes)
  (map write-u8 codes))

; Non-primitive expansion

(define (expand expression)
  (cond
    ((symbol? expression)
      expression)

    ((pair? expression)
      (let ((first (car expression)))
        (cond
          ((eqv? first 'define)
            (let ((pattern (cadr expression)))
              (cons 'set!
                (if (pair? pattern)
                  (list
                    (car pattern)
                    (expand (cons 'lambda (cons (cdr pattern) (cddr expression)))))
                  (list pattern (expand (caddr expression)))))))

          ((eqv? first 'begin)
            (cons 'begin (map expand (cdr expression))))

          (else
            expression))))

    (else
      expression)))

; Context

; (stack . symbols)
(define (make-compile-context)
  (cons '() '()))

(define (compile-context-resolve* variables variable index)
  (cond
    ((null? variables)
      variable)

    ((eqv? (car variables) variable)
      index)

    (else
      (compile-context-resolve (cdr variables) variable (+ index 1)))))

(define (compile-context-resolve context variable index)
  (compile-context-resolve* (car context) variable index))

; Compilation

(define (compile-set variable continuation)
  (rib set-instruction variable continuation))

(define (compile-begin context expressions continuation)
  (compile-expression context
    (car expressions)
    (if (pair? (cdr expressions))
      (compile-begin context (cdr expressions) continuation)
      continuation)))

(define (compile-constant constant continuation)
  (cond
    ((number? constant)
      (rib constant-instruction constant continuation))

    ((string? constant)
      (todo constant))

    ((symbol? constant)
      (todo constant))

    (else
      (todo constant))))

(define (compile-expression context expression continuation)
  (cond
    ((symbol? expression)
      (rib
        get-instruction
        (compile-context-resolve context expression 0)
        continuation))

    ((pair? expression)
      (let ((first (car expression)))
        (cond
          ((eqv? first 'set!)
            (compile-expression context (caddr expression)
              (compile-set
                (compile-context-resolve context (cadr expression) 1)
                continuation)))

          ((eqv? first 'begin)
            (compile-begin context (cdr expression) continuation))

          (else
            (todo first)))))

    (else
      (compile-constant expression continuation))))

(define (compile expression)
  (compile-expression (make-compile-context) expression '()))

; Encoding

;; Context

(define (make-encode-context symbols)
  symbols)

(define (encode-context-symbols context)
  context)

(define (find-symbols codes)
  (if (null? codes)
    '()
    (let (
        (instruction (rib-tag codes))
        (operand (rib-car codes))
        (rest (find-symbols (cdr codes))))
      (if (and
          (not (eqv? instruction if-instruction))
          (symbol? operand)
          (not (memq operand rest)))
        (cons operand rest)
        rest))))

;; Symbols

(define (encode-string string target)
  (if (null? string)
    target
    (encode-string (cdr string) (cons (char->integer (car string)) target))))

(define (encode-symbol symbol target)
  (encode-string (string->list (symbol->string symbol)) target))

(define (encode-symbols* symbols target)
  (let (
      (target (encode-symbol (car symbols) target))
      (rest (cdr symbols)))
    (if (null? rest)
      target
      (encode-symbols*
        rest
        (cons
          (char->integer #\,)
          target)))))

(define (encode-symbols symbols target)
  (let ((target (cons (char->integer #\;) target)))
    (if (null? symbols)
      target
      (encode-symbols* symbols target))))

;; Codes

(define (encode-integer-rest integer first target)
  (let ((part (modulo integer integer-base)))
    (if (eqv? part 0)
      (if first (cons 0 target) target)
      (encode-integer-rest
        (quotient integer integer-base)
        #f
        (cons (* (if first 1 -1) part) target)))))

(define (encode-integer integer target)
  (encode-integer-rest integer #t target))

(define (encode-operand context operand target)
  (cond
    ((symbol? operand)
      (encode-integer
        (* 2 (+ other-index (member-index operand (encode-context-symbols context))))
        target))

    ((number? operand)
      (encode-integer (+ (* operand 2) 1) target))

    (else (error "invalid operand"))))

(define (encode-codes context codes target)
  (if (null? codes)
    target
    (let (
        (instruction (rib-tag codes))
        (operand (rib-car codes)))
      (encode-codes
        context
        (rib-cdr codes)
        (cond
          ((eqv? instruction call-instruction)
            (cons
              (if (null? (rib-cdr codes))
                return-call-code
                call-code)
              (encode-operand context operand target)))

          ((eqv? instruction set-instruction)
            (cons
              set-code
              (encode-operand context operand target)))

          ((eqv? instruction get-instruction)
            (cons
              get-code
              (encode-operand context operand target)))

          ((eqv? instruction constant-instruction)
            (cons
              constant-code
              (encode-operand context operand target)))

          ((eqv? instruction if-instruction)
            (todo codes))

          (else (error "invalid instruction")))))))

(define (encode codes)
  (let ((context (make-encode-context (find-symbols codes))))
    (encode-symbols
      (encode-context-symbols context)
      (encode-codes context codes '()))))
