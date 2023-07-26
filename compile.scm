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

; Types

(define pair-type 0)
(define procedure-type 1)

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

(define reserved-symbols '(rib))

(define (todo value) (error "not implemented" value))

(define (i8->u8 value)
  (if (< value 0)
    (+ 256 value)
    value))

(define (member-index value list)
  (if (null? list)
    (error "value not found" value)
    (if (eqv? value (car list))
      0
      (+ 1 (member-index value (cdr list))))))

(define (make-procedure code environment)
  (rib procedure-type code environment))

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
          ((eqv? first 'begin)
            (cons 'begin (map expand (cdr expression))))

          ((eqv? first 'define)
            (let ((pattern (cadr expression)))
              (cons 'set!
                (if (pair? pattern)
                  (list
                    (car pattern)
                    (expand (cons 'lambda (cons (cdr pattern) (cddr expression)))))
                  (list pattern (expand (caddr expression)))))))

          ((eqv? first 'if)
            (list
              'if
              (expand (cadr expression))
              (expand (caddr expression))
              (if (pair? (cdddr expression))
                (expand (cadddr expression))
                #f)))

          ((eqv? first 'lambda)
            (append (list 'lambda (cadr expression)) (map expand (cddr expression))))

          (else
            (map expand expression)))))

    (else
      expression)))

; Context

; (environment . symbols)
(define (make-compile-context)
  (cons '() '()))

(define (compile-context-environment context)
  (car context))

(define (compile-context-environment-set context environment)
  (cons environment (cdr context)))

(define (compile-context-environment-append context variables)
  (compile-context-environment-set
    context
    (append variables (compile-context-environment context))))

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

(define (compile-constant constant continuation)
  (rib constant-instruction constant continuation))

(define (compile-primitive-call name continuation)
  (compile-constant
    (cond
      ((memq name '(id))
        1)

      ((memq name '(-))
        2)

      ((memq name '(rib))
        3)

      (else
        (error "unknown primitive" name)))
    (rib call-instruction name continuation)))

(define (compile-set variable continuation)
  (rib set-instruction variable continuation))

(define (compile-begin context expressions continuation)
  (compile-expression context
    (car expressions)
    (if (pair? (cdr expressions))
      ; TODO Drop intermediate values.
      (compile-begin context (cdr expressions) continuation)
      continuation)))

(define (compile-call* context function arguments argument-count continuation)
  (if (null? arguments)
    (compile-constant
      argument-count
      (rib
        call-instruction
        (if (symbol? function) function (+ argument-count 1))
        continuation))
    (compile-expression context
      (car arguments)
      (compile-call* context function (cdr arguments) argument-count continuation))))

(define (compile-call context expression continuation)
  (let* (
      (function (car expression))
      (arguments (cdr expression))
      (argument-count (length arguments))
      (continuation (compile-call* context function arguments argument-count continuation)))
    (if (symbol? function)
      continuation
      (compile-expression context function continuation))))

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
          ((eqv? first 'begin)
            (compile-begin context (cdr expression) continuation))

          ((eqv? first 'if)
            (compile-expression
              context
              (cadr expression)
              (rib if-instruction
                ; TODO Join continuation with a closure.
                (compile-expression context (caddr expression) continuation)
                (compile-expression context (cadddr expression) continuation))))

          ((eqv? first 'lambda)
            (let ((parameters (cadr expression)))
              (compile-constant
                (make-procedure
                  (rib
                    pair-type
                    (length parameters)
                    (compile-begin
                      (compile-context-environment-append
                        context
                        ; #f is for a frame.
                        (reverse (cons #f parameters)))
                      (cddr expression)
                      '()))
                  '())
                (compile-primitive-call 'close 1 continuation))))

          ((eqv? first 'quote)
            (compile-constant (cadr expression) continuation))

          ((eqv? first 'set!)
            (compile-expression context (caddr expression)
              (compile-set
                (compile-context-resolve context (cadr expression) 1)
                continuation)))

          (else
            (compile-call context expression continuation)))))

    (else
      (compile-constant expression continuation))))

(define (compile expression)
  (compile-expression
    (make-compile-context)
    expression
    (compile-constant
      #f
      (compile-primitive-call 'id '()))))

; Encoding

;; Context

(define (make-encode-context symbols)
  (cons symbols '()))

(define (encode-context-symbols context)
  (car context))

(define (encode-context-constants context)
  (cdr context))

(define (encode-context-constants-add! context constant symbol)
  (set-cdr!
    context
    (cons
      (constant symbol)
      (encode-context-constants context))))

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
          (not (memq operand reserved-symbols))
          (not (memq operand rest)))
        (cons operand rest)
        rest))))

;; Constants

(define (encode-constant context constant continuation)
  (let ((resolved (encode-context-constant context constant)))
    (if resolved
      (rib get-instruction resolved continuation)
      (cond
        ((or (boolean? constant) (null? constant) (symbol? constant))
          (rib constant-instruction constant continuation))

        ((number? constant)
          (if (< constant 0)
            (rib constant-instruction
              0
              (rib constant-instruction
                (abs constant)
                (compile-primitive-call '- continuation)))
            (rib constant-instruction constant continuation)))

        ((pair? constant)
          (build-constant (car constant)
            (build-constant (cdr constant)
              (rib constant-instruction
                pair-type
                (compile-primitive-call 'rib continuation)))))

        (else
          (error "invalid constant" constant))))))

(define (encode-constants context codes)
  (let (
      (instruction (rib-tag codes))
      (operand (rib-car codes))
      (continuation (encode-constants context (cdr codes))))
    (cond
      ((eqv? instruction constant-instruction)
        (encode-constant context operand continuation))

      ((eqv? instruction if-instruction)
        (rib if-instruction (encode-constants context operand) continuation))

      (else
        (rib instruction operand continuation)))))

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
        (cons (i8->u8 (* (if first 1 -1) part)) target)))))

(define (encode-integer integer target)
  (encode-integer-rest integer #t target))

(define (encode-operand context operand target)
  (encode-integer
    (if (number? operand)
      (+ (* operand 2) 1)
      (* 2
        (cond
          ((boolean? operand)
            (if operand true-index false-index))

          ((null? operand)
            null-index)

          ((eqv? operand 'rib)
            rib-index)

          ((symbol? operand)
            (+ other-index (member-index operand (encode-context-symbols context))))

          (else (error "invalid operand" operand)))))
    target))

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
            (encode-codes
              context
              operand
              (cons if-code target)))

          (else (error "invalid instruction")))))))

(define (encode codes)
  (let ((context (make-encode-context (find-symbols codes))))
    (encode-symbols
      (encode-context-symbols context)
      (encode-codes context codes '()))))
