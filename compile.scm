(define integer-base 128)

; Constants

(define default-constants
  '(
    (#f _false)
    (#t _true)
    (() _null)))

(define rib-symbol 'rib)

; Instructions

(define call-instruction 0)
(define set-instruction 1)
(define get-instruction 2)
(define constant-instruction 3)
(define if-instruction 4)

;; Codes

(define return-call-code 0)
(define call-code 1)
(define closure-code 2)
(define set-code 3)
(define get-code 4)
(define constant-code 5)
(define if-code 6)

; Primitives

(define primitives
  '(
    (id 2)
    (close 5)
    (- 14)))

; Types

(define pair-type 0)
(define procedure-type 1)

; Utility

(cond-expand
  (gambit
    (define (rib tag car cdr)
      (cons (cons (cons '_rib tag) car) cdr))

    (define (rib-tag rib)
      (cdaar rib))

    (define (rib-car rib)
      (cdar rib))

    (define (rib-cdr rib)
      (cdr rib))

    (define (rib? value)
      (and
        (pair? value)
        (pair? (car value))
        (pair? (caar value))
        (eq? (caaar value) '_rib))))

  (else))

(define (todo value) (error "not implemented" value))

(define (i8->u8 value)
  (if (< value 0)
    (+ 256 value)
    value))

(define (member-index value list)
  (cond
    ((null? list)
      #f)

    ((eqv? value (car list))
      0)

    (else
      (let ((index (member-index value (cdr list))))
        (and index (+ 1 index))))))

(define (make-procedure code environment)
  (rib procedure-type code environment))

(define (procedure? value)
  (and (rib? value) (eqv? (rib-tag value) procedure-type)))

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

(define (compile-context-environment-add-temporary context)
  (compile-context-environment-append context (list #f)))

(define (compile-context-resolve* variables variable index)
  (cond
    ((null? variables)
      variable)

    ((eqv? (car variables) variable)
      index)

    (else
      (compile-context-resolve (cdr variables) variable (+ index 1)))))

(define (compile-context-resolve context variable base)
  (let ((index (member-index variable (compile-context-environment context))))
    (if index
      (+ index base)
      variable)))

; Compilation

(define (compile-constant constant continuation)
  (rib constant-instruction constant continuation))

(define (compile-primitive-call name continuation)
  (compile-constant
    (cond
      ((memq name '(close id))
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
      (compile-call*
        (compile-context-environment-add-temporary context)
        function
        (cdr arguments)
        argument-count
        continuation))))

(define (compile-call context expression continuation)
  (let* (
      (function (car expression))
      (arguments (cdr expression))
      (argument-count (length arguments))
      (continuation (lambda (context) (compile-call* context function arguments argument-count continuation))))
    (if (symbol? function)
      (continuation context)
      (compile-expression
        context
        function
        (continuation (compile-context-environment-add-temporary context))))))

; TODO Introduce return-flavoured instructions for all `call`, `constant`, and `get`.
(define (compile-tail continuation)
  (if (null? continuation)
    (compile-primitive-call 'id '())
    continuation))

(define (compile-expression context expression continuation)
  (cond
    ((symbol? expression)
      (rib
        get-instruction
        (compile-context-resolve context expression 0)
        (compile-tail continuation)))

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
                (compile-primitive-call 'close continuation))))

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
      (compile-constant expression (compile-tail continuation)))))

(define (compile expression)
  (compile-expression
    (make-compile-context)
    expression
    (compile-constant
      #f
      (compile-primitive-call 'id '()))))

; Encoding

;; Utility

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
          (not (eq? operand rib-symbol))
          (not (memq operand rest)))
        (cons operand rest)
        rest))))

;; Context

(define (make-encode-context symbols)
  (cons symbols '()))

(define (encode-context-symbols context)
  (car context))

(define (encode-context-all-symbols context)
  (append
    (map cadr default-constants)
    (list rib-symbol)
    (encode-context-symbols context)))

(define (encode-context-constants context)
  (cdr context))

(define (encode-context-constant context constant)
  (let ((pair (assq constant (append default-constants (encode-context-constants context)))))
    (if pair (cadr pair) #f)))

(define (encode-context-constant-id context)
  (string->symbol
    (string-append
      "_"
      (number->string (length (encode-context-constants context))))))

(define (encode-context-add-constant! context constant symbol)
  (begin
    (set-car!
      context
      (cons symbol (encode-context-symbols context)))
    (set-cdr!
      context
      (cons (list constant symbol) (encode-context-constants context)))))

;; Constants

(define (constant-normal? constant)
  (or
    (symbol? constant)
    (and (number? constant) (>= constant 0))
    (procedure? constant)))

(define (build-constant-codes context constant continuation)
  (let ((symbol (encode-context-constant context constant)))
    (if symbol
      (rib get-instruction symbol continuation)
      (cond
        ((constant-normal? constant)
          (rib constant-instruction constant continuation))

        ; Negative number
        ((number? constant)
          (rib constant-instruction
            0
            (rib constant-instruction
              (abs constant)
              (compile-primitive-call '- continuation))))

        ((pair? constant)
          (build-constant-codes*
            context
            (car constant)
            (build-constant-codes*
              context
              (cdr constant)
              (rib constant-instruction
                pair-type
                (compile-primitive-call 'rib continuation)))))

        (else
          (error "invalid constant" constant))))))

(define (build-constant-codes* context constant continuation)
  (build-constant
    context
    constant
    (build-constant-codes
      context
      constant
      continuation)))

(define (build-constant context constant continuation)
  (if (or (constant-normal? constant) (encode-context-constant context constant))
    continuation
    (let* (
        (id (encode-context-constant-id context))
        (continuation
          (build-constant-codes
            context
            constant
            (rib set-instruction id continuation))))
      (begin
        (encode-context-add-constant! context constant id)
        continuation))))

(define (build-constants* context codes continuation)
  (if (null? codes)
    continuation
    (let (
        (instruction (rib-tag codes))
        (operand (rib-car codes))
        (continuation (build-constants* context (rib-cdr codes) continuation)))
      (cond
        ((eqv? instruction constant-instruction)
          (build-constant context operand continuation))

        ((eqv? instruction if-instruction)
          (build-constants* context operand continuation))

        (else
          continuation)))))

(define (build-constants context codes)
  (build-constants* context codes '()))

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

(define (encode-integer-rest integer target)
  (let ((part (modulo integer integer-base)))
    (if (eqv? part 0)
      target
      (encode-integer-rest
        (quotient integer integer-base)
        (cons (i8->u8 (* -1 part)) target)))))

(define (encode-integer integer target)
  (encode-integer-rest
    (quotient integer integer-base)
    (cons (modulo integer integer-base) target)))

(define (encode-procedure context procedure target)
  (let ((code (rib-car procedure)))
    (encode-codes
      context
      (rib-cdr code)
      (cons closure-code
        (encode-integer (rib-car code) target)))))

(define (encode-operand context operand target)
  (encode-integer
    (cond
      ((number? operand)
        (+ (* operand 2) 1))

      ((symbol? operand)
        (* 2
          (or
            (member-index operand (encode-context-all-symbols context))
            (error "symbol not found" value))))

      (else (error "invalid operand" operand)))
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

          ((and
              (eqv? instruction constant-instruction)
              (procedure? operand))
            (encode-procedure context operand target))

          ((eqv? instruction constant-instruction)
            (let ((symbol (encode-context-constant context operand)))
              (if symbol
                (cons get-code (encode-operand context symbol target))
                (cons constant-code (encode-operand context operand target)))))

          ((eqv? instruction if-instruction)
            (encode-codes
              context
              operand
              (cons if-code target)))

          (else (error "invalid instruction")))))))

;; Primitives

(define (build-primitive primitive continuation)
  (rib constant-instruction
    (cadr primitive)
    (rib constant-instruction
      '()
      (rib constant-instruction
        procedure-type
        (compile-primitive-call
          'rib
          (rib set-instruction (car primitive) continuation))))))

(define (build-primitives* primitives continuation)
  (if (null? primitives)
    continuation
    (build-primitive
      (car primitives)
      (build-primitives* (cdr primitives) continuation))))

(define (build-primitives primitives)
  (build-primitives* primitives '()))

;; Main

(define (encode codes)
  (let* (
      (context
        (make-encode-context
          (append
            (map car primitives)
            (find-symbols codes))))
      (constant-codes (build-constants context codes)))
    (encode-symbols
      (encode-context-symbols context)
      (encode-codes
        context
        codes
        (encode-codes
          context
          constant-codes
          (encode-codes
            context
            (build-primitives primitives)
            '()))))))
