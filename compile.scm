#!/usr/bin/env gsi

; Stak compiler based on Ribbit's
;
; All compiler-generated variables are prefixed with `$`.

; Constants

(define default-constants
  '(
    (#f $false)
    (#t $true)
    (() $null)))

(define rib-symbol 'rib)

; Instructions

(define call-instruction 0)
(define set-instruction 1)
(define get-instruction 2)
(define constant-instruction 3)
(define if-instruction 4)
; Only for encoding
(define closure-instruction 5)
(define skip-instruction 6)

; Primitives

(define primitives
  '(
    (cons 1)
    (skip 2)
    (close 3)
    (- 14)))

; Types

(define pair-type 0)
(define procedure-type 1)
(define string-type 3)
(define char-type 4)
(define vector-type 5)
(define bytevector-type 6)

; Utility

(cond-expand
  ((or gambit gauche)
    (define (rib tag car cdr)
      (cons (cons (cons '$rib tag) car) cdr))

    (define (rib-cons car cdr)
      (cons (cons (cons '$rib 0) car) cdr))

    (define rib-tag cdaar)
    (define rib-car cdar)
    (define rib-cdr cdr)

    (define (rib-set-car! rib car)
      (set-cdr! (car rib) car))

    (define (rib-set-cdr! rib cdr)
      (set-cdr! rib cdr))

    (define (rib? value)
      (and
        (pair? value)
        (pair? (car value))
        (pair? (caar value))
        (eqv? (caaar value) '$rib))))

  (else))

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

(define (procedure-code procedure)
  (rib-cdr (rib-car procedure)))

(define (bytevector->list vector)
  (let loop ((index 0) (result '()))
    (if (< index (bytevector-length vector))
      (cons
        (bytevector-u8-ref vector index)
        (loop (+ 1 index) result))
      result)))

(define (last-cdr list)
  (if (pair? list)
    (last-cdr (cdr list))
    list))

(define (predicate expression)
  (and (pair? expression) (car expression)))

(define (count-parameters parameters)
  (if (pair? parameters)
    (+ 1 (count-parameters (cdr parameters)))
    0))

(define (get-parameter-variables parameters)
  (cond
    ((pair? parameters)
      (cons (car parameters) (get-parameter-variables (cdr parameters))))

    ((symbol? parameters)
      (list parameters))

    ((null? parameters)
      '())

    (else
      (error "invalid variadic parameter:" parameters))))

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

; Expansion

;; Context

; TODO Rename expanders meta-environment?
; '(local-expanders global-expanders environment)
(define (make-expansion-context)
  '(() () ()))

(define (expansion-context-expanders context)
  (append (car context) (cadr context)))

(define expansion-context-environment caddr)

(define (expansion-context-add-local-expander context name procedure)
  (cons
    (cons
      (cons name procedure)
      (car context))
    (cdr context)))

(define (expansion-context-add-global-expander! context name procedure)
  (set-car!
    (cdr context)
    (cons
      (cons name procedure)
      (cadr context))))

(define (expansion-context-add-variables context variables)
  (list
    (car context)
    (cadr context)
    (append
      variables
      (caddr context))))

;; Procedures

(define (expand-transformer context name transformer)
  ; TODO
  (lambda (expression)
    (if (and (pair? expression) (eqv? (car expression) 'id))
      (cadr expression)
      expression)))

(define (expand-syntax* expanders names expression)
  (if (null? expanders)
    expression
    (let* (
        (pair (car expanders))
        (name (car pair)))
      (expand-syntax*
        (cdr expanders)
        (cons name names)
        (if (assoc name names)
          expression
          ((cdr pair) expression))))))

(define (expand-syntax context expression)
  (expand-syntax* (expansion-context-expanders context) '() expression))

(define (expand-definition definition)
  (let (
      (pattern (cadr definition))
      (body (cddr definition)))
    (if (symbol? pattern)
      (cons pattern body)
      (list
        (car pattern)
        (cons 'lambda (cons (cdr pattern) body))))))

(define (expand-syntax-body context expressions)
  (let loop ((expressions expressions) (definitions '()))
    (when (null? expressions)
      (error "empty expression sequence"))
    (let ((expression (car expressions)))
      (if (eqv? (predicate expression) 'define-syntax)
        (loop
          (cdr expressions)
          (cons (cdr expression) definitions))
        (list
          (expand-expression
            context
            (cons 'letrec-syntax (cons (reverse definitions) expressions))))))))

(define (expand-body context expressions)
  (let loop ((expressions expressions) (definitions '()))
    (when (null? expressions)
      (error "empty expression sequence"))
    (let* (
        (expression (car expressions))
        (predicate (predicate expression)))
      (cond
        ((eqv? predicate 'define)
          (loop
            (cdr expressions)
            (cons (expand-definition expression) definitions)))

        ((eqv? predicate 'define-syntax)
          (loop
            (list (expand-syntax-body context expressions))
            definitions))

        ((pair? definitions)
          (list
            (expand-expression
              context
              (cons 'letrec (cons (reverse definitions) expressions)))))

        (else
          (expand-sequence context expressions))))))

(define (expand-sequence context expressions)
  (when (null? expressions)
    (error "empty expression sequence"))
  (map
    (lambda (expression) (expand-expression context expression))
    expressions))

(define (expand-expression context expression)
  (let (
      (expand (lambda (expression) (expand-expression context expression)))
      (expression (expand-syntax context expression)))
    (if (pair? expression)
      (let ((first (car expression)))
        (cond
          ((eqv? first 'and)
            (expand
              (cond
                ((null? (cdr expression))
                  #t)

                ((null? (cddr expression))
                  (cadr expression))

                (else
                  (list 'if (cadr expression)
                    (cons 'and (cddr expression))
                    #f)))))

          ((eqv? first 'begin)
            (cons 'begin (expand-sequence context (cdr expression))))

          ((eqv? first 'define)
            (expand (cons 'set! (expand-definition expression))))

          ((eqv? first 'define-syntax)
            (let ((name (cadr expression)))
              (expansion-context-add-global-expander!
                context
                name
                (expand-transformer context name expression))
              #f))

          ((eqv? first 'if)
            (list
              'if
              (expand (cadr expression))
              (expand (caddr expression))
              (if (pair? (cdddr expression))
                (expand (cadddr expression))
                #f)))

          ((eqv? first 'lambda)
            (cons
              'lambda
              (cons
                (cadr expression)
                (expand-body
                  (expansion-context-add-variables
                    context
                    (get-parameter-variables (cadr expression)))
                  (cddr expression)))))

          ((eqv? first 'let)
            (let ((bindings (cadr expression)))
              (cons
                'let
                (cons
                  (map
                    (lambda (binding)
                      (list (car binding) (expand (cadr binding))))
                    bindings)
                  (expand-body context (cddr expression))))))

          ((eqv? first 'letrec)
            (let ((bindings (cadr expression)))
              (expand
                (cons 'let
                  (cons
                    (map
                      (lambda (binding) (list (car binding) #f))
                      bindings)
                    (append
                      (map
                        (lambda (binding) (list 'set! (car binding) (cadr binding)))
                        bindings)
                      (cddr expression)))))))

          ((eqv? first 'let-syntax)
            (error "not implemented"))

          ((eqv? first 'letrec-syntax)
            (error "not implemented"))

          ((eqv? first 'or)
            (expand
              (cond
                ((null? (cdr expression))
                  #f)

                ((null? (cddr expression))
                  (cadr expression))

                (else
                  (list 'let
                    (list (list '$x (cadr expression)))
                    (list 'if '$x
                      '$x
                      (cons 'or (cddr expression))))))))

          ((eqv? first 'quote)
            expression)

          ((pair? first)
            (expand
              (list 'let
                (list (list '$x first))
                (cons '$x (cdr expression)))))

          (else
            (map expand expression))))
      expression)))

(define (expand expression)
  (expand-expression (make-expansion-context) expression))

; Compilation

;; Context

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

(define (compile-context-resolve context variable)
  (or (member-index variable (compile-context-environment context)) variable))

;; Codes

(define (compile-constant constant continuation)
  (rib constant-instruction constant continuation))

(define (compile-primitive-call name continuation)
  (rib
    call-instruction
    (rib-cons
      (cond
        ((memq name '(close))
          1)

        ((memq name '(cons skip -))
          2)

        ((memq name '(rib))
          3)

        (else
          (error "unknown primitive:" name)))
      name)
    continuation))

(define (drop? codes)
  (and
    (rib? codes)
    (eqv? (rib-tag codes) set-instruction)
    (eqv? (rib-car codes) 0)))

(define (compile-unspecified continuation)
  (if (drop? continuation)
    ; Skip a "drop" instruction.
    (rib-cdr continuation)
    (compile-constant #f continuation)))

(define (compile-drop continuation)
  (if (null? continuation)
    continuation
    (rib set-instruction 0 continuation)))

(define (compile-sequence context expressions continuation)
  (compile-expression
    context
    (car expressions)
    (if (null? (cdr expressions))
      continuation
      (compile-drop (compile-sequence context (cdr expressions) continuation)))))

(define (compile-call* context function arguments argument-count continuation)
  (if (null? arguments)
    (rib
      call-instruction
      (rib-cons
        argument-count
        (compile-context-resolve context function))
      continuation)
    (compile-expression
      context
      (car arguments)
      (compile-call*
        (compile-context-environment-add-temporary context)
        function
        (cdr arguments)
        argument-count
        continuation))))

; Functions are normalized into atoms already.
(define (compile-call context expression continuation)
  (let ((arguments (cdr expression)))
    (compile-call*
      context
      (car expression)
      arguments
      (length arguments)
      continuation)))

(define (compile-unbind continuation)
  (if (null? continuation)
    continuation
    (compile-primitive-call 'skip continuation)))

(define (compile-let* context bindings body-context body continuation)
  (if (pair? bindings)
    (let ((binding (car bindings)))
      (compile-expression
        context
        (cadr binding)
        (compile-let*
          (compile-context-environment-add-temporary context)
          (cdr bindings)
          (compile-context-environment-append body-context (list (car binding)))
          body
          (compile-unbind continuation))))
    (compile-sequence body-context body continuation)))

(define (compile-let context bindings body continuation)
  (compile-let* context bindings context body continuation))

(define (compile-expression context expression continuation)
  (cond
    ((symbol? expression)
      (rib
        get-instruction
        (compile-context-resolve context expression)
        continuation))

    ((pair? expression)
      (let ((first (car expression)))
        (cond
          ((eqv? first 'begin)
            (compile-sequence context (cdr expression) continuation))

          ((eqv? first 'if)
            (compile-expression
              context
              (cadr expression)
              (rib if-instruction
                (compile-expression context (caddr expression) continuation)
                (compile-expression context (cadddr expression) continuation))))

          ((eqv? first 'lambda)
            (let ((parameters (cadr expression)))
              (compile-constant
                (make-procedure
                  (rib
                    pair-type
                    (+
                      (* 2 (count-parameters parameters))
                      (if (symbol? (last-cdr parameters)) 1 0))
                    (compile-sequence
                      (compile-context-environment-append
                        context
                        ; #f is for a frame.
                        (reverse (cons #f (get-parameter-variables parameters))))
                      (cddr expression)
                      '()))
                  '())
                (compile-primitive-call 'close continuation))))

          ((eqv? first 'let)
            (compile-let
              context
              (cadr expression)
              (cddr expression)
              continuation))

          ((eqv? first 'quote)
            (compile-constant (cadr expression) continuation))

          ((eqv? first 'set!)
            (compile-expression
              context
              (caddr expression)
              (rib
                set-instruction
                (compile-context-resolve
                  (compile-context-environment-add-temporary context)
                  (cadr expression))
                (compile-unspecified continuation))))

          (else
            (compile-call context expression continuation)))))

    (else
      (compile-constant expression continuation))))

(define (compile expression)
  (compile-expression
    (make-compile-context)
    expression
    '()))

; Encoding

;; Utility

(define (find-symbols* codes symbols)
  (if (null? codes)
    symbols
    (let* (
        (instruction (rib-tag codes))
        (operand (rib-car codes))
        (operand
          (if (eqv? instruction call-instruction)
            (rib-cdr operand)
            operand)))
      (find-symbols*
        (rib-cdr codes)
        (cond
          ((eqv? instruction if-instruction)
            (find-symbols* operand symbols))

          ((and
              (symbol? operand)
              (not (eqv? operand rib-symbol))
              (not (memq operand symbols)))
            (cons operand symbols))

          (else
            symbols))))))

(define (find-symbols codes)
  (find-symbols* codes '()))

(define (reverse-codes codes)
  (let loop ((codes codes) (result '()))
    (if (null? codes)
      result
      (loop (rib-cdr codes) (cons codes result)))))

(define (find-continuation left right)
  (let loop (
      (left (reverse-codes left))
      (right (reverse-codes right))
      (result '()))
    (if (and
        (pair? left)
        (pair? right)
        (eq? (car left) (car right)))
      (loop (cdr left) (cdr right) (car left))
      result)))

(define (count-skips codes continuation)
  (let loop ((codes codes) (count 0))
    (if (eq? codes continuation)
      count
      (loop (rib-cdr codes) (+ 1 count)))))

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
      "$c"
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

(define (build-constant-rib context car cdr tag continuation)
  (let (
      (build-child
        (lambda (constant continuation)
          (build-constant
            context
            constant
            (build-constant-codes
              context
              constant
              continuation)))))
    (build-child
      car
      (build-child
        cdr
        (rib constant-instruction
          tag
          (compile-primitive-call 'rib continuation))))))

(define (build-constant-codes context constant continuation)
  (let (
      (symbol (encode-context-constant context constant))
      (build-constant-rib
        (lambda (car cdr tag)
          (build-constant-rib context car cdr tag continuation))))
    (if symbol
      (rib get-instruction symbol continuation)
      (cond
        ((constant-normal? constant)
          (rib constant-instruction constant continuation))

        ((bytevector? constant)
          (build-constant-rib
            (bytevector-length constant)
            (bytevector->list constant)
            bytevector-type))

        ((char? constant)
          (build-constant-rib (char->integer constant) '() char-type))

        ; Negative number
        ((number? constant)
          (rib constant-instruction
            0
            (rib constant-instruction
              (abs constant)
              (compile-primitive-call '- continuation))))

        ((pair? constant)
          (build-constant-rib (car constant) (cdr constant) pair-type))

        ((string? constant)
          (build-constant-rib
            (string-length constant)
            (map char->integer (string->list constant))
            string-type))

        ((vector? constant)
          (build-constant-rib
            (vector-length constant)
            (vector->list constant)
            vector-type))

        (else
          (error "invalid constant:" constant))))))

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
      (encode-context-add-constant! context constant id)
      continuation)))

(define (build-constants* context codes continuation)
  (if (null? codes)
    continuation
    (let (
        (instruction (rib-tag codes))
        (operand (rib-car codes)))
      (build-constants*
        context
        (rib-cdr codes)
        (cond
          ((eqv? instruction constant-instruction)
            (let ((continuation (build-constant context operand continuation)))
              (if (procedure? operand)
                (build-constants* context (procedure-code operand) continuation)
                continuation)))

          ((eqv? instruction if-instruction)
            (build-constants* context operand continuation))

          (else
            continuation))))))

(define (build-constants context codes)
  (build-constants* context codes '()))

;; Symbols

(define (encode-string string target)
  (if (null? string)
    target
    (encode-string (cdr string) (cons (char->integer (car string)) target))))

(define (encode-symbol symbol target)
  (encode-string (string->list (symbol->string symbol)) target))

; TODO Check if a symbol can be empty.
; Currently, we encode the last 3 symbols as empty symbols just to test this logic.
(define (empty-symbol? symbols)
  (< (length symbols) 4))

(define (count-empty-symbols* symbols count)
  (if (null? symbols)
    count
    (count-empty-symbols*
      (cdr symbols)
      (if (empty-symbol? symbols)
        (+ count 1)
        0))))

(define (count-empty-symbols symbols)
  (count-empty-symbols* symbols 0))

(define (encode-symbols* symbols count target)
  ; We may encounter this only at the first call.
  (if (eqv? count 0)
    target
    (let ((target (encode-symbol (car symbols) target)))
      (if (eqv? count 1)
        target
        (encode-symbols*
          (cdr symbols)
          (- count 1)
          (cons (char->integer #\,) target))))))

; TODO Should we put all empty symbols at the end?
(define (encode-symbols symbols target)
  (let (
      (count (count-empty-symbols symbols))
      (target (cons (char->integer #\;) target)))
    (encode-integer
      count
      (if (null? symbols)
        target
        (encode-symbols* symbols (- (length symbols) count) target)))))

;; Codes

(define integer-base 128)
(define short-integer-base 8)

(define (encode-integer-part integer base bit)
  (+ bit (* 2 (modulo integer base))))

(define (encode-integer-with-base integer base target)
  (let loop (
      (x (quotient integer base))
      (bit 0)
      (target target))
    (if (eqv? x 0)
      (values (encode-integer-part integer base bit) target)
      (loop
        (quotient x integer-base)
        1
        (cons (encode-integer-part x integer-base bit) target)))))

(define (encode-short-integer integer target)
  (encode-integer-with-base integer short-integer-base target))

(define (encode-integer integer target)
  (let-values (((byte target) (encode-integer-with-base integer integer-base target)))
    (cons byte target)))

(define (encode-instruction instruction integer return target)
  (let-values (((integer target) (encode-short-integer integer target)))
    (cons (+ (if return 1 0) (* 2 instruction) (* 16 integer)) target)))

(define (encode-procedure context procedure return target)
  (let ((code (rib-car procedure)))
    (encode-codes
      context
      (rib-cdr code)
      '()
      (encode-instruction
        closure-instruction
        (rib-car code)
        return
        target))))

(define (encode-operand context operand)
  (cond
    ((number? operand)
      (+ (* operand 2) 1))

    ((symbol? operand)
      (* 2
        (or
          (member-index operand (encode-context-all-symbols context))
          (error "symbol not found:" operand))))

    (else (error "invalid operand:" operand))))

(define (encode-codes context codes terminal target)
  (if (eq? codes terminal)
    target
    (let* (
        (instruction (rib-tag codes))
        (operand (rib-car codes))
        (rest (rib-cdr codes))
        (return (null? rest))
        (encode-simple
          (lambda (instruction)
            (encode-instruction
              instruction
              (encode-operand context operand)
              return
              target))))
      (encode-codes
        context
        rest
        terminal
        (cond
          ((memv instruction (list set-instruction get-instruction))
            (encode-simple instruction))

          ((eqv? instruction call-instruction)
            (encode-instruction
              instruction
              (rib-car operand)
              return
              (encode-integer (encode-operand context (rib-cdr operand)) target)))

          ((and
              (eqv? instruction constant-instruction)
              (procedure? operand))
            (encode-procedure context operand return target))

          ((eqv? instruction constant-instruction)
            (let ((symbol (encode-context-constant context operand)))
              (if symbol
                (encode-instruction
                  get-instruction
                  (encode-operand context symbol)
                  return
                  target)
                (encode-simple constant-instruction))))

          ((eqv? instruction if-instruction)
            (let* (
                (continuation (find-continuation operand rest))
                (target
                  (encode-codes
                    context
                    operand
                    continuation
                    (encode-instruction if-instruction 0 #f target))))
              (if (null? continuation)
                target
                (encode-instruction skip-instruction (count-skips rest continuation) #t target))))

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

(define (join-codes! ones others)
  (if (null? ones)
    others
    (begin
      (if (eqv? (rib-tag ones) if-instruction)
        (rib-set-car! ones (join-codes! (rib-car ones) others)))
      (rib-set-cdr! ones (join-codes! (rib-cdr ones) others))
      ones)))

;; Main

(define (encode codes)
  (let* (
      (context
        (make-encode-context
          (append
            (map car primitives)
            (find-symbols codes))))
      (constant-codes (build-constants context codes))
      (primitive-codes (build-primitives primitives))
      (codes (join-codes! primitive-codes (join-codes! constant-codes codes))))
    (encode-symbols
      (encode-context-symbols context)
      (encode-codes context codes '() '()))))

; Main

(write-target (encode (compile (expand (read-source)))))
