#!/usr/bin/env gsi

; Stak compiler based on Ribbit's
;
; All compiler-generated variables are prefixed with `$`.

; Compatibility

(cond-expand
  ((or gambit gauche)
    (import (scheme base) (scheme cxr))

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

; Constants

(define default-constants
  '(
    (#f . $false)
    (#t . $true)
    (() . $null)))

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
    (close 2)
    ($- 13)))

; Types

(define pair-type 0)
(define procedure-type 1)
(define string-type 3)
(define char-type 4)
(define vector-type 5)
(define bytevector-type 6)

; Utility

(define (make-procedure code environment)
  (rib procedure-type code environment))

(define (stak-procedure? value)
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

(define (filter f xs)
  (if (null? xs)
    '()
    (let (
        (x (car xs))
        (xs (filter f (cdr xs))))
      (if (f x)
        (cons x xs)
        xs))))

(define (fold-left f y xs)
  (if (null? xs)
    y
    (fold-left
      f
      (f y (car xs))
      (cdr xs))))

(define (fold-right f y xs)
  (if (null? xs)
    y
    (f
      (fold-right f y (cdr xs))
      (car xs))))

(define (take n list)
  (if (eqv? n 0)
    '()
    (cons
      (car list)
      (take (- n 1) (cdr list)))))

(define (skip n list)
  (if (eqv? n 0)
    list
    (skip (- n 1) (cdr list))))

(define (list-position f xs)
  (let loop ((xs xs) (index 0))
    (cond
      ((null? xs)
        #f)

      ((f (car xs))
        index)

      (else
        (loop (cdr xs) (+ index 1))))))

(define (memv-position one xs)
  (list-position (lambda (other) (eqv? one other)) xs))

(define (list-count f xs)
  (let loop ((xs xs) (count 0))
    (if (null? xs)
      count
      (loop (cdr xs) (+ count (if (f (car xs)) 1 0))))))

(define (zip-alist alist)
  (let (
      (pairs
        (map
          (lambda (pair)
            (let (
                (key (car pair))
                (value (cdr pair)))
              (if (pair? value)
                (cons
                  (cons key (car value))
                  (cons key (cdr value)))
                #f)))
          alist)))
    (if (memv #f pairs)
      '()
      (cons
        (map car pairs)
        (zip-alist (map cdr pairs))))))

(define (predicate expression)
  (and (pair? expression) (car expression)))

(define (count-parameters parameters)
  (if (pair? parameters)
    (+ 1 (count-parameters (cdr parameters)))
    0))

(define (parameter-names parameters)
  (cond
    ((pair? parameters)
      (cons (car parameters) (parameter-names (cdr parameters))))

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

(define-record-type expansion-context
  (make-expansion-context environment)
  expansion-context?
  (environment expansion-context-environment expansion-context-set-environment!))

(define (expansion-context-append context pairs)
  (make-expansion-context (append pairs (expansion-context-environment context))))

(define (expansion-context-push context name procedure)
  (expansion-context-append context (list (cons name procedure))))

(define (expansion-context-set! context name procedure)
  (let* (
      (environment (expansion-context-environment context))
      (pair (assv name environment)))
    (if pair
      (set-cdr! pair procedure)
      ; This works because we pass a reference to an environment in a context
      ; to macro transformers.
      (expansion-context-set-environment!
        context
        (cons (cons name procedure) environment)))))

(define (expansion-context-resolve context expression)
  (let ((pair (assv expression (expansion-context-environment context))))
    (if pair
      (cdr pair)
      expression)))

;; Procedures

(define primitive-functions
  '(
    (+ . $+)
    (- . $-)
    (* . $*)
    (/ . $/)
    (< . $<)))

; TODO Check if those primitive functions are from the `scheme base` library
; before applying optimization.
(define (optimize expression)
  (let ((pair (assv (predicate expression) primitive-functions)))
    (if (and pair (eqv? (length expression) 3))
      (cons (cdr pair) (cdr expression))
      expression)))

(define (denotation-equal? context one other)
  (eqv?
    (expansion-context-resolve context one)
    (expansion-context-resolve context other)))

(define (denote-parameter context name)
  (let (
      (count
        (list-count
          (lambda (pair) (eqv? (car pair) name))
          (expansion-context-environment context)))
      (name (string-append "$" (symbol->string name))))
    (string->symbol
      (if (eqv? count 0)
        name
        (string-append name "$" (number->string count))))))

(define (resolve-parameters context parameters)
  (cond
    ((symbol? parameters)
      (expansion-context-resolve context parameters))

    ((null? parameters)
      '())

    (else
      (cons
        (expansion-context-resolve context (car parameters))
        (resolve-parameters context (cdr parameters))))))

(define (find-pattern-variables literals pattern)
  (cond
    ((memv pattern (append '(_ ...) literals))
      '())

    ((symbol? pattern)
      (list pattern))

    ((list? pattern)
      (fold-left
        append
        '()
        (map
          (lambda (pattern) (find-pattern-variables literals pattern))
          pattern)))

    (else
      '())))

(define (match-ellipsis definition-context use-context name literals pattern expression)
  (fold-right
    (lambda (all ones)
      (and
        all
        ones
        (map
          (lambda (pair)
            (let ((name (car pair)))
              (cons name
                (cons
                  (cdr pair)
                  (cdr (assv name all))))))
          ones)))
    (map
      (lambda (name) (cons name '()))
      (find-pattern-variables literals pattern))
    (map
      (lambda (expression)
        (match-pattern definition-context use-context name literals pattern expression))
      expression)))

; Note that the original `append` function works in this way natively on some Scheme implementations.
(define (merge-matches ones others)
  (if (or (not ones) (not others))
    #f
    (append ones others)))

(define (match-pattern definition-context use-context name literals pattern expression)
  (let (
      (match-pattern
        (lambda (pattern expression)
          (match-pattern definition-context use-context name literals pattern expression))))
    (cond
      ((eqv? pattern '_)
        (if (denotation-equal? use-context expression name) '() #f))

      ((memv pattern literals)
        (if (eqv? expression pattern) '() #f))

      ((symbol? pattern)
        (list (cons pattern expression)))

      ((and (pair? pattern) (list? expression))
        (cond
          ((and
              (pair? (cdr pattern))
              (eqv? (cadr pattern) '...))
            (let ((length (- (length expression) (- (length pattern) 2))))
              (merge-matches
                (match-ellipsis
                  definition-context
                  use-context
                  name
                  literals
                  (car pattern)
                  (take length expression))
                (match-pattern (cddr pattern) (skip length expression)))))

          ((pair? expression)
            (merge-matches
              (match-pattern (car pattern) (car expression))
              (match-pattern (cdr pattern) (cdr expression))))

          (else
            #f)))

      ((equal? pattern expression)
        '())

      (else
        #f))))

(define (fill-ellipsis-template context matches template)
  (map
    (lambda (matches) (fill-template context matches template))
    (let ((variables (find-pattern-variables '() template)))
      (zip-alist
        (filter
          (lambda (pair) (memv (car pair) variables))
          matches)))))

(define (fill-template context matches template)
  (cond
    ((symbol? template)
      (let ((pair (assv template matches)))
        (if pair
          (cdr pair)
          (expansion-context-resolve context template))))

    ((pair? template)
      (if (and
          (pair? (cdr template))
          (eqv? (cadr template) '...))
        (append
          (fill-ellipsis-template context matches (car template))
          (fill-template context matches (cddr template)))
        (cons
          (fill-template context matches (car template))
          (fill-template context matches (cdr template)))))

    (else
      template)))

(define (make-transformer definition-context name transformer)
  (unless (eqv? (predicate transformer) 'syntax-rules)
    (error "unsupported macro transformer"))
  (let (
      (literals (cons name (cadr transformer)))
      (rules (cddr transformer)))
    (lambda (use-context expression)
      (when (eqv? expression name) (error "macro used as value:" expression))
      (let loop ((rules rules))
        (unless (pair? rules)
          (error "no syntax rule matched" expression))
        (let* (
            (rule (car rules))
            (matches (match-pattern definition-context use-context name literals (car rule) expression)))
          (if matches
            (expand-expression use-context (fill-template definition-context matches (cadr rule)))
            (loop (cdr rules))))))))

(define (expand-definition definition)
  (let (
      (pattern (cadr definition))
      (body (cddr definition)))
    (if (symbol? pattern)
      (cons pattern body)
      (list
        (car pattern)
        (cons 'lambda (cons (cdr pattern) body))))))

(define (expand-quasiquote expression)
  (cond
    ((not (pair? expression))
      (list 'quote expression))

    ((eqv? (car expression) 'unquote)
      (cadr expression))

    ((and
        (pair? (car expression))
        (eqv? (caar expression) 'unquote-splicing))
      (list
        'append
        (cadar expression)
        (expand-quasiquote (cdr expression))))

    (else
      (list
        'cons
        (expand-quasiquote (car expression))
        (expand-quasiquote (cdr expression))))))

(define (validate-sequence expressions)
  (when (null? expressions)
    (error "empty expression sequence")))

(define (expand-syntax-body context expressions)
  (let loop ((expressions expressions) (definitions '()))
    (validate-sequence expressions)
    (let* (
        (expression (car expressions))
        (predicate (predicate expression)))
      (cond
        ((eqv? predicate 'define)
          (loop
            (list (expand-body context expressions))
            definitions))

        ((eqv? predicate 'define-syntax)
          (loop
            (cdr expressions)
            (cons (expand-definition expression) definitions)))

        ((pair? definitions)
          (expand-expression
            context
            (list 'letrec-syntax definitions (cons 'begin expressions))))

        (else
          (expand-sequence context expressions))))))

(define (expand-body context expressions)
  (let loop ((expressions expressions) (definitions '()))
    (validate-sequence expressions)
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
          (expand-expression
            context
            (cons 'letrec (cons (reverse definitions) expressions))))

        (else
          (expand-sequence context expressions))))))

(define (expand-sequence context expressions)
  (validate-sequence expressions)
  (cons
    'begin
    (map
      (lambda (expression) (expand-expression context expression))
      expressions)))

(define (expand-expression context expression)
  (define (expand expression)
    (expand-expression context expression))

  (optimize
    (cond
      ((symbol? expression)
        (expansion-context-resolve context expression))

      ((pair? expression)
        (let ((first (car expression)))
          (cond
            ((eqv? first 'begin)
              (expand-sequence context (cdr expression)))

            ((eqv? first 'define)
              (let* (
                  (pair (expand-definition expression))
                  (name (car pair)))
                (expansion-context-set! context name name)
                (expand (cons 'set! pair))))

            ((eqv? first 'define-syntax)
              (let ((name (cadr expression)))
                (expansion-context-set!
                  context
                  name
                  (make-transformer context name (caddr expression)))
                #f))

            ((eqv? first 'if)
              (list
                'if
                (expand (cadr expression))
                (expand (caddr expression))
                (if (pair? (cdddr expression))
                  (expand (cadddr expression))
                  #f)))

            ; TODO Implement an import statement.
            ((eqv? first 'import)
              #f)

            ((eqv? first 'lambda)
              (let (
                  (context
                    (expansion-context-append
                      context
                      (map
                        (lambda (name) (cons name (denote-parameter context name)))
                        (parameter-names (cadr expression))))))
                (list
                  'lambda
                  (resolve-parameters context (cadr expression))
                  (expand-body context (cddr expression)))))

            ((eqv? first 'let-syntax)
              (expand-expression
                (fold-left
                  (lambda (context pair)
                    (let ((name (car pair)))
                      (expansion-context-push
                        context
                        name
                        (make-transformer context name (cadr pair)))))
                  context
                  (cadr expression))
                (caddr expression)))

            ((eqv? first 'letrec-syntax)
              (let* (
                  (bindings (cadr expression))
                  (context
                    (fold-left
                      (lambda (context pair)
                        (expansion-context-push context (car pair) #f))
                      context
                      bindings)))
                (for-each
                  (lambda (pair)
                    (let ((name (car pair)))
                      (expansion-context-set!
                        context
                        name
                        (make-transformer context name (cadr pair)))))
                  bindings)
                (expand-expression context (caddr expression))))

            ((eqv? first 'quasiquote)
              (expand-quasiquote (cadr expression)))

            ((eqv? first 'quote)
              expression)

            (else
              (let ((expander (expansion-context-resolve context first)))
                (if (procedure? expander)
                  (expand (expander context expression))
                  (map expand expression)))))))

      (else
        expression))))

(define (expand expression)
  (expand-expression (make-expansion-context '()) expression))

; Compilation

;; Context

(define-record-type compilation-context
  (make-compilation-context environment)
  compilation-context?
  (environment compilation-context-environment))

(define (compilation-context-append-locals context variables)
  (make-compilation-context (append variables (compilation-context-environment context))))

(define (compilation-context-push-local context variable)
  (compilation-context-append-locals context (list variable)))

; If a variable is not in environment, it is considered to be global.
(define (compilation-context-resolve context variable)
  (or (memv-position variable (compilation-context-environment context)) variable))

;; Procedures

(define (compile-constant constant continuation)
  (rib constant-instruction constant continuation))

(define (compile-primitive-call name continuation)
  (rib
    call-instruction
    (rib-cons
      (cond
        ((memq name '(close))
          1)

        ((memq name '(cons $-))
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
        (compilation-context-resolve context function))
      continuation)
    (compile-expression
      context
      (car arguments)
      (compile-call*
        (compilation-context-push-local context #f)
        function
        (cdr arguments)
        argument-count
        continuation))))

(define (compile-call context expression continuation)
  (let* (
      (function (car expression))
      (arguments (cdr expression))
      (continue
        (lambda (context function continuation)
          (compile-call* context function arguments (length arguments) continuation))))
    (if (symbol? function)
      (continue context function continuation)
      (compile-expression
        context
        function
        (continue
          (compilation-context-push-local context '$function)
          '$function
          (compile-unbind continuation))))))

(define (compile-unbind continuation)
  (if (null? continuation)
    continuation
    (rib set-instruction 1 continuation)))

(define (compile-expression context expression continuation)
  (cond
    ((symbol? expression)
      (rib
        get-instruction
        (compilation-context-resolve context expression)
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
                      (compilation-context-append-locals
                        context
                        ; #f is for a frame.
                        (reverse (cons #f (parameter-names parameters))))
                      (cddr expression)
                      '()))
                  '())
                (compile-primitive-call 'close continuation))))

          ((eqv? first 'quote)
            (compile-constant (cadr expression) continuation))

          ((eqv? first 'set!)
            (compile-expression
              context
              (caddr expression)
              (rib
                set-instruction
                (compilation-context-resolve
                  (compilation-context-push-local context #f)
                  (cadr expression))
                (compile-unspecified continuation))))

          (else
            (compile-call context expression continuation)))))

    (else
      (compile-constant expression continuation))))

(define (compile expression)
  (compile-expression (make-compilation-context '()) expression '()))

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

(define-record-type encode-context
  (make-encode-context symbols constants)
  encode-context?
  (symbols encode-context-symbols encode-context-set-symbols!)
  (constants encode-context-constants encode-context-set-constants!))

(define (encode-context-all-symbols context)
  (append
    (map cdr default-constants)
    (list rib-symbol)
    (encode-context-symbols context)))

(define (encode-context-constant context constant)
  (let ((pair (assq constant (append default-constants (encode-context-constants context)))))
    (if pair (cdr pair) #f)))

(define (encode-context-constant-id context)
  (string->symbol
    (string-append
      "$c"
      (number->string (length (encode-context-constants context))))))

(define (encode-context-add-constant! context constant symbol)
  (encode-context-set-symbols!
    context
    (cons symbol (encode-context-symbols context)))
  (encode-context-set-constants!
    context
    (cons (cons constant symbol) (encode-context-constants context))))

;; Constants

(define (constant-normal? constant)
  (or
    (symbol? constant)
    (and (number? constant) (>= constant 0))
    (stak-procedure? constant)))

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
              (compile-primitive-call '$- continuation))))

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

(define (build-constants context codes continuation)
  (if (null? codes)
    continuation
    (let (
        (instruction (rib-tag codes))
        (operand (rib-car codes)))
      (build-constants
        context
        (rib-cdr codes)
        (cond
          ((eqv? instruction constant-instruction)
            (let ((continuation (build-constant context operand continuation)))
              (if (stak-procedure? operand)
                (build-constants context (procedure-code operand) continuation)
                continuation)))

          ((eqv? instruction if-instruction)
            (build-constants context operand continuation))

          (else
            continuation))))))

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
          (memv-position operand (encode-context-all-symbols context))
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
              (stak-procedure? operand))
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

(define (build-primitives primitives continuation)
  (if (null? primitives)
    continuation
    (build-primitive
      (car primitives)
      (build-primitives (cdr primitives) continuation))))

;; Main

(define (encode codes)
  (let* (
      (context
        (make-encode-context
          (append
            (map car primitives)
            (find-symbols codes))
          '()))
      (codes (build-primitives primitives (build-constants context codes codes))))
    (encode-symbols
      (encode-context-symbols context)
      (encode-codes context codes '() '()))))

; Main

(write-target (encode (compile (expand (read-source)))))
