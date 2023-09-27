#!/usr/bin/env gsi

; Stak compiler based on Ribbit's
;
; All compiler-generated variables are prefixed with `$`.

; Compatibility

(import
  (scheme base)
  (scheme cxr)
  (scheme read)
  (scheme write))

(cond-expand
  ((or chibi gambit gauche)
    (define (rib tag car cdr)
      (cons (cons (cons '$$rib tag) car) cdr))

    (define (rib-cons car cdr)
      (cons (cons (cons '$$rib 0) car) cdr))

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
        (eqv? (caaar value) '$$rib))))

  (else))

; Constants

(define default-constants
  '(
    (#f . $$false)
    (#t . $$true)
    (() . $$null)))

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
    ($$- 13)))

(define primitive-syntaxes
  '(
    $$begin
    $$define
    $$define-syntax
    $$if
    $$lambda
    $$let-syntax
    $$letrec-syntax
    $$quasiquote
    $$quote
    $$set!))

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

; Note that the original `append` function works in this way natively on some Scheme implementations.
(define (maybe-append xs ys)
  (and xs ys (append xs ys)))

(define (relaxed-length xs)
  (let loop ((xs xs) (y 0))
    (if (pair? xs)
      (loop (cdr xs) (+ y 1))
      y)))

(define (relaxed-deep-map f xs)
  (cond
    ((null? xs)
      '())

    ((pair? xs)
      (cons
        (relaxed-deep-map f (car xs))
        (relaxed-deep-map f (cdr xs))))

    (else
      (f xs))))

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
      (error "invalid variadic parameter" parameters))))

; Source code reading

(define (read-all)
  (let ((x (read)))
    (if (eof-object? x)
      '()
      (cons x (read-all)))))

(define (read-source)
  (cons '$$begin (read-all)))

; Target code writing

(define (write-target codes)
  (map write-u8 codes))

; Expansion

(define default-syntactic-environment
  (map
    (lambda (syntax) (cons syntax syntax))
    primitive-syntaxes))

(define-record-type denotation
  (make-denotation name value)
  denotation?
  (name denotation-name)
  (value denotation-value))

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

;; Procedures

(define primitive-functions
  '(
    (+ . $$+)
    (- . $$-)
    (* . $$*)
    (/ . $$/)
    (< . $$<)))

; TODO Check if those primitive functions are from the `scheme base` library
; before applying optimization.
(define (optimize expression)
  (let ((pair (assv (predicate expression) primitive-functions)))
    (if (and pair (eqv? (length expression) 3))
      (cons (cdr pair) (cdr expression))
      expression)))

; Note that we distinguish unresolved identifiers and denotations even after
; denotation resolution because there is no "true" name of global variables in
; this implementation differently from the original paper of "Macros That Work."
(define (resolve-denotation context expression)
  (if (denotation? expression)
    expression
    (let ((pair (assv expression (expansion-context-environment context))))
      (if pair
        (make-denotation expression (cdr pair))
        expression))))

(define (resolve-denotation-value context expression)
  (let ((denotation (resolve-denotation context expression)))
    (if (denotation? denotation)
      (denotation-value denotation)
      denotation)))

(define (unresolve-denotation denotation)
  (if (denotation? denotation)
    (denotation-name denotation)
    denotation))

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

(define (match-ellipsis-pattern definition-context use-context literals pattern expression)
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
        (match-pattern definition-context use-context literals pattern expression))
      expression)))

(define (match-pattern definition-context use-context literals pattern expression)
  (define (match pattern expression)
    (match-pattern definition-context use-context literals pattern expression))

  (cond
    ((eqv? pattern '_)
      '())

    ((memv pattern literals)
      (if (eqv?
          (resolve-denotation-value use-context expression)
          (resolve-denotation-value definition-context pattern))
        '()
        #f))

    ((symbol? pattern)
      (list (cons pattern expression)))

    ((pair? pattern)
      (cond
        ((and
            (pair? (cdr pattern))
            (eqv? (cadr pattern) '...))
          (let ((length (- (relaxed-length expression) (- (relaxed-length pattern) 2))))
            (maybe-append
              (match-ellipsis-pattern
                definition-context
                use-context
                literals
                (car pattern)
                (take length expression))
              (match (cddr pattern) (skip length expression)))))

        ((pair? expression)
          (maybe-append
            (match (car pattern) (car expression))
            (match (cdr pattern) (cdr expression))))

        (else
          #f)))

    ((equal? pattern expression)
      '())

    (else
      #f)))

(define (fill-ellipsis-template definition-context use-context matches template)
  (map
    (lambda (matches) (fill-template definition-context use-context matches template))
    (let ((variables (find-pattern-variables '() template)))
      (zip-alist
        (filter
          (lambda (pair) (memv (car pair) variables))
          matches)))))

(define (fill-template definition-context use-context matches template)
  (define (fill template)
    (fill-template definition-context use-context matches template))

  (cond
    ((symbol? template)
      (let ((pair (assv template matches)))
        (if pair
          (cdr pair)
          (let (
              (name (denote-parameter use-context template))
              (denotation (resolve-denotation definition-context template)))
            (when (denotation? denotation)
              ; TODO Test if this is really hygiene.
              ; It looks like this destructive update of contexts is fine because
              ; we always generate new names. But I'm not sure...
              ; For example, how about this?
              ;
              ; ```scheme
              ; (((lambdas (x) x) f) ((lambdas (x) x) y))
              ; ```
              (expansion-context-set! use-context name (denotation-value denotation)))
            name))))

    ((pair? template)
      (if (and
          (pair? (cdr template))
          (eqv? (cadr template) '...))
        (append
          (fill-ellipsis-template definition-context use-context matches (car template))
          (fill (cddr template)))
        (cons
          (fill (car template))
          (fill (cdr template)))))

    (else
      template)))

(define (make-transformer definition-context transformer)
  (unless (eqv? (predicate transformer) 'syntax-rules)
    (error "unsupported macro transformer" transformer))
  (let (
      (literals (cadr transformer))
      (rules (cddr transformer)))
    (lambda (use-context expression)
      (let loop ((rules rules))
        (unless (pair? rules)
          (error "invalid syntax" expression))
        (let* (
            (rule (car rules))
            (matches (match-pattern definition-context use-context literals (car rule) expression)))
          (if matches
            (fill-template definition-context use-context matches (cadr rule))
            (loop (cdr rules))))))))

(define (expand-definition definition)
  (let (
      (pattern (cadr definition))
      (body (cddr definition)))
    (if (symbol? pattern)
      (cons pattern body)
      (list
        (car pattern)
        (cons '$$lambda (cons (cdr pattern) body))))))

(define (expand-quasiquote expression)
  (cond
    ((not (pair? expression))
      `($$quote ,expression))

    ((eqv? (car expression) 'unquote)
      (cadr expression))

    ((and
        (pair? (car expression))
        (eqv? (caar expression) 'unquote-splicing))
      `(append
        ,(cadar expression)
        ,(expand-quasiquote (cdr expression))))

    (else
      `(cons
        ,(expand-quasiquote (car expression))
        ,(expand-quasiquote (cdr expression))))))

; https://www.researchgate.net/publication/220997237_Macros_That_Work
(define (expand-expression context expression)
  (define (expand expression)
    (expand-expression context expression))

  (optimize
    (cond
      ((symbol? expression)
        (let ((value (resolve-denotation-value context expression)))
          (when (procedure? value)
            (error "invalid syntax" expression))
          value))

      ((pair? expression)
        (case (resolve-denotation-value context (car expression))
          (($$define)
            (let ((name (cadr expression)))
              (expansion-context-set! context name name)
              (expand `($$set! ,@(cdr expression)))))

          (($$define-syntax)
            (expansion-context-set!
              context
              (cadr expression)
              (make-transformer context (caddr expression)))
            #f)

          ; TODO Implement an import statement.
          ((import)
            #f)

          (($$lambda)
            (let* (
                (parameters (relaxed-deep-map unresolve-denotation (cadr expression)))
                (context
                  (expansion-context-append
                    context
                    (map
                      (lambda (name) (cons name (denote-parameter context name)))
                      (parameter-names parameters)))))
              (list
                '$$lambda
                (relaxed-deep-map
                  (lambda (name) (resolve-denotation-value context name))
                  parameters)
                (expand-expression context (caddr expression)))))

          (($$let-syntax)
            (expand-expression
              (fold-left
                (lambda (context pair)
                  (expansion-context-push
                    context
                    (car pair)
                    (make-transformer context (cadr pair))))
                context
                (cadr expression))
              (caddr expression)))

          (($$letrec-syntax)
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
                  (expansion-context-set!
                    context
                    (car pair)
                    (make-transformer context (cadr pair))))
                bindings)
              (expand-expression context (caddr expression))))

          (($$quasiquote)
            (expand-quasiquote (cadr expression)))

          (($$quote)
            (cons '$$quote (cdr expression)))

          (else =>
            (lambda (value)
              (if (procedure? value)
                (expand (value context expression))
                (map expand expression))))))

      (else
        (resolve-denotation-value context expression)))))

(define (expand expression)
  (expand-expression
    (make-expansion-context default-syntactic-environment)
    expression))

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
      (case name
        ((close)
          1)

        ((cons $$-)
          2)

        ((rib)
          3)

        (else
          (error "unknown primitive" name)))
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
      (case (car expression)
        (($$begin)
          (compile-sequence context (cdr expression) continuation))

        (($$if)
          (compile-expression
            context
            (cadr expression)
            (rib if-instruction
              (compile-expression context (caddr expression) continuation)
              (compile-expression context (cadddr expression) continuation))))

        (($$lambda)
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

        (($$quote)
          (compile-constant (cadr expression) continuation))

        (($$set!)
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
          (compile-call context expression continuation))))

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
  (make-encode-context symbols constants all-symbols)
  encode-context?
  (symbols encode-context-symbols encode-context-set-symbols!)
  (constants encode-context-constants encode-context-set-constants!)
  (all-symbols encode-context-all-symbols* encode-context-set-all-symbols!))

(define (encode-context-all-symbols context)
  (when (not (encode-context-all-symbols* context))
    (encode-context-set-all-symbols!
      context
      (append
        (map cdr default-constants)
        (list rib-symbol)
        (encode-context-symbols context)
        (map cdr (encode-context-constants context)))))
  (encode-context-all-symbols* context))

(define (encode-context-constant context constant)
  (cond
    ((assv constant (append default-constants (encode-context-constants context)))
      =>
      cdr)

    (else
      #f)))

(define (encode-context-constant-id context)
  (string->symbol
    (string-append
      "$c"
      (number->string (length (encode-context-constants context))))))

(define (encode-context-add-constant! context constant symbol)
  (encode-context-set-constants!
    context
    (cons (cons constant symbol) (encode-context-constants context))))

;; Constants

; We do not need to check boolean and null which are registered as default constants.
(define (constant-normal? constant)
  (or
    (symbol? constant)
    (and (number? constant) (>= constant 0))
    (stak-procedure? constant)))

(define (build-rib-constant-codes context car cdr tag continue)
  (define (build-child constant continue)
    (build-constant
      context
      constant
      (lambda ()
        (build-constant-codes
          context
          constant
          continue))))

  (build-child
    car
    (lambda ()
      (build-child
        cdr
        (lambda ()
          (if (eqv? tag pair-type)
            (compile-primitive-call 'cons (continue))
            (rib
              constant-instruction
              tag
              (compile-primitive-call 'rib (continue)))))))))

(define (build-constant-codes context constant continue)
  (let (
      (symbol (encode-context-constant context constant))
      (build-rib-constant-codes
        (lambda (car cdr tag)
          (build-rib-constant-codes context car cdr tag continue))))
    (if symbol
      (rib get-instruction symbol (continue))
      (cond
        ((constant-normal? constant)
          (rib constant-instruction constant (continue)))

        ((bytevector? constant)
          (build-rib-constant-codes
            (bytevector-length constant)
            (bytevector->list constant)
            bytevector-type))

        ((char? constant)
          (build-rib-constant-codes (char->integer constant) '() char-type))

        ; Negative number
        ((number? constant)
          (rib constant-instruction
            0
            (rib constant-instruction
              (abs constant)
              (compile-primitive-call '$$- (continue)))))

        ((pair? constant)
          (build-rib-constant-codes (car constant) (cdr constant) pair-type))

        ((string? constant)
          (build-rib-constant-codes
            (string-length constant)
            (map char->integer (string->list constant))
            string-type))

        ((vector? constant)
          (build-rib-constant-codes
            (vector-length constant)
            (vector->list constant)
            vector-type))

        (else
          (error "invalid constant" constant))))))

(define (build-constant context constant continue)
  (if (or (constant-normal? constant) (encode-context-constant context constant))
    (continue)
    (let ((id (encode-context-constant-id context)))
      (build-constant-codes
        context
        constant
        (lambda ()
          (encode-context-add-constant! context constant id)
          (rib set-instruction id (continue)))))))

(define (build-constants context codes continue)
  (if (null? codes)
    (continue)
    (let (
        (continue (lambda () (build-constants context (rib-cdr codes) continue)))
        (instruction (rib-tag codes))
        (operand (rib-car codes)))
      (cond
        ((eqv? instruction constant-instruction)
          (build-constant
            context
            operand
            (if (stak-procedure? operand)
              (lambda () (build-constants context (procedure-code operand) continue))
              continue)))

        ((eqv? instruction if-instruction)
          (build-constants context operand continue))

        (else
          (continue))))))

;; Symbols

(define (encode-string string target)
  (if (null? string)
    target
    (encode-string (cdr string) (cons (char->integer (car string)) target))))

(define (encode-symbol symbol target)
  (encode-string (string->list (symbol->string symbol)) target))

(define (empty-symbol? symbol)
  (eqv? (string-ref (symbol->string symbol) 0) #\$))

(define (count-empty-symbols symbols)
  (let loop ((symbols symbols) (count 0))
    (if (null? symbols)
      count
      (loop
        (cdr symbols)
        (if (empty-symbol? (car symbols))
          (+ count 1)
          0)))))

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
          (error "symbol not found" operand))))

    (else
      (error "invalid operand" operand))))

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

          (else
            (error "invalid instruction" instruction)))))))

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
          '()
          #f))
      (codes
        (build-primitives
          primitives
          (build-constants context codes (lambda () codes)))))
    (encode-symbols
      (append
        (encode-context-symbols context)
        (map cdr (encode-context-constants context)))
      (encode-codes context codes '() '()))))

; Main

(write-target (encode (compile (expand (read-source)))))
