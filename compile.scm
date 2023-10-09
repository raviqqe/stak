; Stak compiler based on Ribbit's
;
; All compiler-internal variables contain at least one `$` character in their names.

; Compatibility

(import
  (scheme base)
  (scheme cxr)
  (scheme read)
  (scheme write))

(cond-expand
  ((or chibi gauche guile)
    (define-record-type rib*
      (make-rib tag car cdr)
      rib?
      (tag rib-tag)
      (car rib-car rib-set-car!)
      (cdr rib-cdr rib-set-cdr!))

    (define rib make-rib)

    (define (rib-cons car cdr)
      (rib 0 car cdr)))

  (else))

; Constants

(define default-constants
  '(
    (#f . $$false)
    (#t . $$true)
    (() . $$null)))

(define rib-symbol '$$rib)

; Instructions

(define call-instruction 0)
(define set-instruction 1)
(define get-instruction 2)
(define constant-instruction 3)
(define if-instruction 4)
; Only for encoding
(define closure-instruction 5)
(define skip-instruction 6)
(define continue-instruction 7)

; Primitives

(define primitives
  '(
    ($$cons 1)
    ($$close 2)
    ($$- 13)))

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

(define (bytevector->list xs)
  (let loop ((index 0) (result '()))
    (if (< index (bytevector-length xs))
      (cons
        (bytevector-u8-ref xs index)
        (loop (+ 1 index) result))
      result)))

(define (last-cdr xs)
  (if (pair? xs)
    (last-cdr (cdr xs))
    xs))

(define (set-last-cdr! xs x)
  (if (pair? (cdr xs))
    (set-last-cdr! (cdr xs) x)
    (set-cdr! xs x)))

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
    (f (fold-right f y (cdr xs)) (car xs))))

(define (take n list)
  (if (= n 0)
    '()
    (cons
      (car list)
      (take (- n 1) (cdr list)))))

(define (skip n list)
  (if (= n 0)
    list
    (skip (- n 1) (cdr list))))

(define (list-unique xs)
  (let loop ((xs xs) (ys '()))
    (if (null? xs)
      ys
      (loop
        (cdr xs)
        (let ((x (car xs)))
          (if (memv x ys)
            ys
            (cons x ys)))))))

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

(define (map-values f xs)
  (map (lambda (pair) (cons (car pair) (f (cdr pair)))) xs))

(define (zip-alist xs)
  (define (zip xs)
    (if (memv #f (map (lambda (pair) (pair? (cdr pair))) xs))
      '()
      (cons
        (map-values car xs)
        (zip (map-values cdr xs)))))

  (if (null? xs) '() (zip xs)))

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

;; Context

(define-record-type expansion-context
  (make-expansion-context environment)
  expansion-context?
  (environment expansion-context-environment expansion-context-set-environment!))

(define (expansion-context-append context pairs)
  (make-expansion-context (append pairs (expansion-context-environment context))))

(define (expansion-context-push context name denotation)
  (expansion-context-append context (list (cons name denotation))))

(define (expansion-context-set! context name denotation)
  (let* (
      (environment (expansion-context-environment context))
      (pair (assv name environment)))
    (when pair (set-cdr! pair denotation))
    pair))

(define (expansion-context-set-last! context name denotation)
  (unless (expansion-context-set! context name denotation)
    (let (
        (environment (expansion-context-environment context))
        (tail (list (cons name denotation))))
      (if (null? environment)
        (expansion-context-set-environment! context tail)
        (set-last-cdr! environment tail)))))

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
    (if (and pair (= (length expression) 3))
      (cons (cdr pair) (cdr expression))
      expression)))

(define (resolve-denotation context expression)
  (cond
    ((assv expression (expansion-context-environment context)) =>
      cdr)

    (else
      expression)))

(define (rename-variable context name)
  (let* (
      (denotation (resolve-denotation context name))
      (count
        (list-count
          (lambda (pair) (eqv? (cdr pair) denotation))
          (expansion-context-environment context))))
    (string->symbol (string-append (symbol->string name) "$" (number->string count 32)))))

(define (find-pattern-variables bound-variables pattern)
  (define (find pattern)
    (cond
      ((memv pattern (append '(_ ...) bound-variables))
        '())

      ((symbol? pattern)
        (list pattern))

      ((pair? pattern)
        (append
          (find (car pattern))
          (find (cdr pattern))))

      (else
        '())))

  (list-unique (find pattern)))

(define-record-type ellipsis-match
  (make-ellipsis-match value)
  ellipsis-match?
  (value ellipsis-match-value))

(define (match-ellipsis-pattern definition-context use-context literals pattern expression)
  (let (
      (matches
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
            expression))))
    (and
      matches
      (map
        (lambda (pair)
          (cons (car pair) (make-ellipsis-match (cdr pair))))
        matches))))

(define (match-pattern definition-context use-context literals pattern expression)
  (define (match pattern expression)
    (match-pattern definition-context use-context literals pattern expression))

  (cond
    ((eqv? pattern '_)
      '())

    ((memv pattern literals)
      (if (eqv?
          (resolve-denotation use-context expression)
          (resolve-denotation definition-context pattern))
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
            (and
              (>= length 0)
              (maybe-append
                (match-ellipsis-pattern
                  definition-context
                  use-context
                  literals
                  (car pattern)
                  (take length expression))
                (match (cddr pattern) (skip length expression))))))

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
    (let* (
        (variables (find-pattern-variables '() template))
        (matches (filter (lambda (pair) (memv (car pair) variables)) matches))
        (singleton-matches (filter (lambda (pair) (not (ellipsis-match? (cdr pair)))) matches))
        (ellipsis-matches (filter (lambda (pair) (ellipsis-match? (cdr pair))) matches)))
      (when (null? ellipsis-matches)
        (error "no ellipsis pattern variables" template))
      (map
        (lambda (matches) (append singleton-matches matches))
        (zip-alist
          (map
            (lambda (pair)
              (cons (car pair) (ellipsis-match-value (cdr pair))))
            ellipsis-matches))))))

(define (fill-template definition-context use-context matches template)
  (define (fill template)
    (fill-template definition-context use-context matches template))

  (cond
    ((symbol? template)
      (cond
        ((assv template matches) =>
          cdr)

        ; Skip a literal.
        (else
          template)))

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
            (let* (
                (template (cadr rule))
                (names
                  (map
                    (lambda (name) (cons name (rename-variable use-context name)))
                    (find-pattern-variables (append literals (map car matches)) template)))
                (use-context
                  (expansion-context-append
                    use-context
                    (map
                      (lambda (pair)
                        (cons
                          (cdr pair)
                          (resolve-denotation definition-context (car pair))))
                      names))))
              (values
                (fill-template definition-context use-context (append names matches) template)
                use-context))
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
        (let ((value (resolve-denotation context expression)))
          (when (procedure? value)
            (error "invalid syntax" expression))
          value))

      ((pair? expression)
        (case (resolve-denotation context (car expression))
          (($$define)
            (let ((name (cadr expression)))
              (expansion-context-set! context name name)
              (expand `($$set! ,@(cdr expression)))))

          (($$define-syntax)
            (expansion-context-set-last!
              context
              (cadr expression)
              (make-transformer context (caddr expression)))
            #f)

          (($$lambda)
            (let* (
                (parameters (cadr expression))
                (context
                  (expansion-context-append
                    context
                    (map
                      (lambda (name) (cons name (rename-variable context name)))
                      (parameter-names parameters))))
                ; We need to resolve parameter denotations before expanding a body.
                (parameters
                  (relaxed-deep-map
                    (lambda (name) (resolve-denotation context name))
                    parameters)))
              (list
                '$$lambda
                parameters
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
                (let-values (((expression context) (value context expression)))
                  (expand-expression context expression))
                (map expand expression))))))

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
      (case name
        (($$close)
          1)

        (($$cons $$-)
          2)

        (($$rib)
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

(define (compile-raw-call context function arguments argument-count continuation)
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
      (compile-raw-call
        (compilation-context-push-local context #f)
        function
        (cdr arguments)
        argument-count
        continuation))))

(define (compile-call context expression variadic continuation)
  (let* (
      (function (car expression))
      (arguments (cdr expression))
      (continue
        (lambda (context function continuation)
          (compile-raw-call
            context
            function
            arguments
            (- (* 2 (length arguments)) (if variadic 1 0))
            continuation))))
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
        (($$apply)
          (compile-call context (cdr expression) #t continuation))

        (($$begin)
          (compile-sequence context (cdr expression) continuation))

        (($$if)
          (compile-expression
            context
            (cadr expression)
            (rib
              if-instruction
              (compile-expression context (caddr expression) (rib continue-instruction 0 continuation))
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
              (compile-primitive-call '$$close continuation))))

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
          (compile-call context expression #f continuation))))

    (else
      (compile-constant expression continuation))))

(define (compile expression)
  (compile-expression (make-compilation-context '()) expression '()))

; Constant building

;; Context

(define-record-type constant-context
  (make-constant-context constants constant-id)
  constant-context?
  (constants constant-context-constants constant-context-set-constants!)
  (constant-id constant-context-constant-id constant-context-set-constant-id!))

(define (constant-context-constant context constant)
  (cond
    ((assv constant (append default-constants (constant-context-constants context))) =>
      cdr)

    (else
      #f)))

(define (constant-context-add-constant! context constant symbol)
  (constant-context-set-constants!
    context
    (cons (cons constant symbol) (constant-context-constants context))))

(define (constant-context-generate-constant-id! context)
  (let ((id (constant-context-constant-id context)))
    (constant-context-set-constant-id! context (+ id 1))
    (string->symbol (string-append "$" (number->string id)))))

;; Main

; We do not need to check boolean and null which are registered as default constants.
(define (constant-normal? constant)
  (or
    (symbol? constant)
    (and (number? constant) (>= constant 0))
    (stak-procedure? constant)))

(define (build-rib-constant-codes context car cdr tag continue)
  (define (build-child constant continue)
    (build-constant context constant (lambda () (build-constant-codes context constant continue))))

  (build-child
    car
    (lambda ()
      (build-child
        cdr
        (lambda ()
          (if (eqv? tag pair-type)
            (compile-primitive-call '$$cons (continue))
            (rib
              constant-instruction
              tag
              (compile-primitive-call rib-symbol (continue)))))))))

(define (build-constant-codes context constant continue)
  (let (
      (symbol (constant-context-constant context constant))
      (build-rib
        (lambda (car cdr tag)
          (build-rib-constant-codes context car cdr tag continue))))
    (if symbol
      (rib get-instruction symbol (continue))
      (cond
        ((constant-normal? constant)
          (rib constant-instruction constant (continue)))

        ((bytevector? constant)
          (build-rib
            (bytevector-length constant)
            (bytevector->list constant)
            bytevector-type))

        ((char? constant)
          (build-rib (char->integer constant) '() char-type))

        ((and (number? constant) (> 0 constant))
          (rib
            constant-instruction
            0
            (rib
              constant-instruction
              (abs constant)
              (compile-primitive-call '$$- (continue)))))

        ((pair? constant)
          (build-rib (car constant) (cdr constant) pair-type))

        ((string? constant)
          (build-rib
            (string-length constant)
            (map char->integer (string->list constant))
            string-type))

        ((vector? constant)
          (build-rib
            (vector-length constant)
            (vector->list constant)
            vector-type))

        (else
          (error "invalid constant" constant))))))

(define (build-constant context constant continue)
  (if (or (constant-normal? constant) (constant-context-constant context constant))
    (continue)
    (let ((id (constant-context-generate-constant-id! context)))
      (build-constant-codes
        context
        constant
        (lambda ()
          (constant-context-add-constant! context constant id)
          (rib set-instruction id (continue)))))))

(define (build-constants context codes)
  (let loop ((codes codes) (continue (lambda () codes)))
    (if (terminal-codes? codes)
      (continue)
      (let* (
          (instruction (rib-tag codes))
          (operand (rib-car codes))
          (codes (rib-cdr codes))
          (continue (lambda () (loop codes continue))))
        (cond
          ((eqv? instruction constant-instruction)
            (build-constant
              context
              operand
              (if (stak-procedure? operand)
                (lambda () (loop (procedure-code operand) continue))
                continue)))

          ((eqv? instruction if-instruction)
            (loop operand continue))

          (else
            (continue)))))))

; Encoding

;; Utility

(define (find-symbols codes)
  (let loop ((codes codes) (symbols '()))
    (if (terminal-codes? codes)
      symbols
      (let* (
          (instruction (rib-tag codes))
          (operand (rib-car codes))
          (codes (rib-cdr codes))
          (operand
            (if (eqv? instruction call-instruction)
              (rib-cdr operand)
              operand)))
        (loop
          codes
          (cond
            ((and
                (eqv? instruction constant-instruction)
                (stak-procedure? operand))
              (loop (procedure-code operand) symbols))

            ((eqv? instruction if-instruction)
              (loop operand symbols))

            ((and
                (symbol? operand)
                (not (eqv? operand rib-symbol))
                (not (memq operand symbols)))
              (cons operand symbols))

            (else
              symbols)))))))

(define (terminal-codes? codes)
  (or (null? codes) (eqv? (rib-tag codes) continue-instruction)))

(define (find-continuation codes)
  (cond
    ((null? codes)
      '())

    ((terminal-codes? codes)
      (rib-cdr codes))

    (else
      (find-continuation (rib-cdr codes)))))

(define (count-skips codes continuation)
  (let loop ((codes codes) (count 0))
    (if (eq? codes continuation)
      count
      (loop (rib-cdr codes) (+ 1 count)))))

;; Context

(define-record-type encode-context
  (make-encode-context symbols constant-context)
  encode-context?
  (symbols encode-context-symbols encode-context-set-symbols!)
  (constant-context encode-context-constant-context))

(define (encode-context-constant context constant)
  (constant-context-constant (encode-context-constant-context context) constant))

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
  (if (= count 0)
    target
    (let ((target (encode-symbol (car symbols) target)))
      (if (= count 1)
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
    (if (= x 0)
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
          (memv-position operand (encode-context-symbols context))
          (error "symbol not found" operand))))

    (else
      (error "invalid operand" operand))))

(define (encode-codes context codes terminal target)
  (if (eq? codes terminal)
    target
    (let* (
        (instruction (rib-tag codes))
        (operand (rib-car codes))
        (codes (rib-cdr codes))
        (return (null? codes))
        (encode-simple
          (lambda (instruction)
            (encode-instruction
              instruction
              (encode-operand context operand)
              return
              target))))
      (encode-codes
        context
        codes
        terminal
        (cond
          ((memv instruction (list set-instruction get-instruction continue-instruction))
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
                (continuation (find-continuation operand))
                (target
                  (encode-codes
                    context
                    operand
                    continuation
                    (encode-instruction if-instruction 0 #f target))))
              (if (null? continuation)
                target
                (encode-instruction skip-instruction (count-skips codes continuation) #t target))))

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
          rib-symbol
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
      (constant-context (make-constant-context '() 0))
      (codes
        (build-primitives
          primitives
          (build-constants constant-context codes)))
      (symbols (find-symbols codes)))
    (encode-symbols
      symbols
      (encode-codes
        (make-encode-context
          (append (map cdr default-constants) (list rib-symbol) symbols)
          constant-context)
        codes
        '()
        '()))))

; Main

(write-target (encode (compile (expand (read-source)))))
