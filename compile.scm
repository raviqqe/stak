; Stak Scheme compiler.
;
; All compiler-internal variables contain at least one `$` or `%` character in their names.

(import
  (scheme base)
  (only (scheme cxr))
  (scheme eval)
  (only (scheme file))
  (only (scheme inexact))
  (scheme process-context)
  (scheme read)
  (only (scheme write)))

(define frontend
  '(
    ; Instructions

    (define constant-instruction 0)
    (define get-instruction 1)
    (define set-instruction 2)
    (define if-instruction 3)
    (define call-instruction 4)
    (define nop-instruction 65535)

    ; Primitives

    (define primitives
     '(($$rib 0)
       ($$close 1)
       ($$unbind 2)
       ($$< 10)
       ($$+ 11)
       ($$- 12)
       ($$* 13)
       ($$/ 14)))

    ; Types

    (define pair-type 0)
    (define null-type 1)
    (define boolean-type 2)
    (define procedure-type 3)
    (define symbol-type 4)
    (define string-type 5)
    (define char-type 6)
    (define vector-type 7)
    (define bytevector-type 8)

    ; Utility

    (define (debug . xs)
     (write xs (current-error-port))
     (newline (current-error-port)))

    (define (code-rib tag car cdr)
     (rib car cdr tag))

    (define (call-rib arity procedure continuation)
     (code-rib (+ call-instruction arity) procedure continuation))

    (define (constant-rib constant continuation)
     (code-rib constant-instruction constant continuation))

    (define (nop-rib continuation)
     (code-rib nop-instruction 0 continuation))

    (define (data-rib type car cdr)
     (rib car cdr type))

    (define (make-procedure arity code environment)
     (data-rib procedure-type (cons-rib arity code) environment))

    ; Because we have the `bytevector->list` procedure in Stak Scheme's standard library and
    ; the `bytevector-u8-ref` procedure in the standard library uses `bytevector->list` internally and
    ; Stak Scheme allows overwriting imported symbols,
    ; we intentionally name this procedure differently from `bytevector->list`.
    (define (bytes->list xs)
     (let loop ((index 0))
      (if (< index (bytevector-length xs))
       (cons (bytevector-u8-ref xs index) (loop (+ 1 index)))
       '())))

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
      (let ((x (car xs))
            (xs (filter f (cdr xs))))
       (if (f x)
        (cons x xs)
        xs))))

    (define (list-head xs n)
     (if (zero? n)
      '()
      (cons
       (car xs)
       (list-head (cdr xs) (- n 1)))))

    (define (list-index f xs)
     (let loop ((xs xs) (index 0))
      (cond
       ((null? xs)
        #f)
       ((f (car xs))
        index)
       (else
        (loop (cdr xs) (+ index 1))))))

    (define (equal-index f)
     (lambda (x xs)
      (list-index (lambda (y) (f x y)) xs)))

    (define memv-index (equal-index eqv?))
    (define memq-index (equal-index eq?))

    (define (append-map f xs)
     (apply append (map f xs)))

    (define (relaxed-length xs)
     (do ((xs xs (cdr xs)) (y 0 (+ y 1)))
      ((not (pair? xs))
       y)))

    (define (relaxed-map f xs)
     (if (pair? xs)
      (let ((x (f (car xs))))
       (cons x (relaxed-map f (cdr xs))))
      (f xs)))

    (define (deep-map f x)
     (let ((x (f x)))
      (if (list? x)
       (map (lambda (x) (deep-map f x)) x)
       x)))

    (define (relaxed-deep-map f xs)
     (if (pair? xs)
      (cons
       (relaxed-deep-map f (car xs))
       (relaxed-deep-map f (cdr xs)))
      (f xs)))

    (define (maybe-append xs ys)
     (and xs ys (append xs ys)))

    (define (unique xs)
     (if (null? xs)
      '()
      (let ((ys (unique (cdr xs))))
       (if (memq (car xs) ys)
        ys
        (cons (car xs) ys)))))

    (define (deep-unique x)
     (cond
      ((and (pair? x) (symbol? (car x)))
       (unique (cons (car x) (deep-unique (cdr x)))))

      ((pair? x)
       (deep-unique
        (append
         (deep-unique (car x))
         (deep-unique (cdr x)))))

      ((symbol? x)
       (list x))

      (else
       '())))

    (define (fold-left f y xs)
     (if (null? xs)
      y
      (fold-left
       f
       (f (car xs) y)
       (cdr xs))))

    (define (map-values f xs)
     (map (lambda (pair) (cons (car pair) (f (cdr pair)))) xs))

    (define (filter-values f xs)
     (filter (lambda (pair) (f (cdr pair))) xs))

    (define (append-multi-map key values xs)
     (cons
      (cons
       key
       (unique
        (append
         values
         (cond
          ((assq key xs) =>
           cdr)
          (else
           '())))))
      (filter
       (lambda (pair) (not (eq? (car pair) key)))
       xs)))

    (define (merge-multi-maps xs ys)
     (fold-left
      (lambda (pair ys)
       (append-multi-map (car pair) (cdr pair) ys))
      ys
      xs))

    ; TODO Set a true machine epsilon.
    (define epsilon
     (let ((x (/ 1 10000000 100000000)))
      (if (zero? x) 1 x)))

    (define (maybe-car expression)
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

    (define (symbol-append . xs)
     (string->symbol (apply string-append (map symbol->string xs))))

    (define symbol-name-separator #\#)

    ; Inclusion

    (define (include-files expression)
     (deep-map
      (lambda (expression)
       (if (and
            (pair? expression)
            (eq? (car expression) 'include))
        (cons
         'begin
         (append-map
          (lambda (name)
           (with-input-from-file name
            (lambda ()
             (let loop ()
              (let ((value (read)))
               (if (eof-object? value)
                '()
                (cons value (loop))))))))
          (cdr expression)))
        expression))
      expression))

    ; Library system

    ;; Types

    (define-record-type library
     (make-library exports imports body)
     library?
     (exports library-exports)
     (imports library-imports)
     (body library-body))

    (define-record-type library-context
     (make-library-context libraries imported)
     library-context?
     (libraries library-context-libraries library-context-set-libraries!)
     (imported library-context-imported library-context-set-imported!))

    (define (library-context-find context name)
     (cond
      ((assoc name (library-context-libraries context)) =>
       cdr)
      (else
       (error "unknown library" name))))

    (define (library-context-add! context name library)
     (library-context-set-libraries!
      context
      (cons (cons name library) (library-context-libraries context))))

    (define (library-context-import! context name)
     (let* ((names (library-context-imported context))
            (imported (member name names)))
      (unless imported
       (library-context-set-imported! context (cons name names)))
      (not imported)))

    ;; Procedures

    (define library-symbol-indicator #\%)

    (define (resolve-symbol-string name)
     (let* ((string (symbol->string name))
            (index (memv-index symbol-name-separator (string->list string))))
      (if index
       (string-copy string (+ index 1))
       string)))

    (define (built-in-symbol? name)
     (let ((name (symbol->string name)))
      (equal? (substring name 0 (min 2 (string-length name))) "$$")))

    (define (parse-import-set set)
     (let loop ((set set) (qualify (lambda (name) name)))
      (let ((loop (lambda (qualify) (loop (cadr set) qualify))))
       (case (maybe-car set)
        ((except)
         (loop
          (let ((names (cddr set)))
           (lambda (name)
            (if (memq name names)
             #f
             (qualify name))))))

        ((only)
         (loop
          (let ((names (cddr set)))
           (lambda (name)
            (if (memq name names)
             (qualify name)
             #f)))))

        ((prefix)
         (loop
          (lambda (name)
           (qualify (symbol-append (caddr set) name)))))

        ((rename)
         (loop
          (lambda (name)
           (qualify
            (cond
             ((assq name (cddr set)) =>
              cadr)

             (else
              name))))))

        (else
         (cons set qualify))))))

    (define (resolve-environment-symbols resolve expression)
     (relaxed-deep-map
      (lambda (value)
       (if (symbol? value)
        (resolve value)
        value))
      expression))

    (define (expand-library-bodies context names)
     (append-map
      (lambda (name)
       (if (library-context-import! context name)
        (let ((library (library-context-find context name)))
         (append
          (expand-library-bodies context (library-imports library))
          (library-body library)))
        '()))
      names))

    (define (collect-imported-names context sets)
     (append-map
      (lambda (pair)
       (append-map
        (lambda (names)
         (let ((name ((cdr pair) (car names))))
          (if name
           (list (cons name (cdr names)))
           '())))
        (library-exports (library-context-find context (car pair)))))
      sets))

    (define (add-library-definition! context expression)
     (define (collect-bodies predicate)
      (append-map
       cdr
       (filter
        (lambda (body) (eq? (car body) predicate))
        (cddr expression))))

     (define sets (map parse-import-set (collect-bodies 'import)))
     (define names (collect-imported-names context sets))

     (define (resolve-symbol name)
      (cond
       ((built-in-symbol? name)
        name)
       ((assq name names) =>
        cdr)
       (else
        (let ((renamed (string->uninterned-symbol
                        (string-append
                         (string library-symbol-indicator symbol-name-separator)
                         (symbol->string name)))))
         (set! names (cons (cons name renamed) names))
         renamed))))

     (library-context-add!
      context
      (cadr expression)
      (make-library
       (map-values
        resolve-symbol
        (map
         (lambda (name)
          (if (eq? (maybe-car name) 'rename)
           (cons (caddr name) (cadr name))
           (cons name name)))
         (collect-bodies 'export)))
       (map car sets)
       (resolve-environment-symbols resolve-symbol (collect-bodies 'begin)))))

    ; Macro system

    ;; Types

    (define-record-type macro-state
     (make-macro-state globals literals static-symbols dynamic-symbols)
     macro-state?
     (globals macro-state-globals macro-state-set-globals!)
     (literals macro-state-literals macro-state-set-literals!)
     (static-symbols macro-state-static-symbols macro-state-set-static-symbols!)
     (dynamic-symbols macro-state-dynamic-symbols macro-state-set-dynamic-symbols!))

    (define-record-type macro-context
     (make-macro-context state locals)
     macro-context?
     (state macro-context-state)
     (locals macro-context-locals))

    (define (macro-context-append context pairs)
     (make-macro-context
      (macro-context-state context)
      (append pairs (macro-context-locals context))))

    (define (macro-context-set-local! context name denotation)
     (let ((pair (assq name (macro-context-locals context))))
      (when pair (set-cdr! pair denotation))
      pair))

    (define (macro-context-set-global! context name denotation)
     (let ((state (macro-context-state context)))
      (macro-state-set-globals!
       state
       (cons (cons name denotation) (macro-state-globals state)))))

    (define (macro-context-append-literal! context name syntax)
     (define state (macro-context-state context))

     (macro-state-set-literals!
      state
      (cons
       (cons name syntax)
       (macro-state-literals state))))

    (define (macro-context-append-static-symbol! context symbol)
     (define state (macro-context-state context))
     (define symbols (macro-state-static-symbols state))

     (unless (memq symbol symbols)
      (macro-state-set-static-symbols! state (cons symbol symbols))))

    (define (macro-context-append-dynamic-symbol! context symbol)
     (define state (macro-context-state context))
     (define symbols (macro-state-dynamic-symbols state))

     (unless (memq symbol symbols)
      (macro-state-set-dynamic-symbols! state (cons symbol symbols))))

    (define-record-type rule-context
     (make-rule-context definition-context use-context literals)
     rule-context?
     (definition-context rule-context-definition-context)
     (use-context rule-context-use-context)
     (literals rule-context-literals))

    ;; Procedures

    (define (resolve-denotation context value)
     (cond
      ((assq value (macro-context-locals context)) =>
       cdr)
      (else
       value)))

    (define (resolve-denotation-value context value)
     (let ((value (resolve-denotation context value)))
      (cond
       ((and
         (symbol? value)
         (assq value (macro-state-globals (macro-context-state context))))
        =>
        cdr)
       (else
        value))))

    (define (rename-variable name)
     (string->uninterned-symbol
      (string-append
       (string symbol-name-separator)
       (resolve-symbol-string name))))

    (define-record-type ellipsis-match
     (make-ellipsis-match value)
     ellipsis-match?
     (value ellipsis-match-value))

    (define-record-type ellipsis-pattern
     (make-ellipsis-pattern element variables)
     ellipsis-pattern?
     (element ellipsis-pattern-element)
     (variables ellipsis-pattern-variables))

    (define (find-pattern-variables bound-variables pattern)
     (let loop ((pattern pattern) (variables '()))
      (cond
       ((pair? pattern)
        (loop
         (car pattern)
         (loop
          (cdr pattern)
          variables)))

       ((ellipsis-pattern? pattern)
        (loop (ellipsis-pattern-element pattern) variables))

       ((and (symbol? pattern) (not (memq pattern bound-variables)))
        (cons pattern variables))

       (else
        variables))))

    (define (compile-pattern context ellipsis literals pattern)
     (define (compile pattern)
      (compile-pattern context ellipsis literals pattern))

     (cond
      ((not (pair? pattern))
       pattern)

      ((and
        (pair? (cdr pattern))
        (eq? ellipsis (resolve-denotation context (cadr pattern))))
       (cons
        (let ((pattern (compile (car pattern))))
         (make-ellipsis-pattern
          pattern
          (find-pattern-variables literals pattern)))
        (compile (cddr pattern))))

      (else
       (cons
        (compile (car pattern))
        (compile (cdr pattern))))))

    (define (match-ellipsis-pattern context pattern expression)
     (map-values
      make-ellipsis-match
      (apply
       map
       list
       (ellipsis-pattern-variables pattern)
       (map
        (lambda (expression)
         (match-pattern context (ellipsis-pattern-element pattern) expression))
        expression))))

    (define (match-pattern context pattern expression)
     (define (match pattern expression)
      (match-pattern context pattern expression))

     (cond
      ((and
        (symbol? pattern)
        (memq
         (resolve-denotation (rule-context-definition-context context) pattern)
         (rule-context-literals context)))
       =>
       (lambda (pair)
        (unless (eq?
                 (resolve-denotation (rule-context-use-context context) expression)
                 (car pair))
         (raise #f))
        '()))

      ((symbol? pattern)
       (list (cons pattern expression)))

      ((pair? pattern)
       (cond
        ((ellipsis-pattern? (car pattern))
         (let ((length (- (relaxed-length expression) (relaxed-length (cdr pattern)))))
          (when (negative? length)
           (raise #f))
          (append
           (match-ellipsis-pattern context (car pattern) (list-head expression length))
           (match (cdr pattern) (list-tail expression length)))))

        ((pair? expression)
         (append
          (match (car pattern) (car expression))
          (match (cdr pattern) (cdr expression))))

        (else
         (raise #f))))

      ((equal? pattern expression)
       '())

      (else
       (raise #f))))

    (define (fill-ellipsis-template context matches template)
     (let* ((variables (ellipsis-pattern-variables template))
            (template (ellipsis-pattern-element template))
            (matches (filter (lambda (pair) (memq (car pair) variables)) matches))
            (singleton-matches (filter-values (lambda (match) (not (ellipsis-match? match))) matches))
            (ellipsis-matches (filter-values ellipsis-match? matches)))
      (when (null? ellipsis-matches)
       (error "no ellipsis pattern variables" template))
      (apply
       map
       (lambda matches (fill-template context (append matches singleton-matches) template))
       (map (lambda (pair) (ellipsis-match-value (cdr pair))) ellipsis-matches))))

    (define (fill-template context matches template)
     (define (fill template)
      (fill-template context matches template))

     (cond
      ((and (symbol? template) (assq template matches)) =>
       cdr)

      ((pair? template)
       (append
        (let ((first (car template)))
         (if (ellipsis-pattern? first)
          (fill-ellipsis-template context matches first)
          (list (fill first))))
        (fill (cdr template))))

      (else
       template)))

    (define (make-transformer definition-context transformer)
     (define (resolve value)
      (resolve-denotation definition-context value))

     (case (resolve (maybe-car transformer))
      (($$syntax-rules)
       (let* ((ellipsis (resolve (cadr transformer)))
              (literals (caddr transformer))
              (literal-denotations (map resolve literals))
              (rules
               (map
                (lambda (rule)
                 (map
                  (lambda (pattern)
                   (compile-pattern definition-context ellipsis literals pattern))
                  rule))
                (cdddr transformer))))
        (lambda (use-context expression)
         (let loop ((rules rules))
          (unless (pair? rules)
           (error "invalid syntax" expression))
          (let ((rule (car rules))
                (rule-context
                 (make-rule-context definition-context use-context literal-denotations)))
           (guard (value
                   ((not value)
                    (loop (cdr rules))))
            (let* ((matches (match-pattern rule-context (car rule) expression))
                   (template (cadr rule))
                   (names
                    (map
                     (lambda (name) (cons name (rename-variable name)))
                     (find-pattern-variables
                      (append literals (map car matches))
                      template))))
             (values
              (fill-template rule-context (append names matches) template)
              (macro-context-append
               use-context
               (map
                (lambda (pair)
                 (cons (cdr pair) (resolve (car pair))))
                names))))))))))
      (else
       (error "unsupported macro transformer" transformer))))

    ; https://www.researchgate.net/publication/220997237_Macros_That_Work
    (define (expand-macro context expression)
     (define (expand expression)
      (expand-macro context expression))

     (define (resolve name)
      (resolve-denotation-value context name))

     (cond
      ((symbol? expression)
       (let ((value (resolve expression)))
        (when (procedure? value)
         (error "invalid syntax" expression))
        (when (and
               (symbol? value)
               (not (assq expression (macro-context-locals context))))
         (macro-context-append-dynamic-symbol! context expression))
        value))

      ((pair? expression)
       (case (resolve (car expression))
        (($$define)
         (let ((name (cadr expression)))
          (macro-context-set-global! context name name)
          (macro-context-append-static-symbol! context name)
          (expand (cons '$$set! (cdr expression)))))

        (($$define-syntax)
         (let ((name (cadr expression))
               (transformer (caddr expression)))
          (macro-context-set-global!
           context
           name
           (make-transformer context transformer))
          (macro-context-append-literal!
           context
           name
           (relaxed-deep-map
            (lambda (value) (resolve-denotation context value))
            transformer))
          (macro-context-append-static-symbol! context name)
          #f))

        (($$lambda)
         (let* ((parameters (cadr expression))
                (context
                 (macro-context-append
                  context
                  (map
                   (lambda (name) (cons name (rename-variable name)))
                   (parameter-names parameters))))
                ; We need to resolve parameter denotations before expanding a body.
                (parameters
                 (relaxed-deep-map
                  (lambda (name) (resolve-denotation context name))
                  parameters)))
          (list
           '$$lambda
           parameters
           (expand-macro context (caddr expression)))))

        (($$let-syntax)
         (expand-macro
          (macro-context-append
           context
           (map-values
            (lambda (transformer)
             (make-transformer context (car transformer)))
            (cadr expression)))
          (caddr expression)))

        (($$letrec-syntax)
         (let* ((bindings (cadr expression))
                (context
                 (macro-context-append
                  context
                  (map-values
                   (lambda (transformer) #f)
                   bindings))))
          (for-each
           (lambda (pair)
            (macro-context-set-local!
             context
             (car pair)
             (make-transformer context (cadr pair))))
           bindings)
          (expand-macro context (caddr expression))))

        (($$quote)
         (cons
          '$$quote
          (relaxed-deep-map
           (lambda (value)
            (if (symbol? value)
             (string->symbol (resolve-symbol-string value))
             value))
           (cdr expression))))

        (($$syntax-error)
         (apply error (cadr expression) (cddr expression)))

        (else =>
         (lambda (value)
          (if (procedure? value)
           (let-values (((expression context) (value context expression)))
            (expand-macro context expression))
           (map expand expression))))))

      (else
       expression)))

    ; Optimization

    (define-record-type optimization-context
     (make-optimization-context optimizers literals)
     optimization-context?
     (optimizers optimization-context-optimizers optimization-context-set-optimizers!)
     (literals optimization-context-literals optimization-context-set-literals!))

    (define (optimization-context-append! context name optimizer)
     (optimization-context-set-optimizers!
      context
      (cons
       (cons name optimizer)
       (optimization-context-optimizers context))))

    (define (optimization-context-append-literal! context name literal)
     (optimization-context-set-literals!
      context
      (cons
       (cons name literal)
       (optimization-context-literals context))))

    (define optimizer-macro-context
     (make-macro-context (make-macro-state '() '() '() '()) '()))

    (define (make-optimizer optimizer)
     (case (car optimizer)
      (($$syntax-rules)
       (let* ((ellipsis (cadr optimizer))
              (literals (caddr optimizer))
              (rules
               (map
                (lambda (rule)
                 (map
                  (lambda (pattern)
                   (compile-pattern optimizer-macro-context ellipsis literals pattern))
                  rule))
                (cdddr optimizer))))
        (lambda (expression)
         (let loop ((rules rules))
          (if (null? rules)
           expression
           (guard (value
                   ((not value)
                    (loop (cdr rules))))
            (let ((rule (car rules))
                  (rule-context
                   (make-rule-context
                    optimizer-macro-context
                    optimizer-macro-context
                    literals)))
             (fill-template
              rule-context
              (match-pattern rule-context (car rule) expression)
              (cadr rule)))))))))
      (else
       (error "unsupported optimizer" optimizer))))

    (define (optimize-expression optimize expression)
     (if (or (not (pair? expression)) (eq? (car expression) '$$quote))
      expression
      (optimize
       (relaxed-map
        (lambda (expression) (optimize-expression optimize expression))
        expression))))

    (define (optimize-custom-expression context expression)
     (optimize-expression
      (lambda (expression)
       (let ((predicate (car expression)))
        (cond
         ((eq? predicate '$$define-optimizer)
          (let ((name (cadr expression)))
           (optimization-context-append! context name (make-optimizer (caddr expression)))
           (optimization-context-append-literal! context name (caddr expression)))
          #f)
         ((assq predicate (optimization-context-optimizers context)) =>
          (lambda (pair)
           (let ((optimized ((cdr pair) expression)))
            (if (equal? optimized expression)
             expression
             (optimize-custom-expression context optimized)))))
         (else
          expression))))
      expression))

    (define (optimize-begin expression)
     (optimize-expression
      (lambda (expression)
       (if (eq? (car expression) '$$begin)
        ; TODO Define this pass by `define-optimizer`.
        (cons '$$begin
         (let loop ((expressions (cdr expression)))
          (let ((expression (car expressions))
                (expressions (cdr expressions)))
           (cond
            ((null? expressions)
             (list expression))
            ((not (pair? expression))
             (loop expressions))
            ((eq? (car expression) '$$begin)
             (loop (append (cdr expression) expressions)))
            (else
             (cons expression (loop expressions)))))))
        expression))
      expression))

    ; Free variable analysis

    (define (find-free-variables bound-variables expression)
     (define (find expression)
      (find-free-variables bound-variables expression))

     (cond
      ((pair? expression)
       (case (car expression)
        (($$lambda)
         (filter
          (lambda (name)
           (memq name bound-variables))
          (cadr expression)))
        (($$quote)
         '())
        (else
         (unique
          (append
           (find (car expression))
           (find (cdr expression)))))))
      ((symbol? expression)
       (if (memq expression bound-variables)
        (list expression)
        '()))
      (else
       '())))

    (define (analyze-expressions bound-variables expressions)
     (map
      (lambda (expression)
       (analyze-expression bound-variables expression))
      expressions))

    (define (analyze-expression bound-variables expression)
     (cond
      ((pair? expression)
       (case (car expression)
        (($$lambda)
         (let* ((parameters (cadr expression))
                (body
                 (analyze-expressions
                  (unique (append (parameter-names parameters) bound-variables))
                  (cddr expression))))
          (cons
           '$$lambda
           (cons
            (unique
             (append-map
              (lambda (expression)
               (find-free-variables bound-variables expression))
              body))
            (cons parameters body)))))
        (($$quote)
         expression)
        (else
         (cons
          (analyze-expression bound-variables (car expression))
          (analyze-expression bound-variables (cdr expression))))))
      (else
       expression)))

    (define (analyze-free-variables expression)
     (analyze-expression '() expression))

    ; Compilation

    ;; Context

    (define-record-type compilation-context
     (make-compilation-context environment metadata)
     compilation-context?
     (environment compilation-context-environment)
     (metadata compilation-context-metadata))

    (define (compilation-context-append-locals context variables)
     (make-compilation-context
      (append variables (compilation-context-environment context))
      (compilation-context-metadata context)))

    (define (compilation-context-push-local context variable)
     (compilation-context-append-locals context (list variable)))

    ; If a variable is not in environment, it is considered to be global.
    (define (compilation-context-resolve context variable)
     (or (memq-index variable (compilation-context-environment context)) variable))

    ;; Procedures

    (define (compile-arity argument-count variadic)
     (+
      (* 2 argument-count)
      (if variadic 1 0)))

    (define (drop? codes)
     (and
      (rib? codes)
      (not (null? codes))
      (eq? (rib-tag codes) set-instruction)
      (eq? (rib-car codes) 0)))

    (define (compile-unspecified continuation)
     (if (drop? continuation)
      ; Skip a "drop" instruction.
      (rib-cdr continuation)
      (constant-rib #f continuation)))

    (define (compile-drop continuation)
     (if (null? continuation)
      continuation
      (code-rib set-instruction 0 continuation)))

    (define (compile-sequence context expressions continuation)
     (compile-expression
      context
      (car expressions)
      (if (null? (cdr expressions))
       continuation
       (compile-drop (compile-sequence context (cdr expressions) continuation)))))

    (define (compile-unbind continuation)
     (if (null? continuation)
      continuation
      (call-rib (compile-arity 2 #f) '$$unbind continuation)))

    (define (compile-let context bindings body continuation)
     (let loop ((context context)
                (body-context context)
                (bindings bindings)
                (continuation continuation))
      (if (pair? bindings)
       (let ((binding (car bindings)))
        (compile-expression
         context
         (cadr binding)
         (loop
          (compilation-context-push-local context #f)
          (compilation-context-push-local body-context (car binding))
          (cdr bindings)
          (compile-unbind continuation))))
       (compile-expression body-context body continuation))))

    (define (compile-unsafe-unbind continuation)
     (if (null? continuation)
      continuation
      (code-rib set-instruction 1 continuation)))

    (define (compile-raw-call context procedure arguments arity continuation)
     (if (null? arguments)
      (call-rib
       arity
       (compilation-context-resolve context procedure)
       continuation)
      (compile-expression
       context
       (car arguments)
       (compile-raw-call
        (compilation-context-push-local context #f)
        procedure
        (cdr arguments)
        arity
        continuation))))

    (define (compile-call context expression variadic continuation)
     (let* ((procedure (car expression))
            (arguments (cdr expression))
            (continue
             (lambda (context procedure continuation)
              (compile-raw-call
               context
               procedure
               arguments
               (compile-arity
                (- (length arguments) (if variadic 1 0))
                variadic)
               continuation))))
      (if (symbol? procedure)
       (continue context procedure continuation)
       (compile-expression
        context
        procedure
        (continue
         (compilation-context-push-local context '$procedure)
         '$procedure
         (compile-unsafe-unbind continuation))))))

    (define (compile-expression context expression continuation)
     (cond
      ((symbol? expression)
       (code-rib
        get-instruction
        (compilation-context-resolve context expression)
        continuation))

      ((let ((predicate (maybe-car expression)))
        (and
         (eq? (maybe-car predicate) '$$lambda)
         (list? (caddr predicate))))
       (let ((predicate (car expression)))
        (compile-let
         context
         (map list (caddr predicate) (cdr expression))
         (cadddr predicate)
         continuation)))

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
          (let ((continuation
                 (if (null? continuation)
                  '()
                  (nop-rib continuation))))
           (code-rib
            if-instruction
            (compile-expression
             context
             (caddr expression)
             continuation)
            (compile-expression context (cadddr expression) continuation)))))

        (($$lambda)
         (let ((parameters (caddr expression)))
          (constant-rib
           (make-procedure
            (compile-arity
             (count-parameters parameters)
             (symbol? (last-cdr parameters)))
            (compile-sequence
             (compilation-context-append-locals
              context
              ; #f is for a frame.
              (reverse (cons #f (parameter-names parameters))))
             (cdddr expression)
             '())
            '())
           (if (null? (cadr expression))
            continuation
            (call-rib (compile-arity 1 #f) '$$close continuation)))))

        (($$libraries)
         (constant-rib (metadata-libraries (compilation-context-metadata context)) continuation))

        (($$macros)
         (constant-rib (metadata-macros (compilation-context-metadata context)) continuation))

        (($$optimizers)
         (constant-rib (metadata-optimizers (compilation-context-metadata context)) continuation))

        (($$quote)
         (constant-rib (cadr expression) continuation))

        (($$set!)
         (compile-expression
          context
          (caddr expression)
          (code-rib
           set-instruction
           (compilation-context-resolve
            (compilation-context-push-local context #f)
            (cadr expression))
           (compile-unspecified continuation))))

        (($$symbols)
         (constant-rib (metadata-symbols (compilation-context-metadata context)) continuation))

        (($$dynamic-symbols)
         (constant-rib (metadata-dynamic-symbols (compilation-context-metadata context)) continuation))

        (else
         (compile-call context expression #f continuation))))

      (else
       (constant-rib expression continuation))))))

(define backend
  '(
    ; Library system

    (define (library-symbol? name)
     (memv library-symbol-indicator (string->list (symbol->string name))))

    (define library-predicates '(define-library import))

    (define (expand-libraries expression)
     (let* ((context (make-library-context '() '()))
            (expressions (cdr expression))
            (sets
             (map
              parse-import-set
              (append-map
               (lambda (expression)
                (if (eq? (maybe-car expression) 'import)
                 (cdr expression)
                 '()))
               expressions))))
      (for-each
       (lambda (expression)
        (when (eq? (maybe-car expression) 'define-library)
         (add-library-definition! context expression)))
       expressions)
      (let ((expression
             (cons
              (car expression)
              (append
               (expand-library-bodies context (map car sets))
               (resolve-environment-symbols
                (let ((names (collect-imported-names context sets)))
                 (lambda (name)
                  (cond
                   ((assq name names) =>
                    cdr)
                   (else
                    name))))
                (filter
                 (lambda (expression)
                  (not (memq (maybe-car expression) library-predicates)))
                 expressions))))))
       (values
        expression
        (map-values
         library-exports
         (filter
          (lambda (pair)
           (member (car pair) (library-context-imported context)))
          (library-context-libraries context)))))))

    ; Macro system

    (define (expand-macros expression)
     (let* ((context (make-macro-context (make-macro-state '() '() '() '()) '()))
            (expression (expand-macro context expression))
            (state (macro-context-state context)))
      (values
       expression
       (reverse
        (filter
         (lambda (pair) (library-symbol? (car pair)))
         (macro-state-literals state)))
       (filter
        (lambda (name)
         (not
          (or
           (memq name (macro-state-static-symbols state))
           (built-in-symbol? name))))
        (macro-state-dynamic-symbols state)))))

    ; Optimization

    (define (optimize-custom expression)
     (let* ((context (make-optimization-context '() '()))
            (expression (optimize-custom-expression context expression)))
      (values expression (optimization-context-literals context))))

    ; Tree shaking

    (define-record-type tree-shake-context
     (make-tree-shake-context dependencies symbols)
     tree-shake-context?
     (dependencies tree-shake-context-dependencies)
     (symbols tree-shake-context-symbols tree-shake-context-set-symbols!))

    (define (tree-shake-context-append! context symbols)
     (for-each
      (lambda (symbol)
       (let ((symbols (tree-shake-context-symbols context)))
        (unless (memq symbol symbols)
         (tree-shake-context-set-symbols! context (cons symbol symbols))
         (tree-shake-context-append!
          context
          (or
           (assq symbol (tree-shake-context-dependencies context))
           '())))))
      symbols))

    (define (find-library-symbols locals expression)
     (cond
      ((eq? (maybe-car expression) '$$lambda)
       (find-library-symbols
        (append (parameter-names (cadr expression)) locals)
        (cddr expression)))
      ((pair? expression)
       (append
        (find-library-symbols locals (car expression))
        (find-library-symbols locals (cdr expression))))
      ((and
        (symbol? expression)
        (library-symbol? expression)
        (not (memq expression locals)))
       (list expression))
      (else
       '())))

    ; The false key is for symbols always required.
    (define (find-symbol-dependencies expression)
     (case (maybe-car expression)
      (($$begin)
       (fold-left
        (lambda (expression xs)
         (merge-multi-maps (find-symbol-dependencies expression) xs))
        '()
        (cdr expression)))
      (($$set!)
       (let ((symbol (cadr expression)))
        (append
         (if (library-symbol? symbol)
          '()
          (list (cons #f (list symbol))))
         (list (cons symbol (find-library-symbols '() (caddr expression)))))))
      (else
       (list
        (cons
         #f
         (find-library-symbols '() expression))))))

    (define (shake-sequence context locals expressions)
     (if (null? expressions)
      '()
      (let ((first (car expressions)))
       (let* ((expressions (shake-sequence context locals (cdr expressions)))
              (expression (shake-expression context locals first)))
        (cons expression expressions)))))

    (define (shake-expression context locals expression)
     (case (maybe-car expression)
      (($$lambda)
       (let ((parameters (cadr expression)))
        (cons
         '$$lambda
         (cons
          parameters
          (shake-sequence
           context
           (append (parameter-names parameters) locals)
           (cddr expression))))))
      (($$quote)
       expression)
      (($$set!)
       (let ((symbol (cadr expression)))
        (if (not
             (or
              (memq symbol locals)
              (memq symbol (tree-shake-context-symbols context))))
         #f
         expression)))
      (else
       (if (pair? expression)
        (shake-sequence context locals expression)
        expression))))

    (define (shake-tree features expression)
     (if (memq 'libraries features)
      expression
      (let* ((dependencies (find-symbol-dependencies expression))
             (context (make-tree-shake-context dependencies '())))
       ; There is no global symbol corresponding to `#f` if no library symbol is found.
       (tree-shake-context-append! context (or (assq #f dependencies) '()))
       (shake-expression context '() expression))))

    (define (shake-syntax-tree libraries macros)
     (let* ((dependencies
             (map-values
              (lambda (transformer)
               (let ((literals (cons (cadr transformer) (caddr transformer))))
                (append-map
                 (lambda (expression)
                  (find-library-symbols
                   (append (find-symbols (car expression)) literals)
                   (cadr expression)))
                 (cdddr transformer))))
              macros))
            (context (make-tree-shake-context dependencies '())))
      (tree-shake-context-append! context (map cdr (append-map cdr libraries)))
      (filter
       (lambda (pair)
        (memq (car pair) (tree-shake-context-symbols context)))
       macros)))

    ; Feature detection

    (define features
     '(($$dynamic-symbols . dynamic-symbols)
       ($$libraries . libraries)
       ($$macros . macros)
       ($$optimizers . optimizers)
       ($$symbols . symbols)))

    (define (detect-features expression)
     (cond
      ((and
        (pair? expression)
        (null? (cdr expression))
        (assq (car expression) features))
       =>
       (lambda (pair)
        (list (cdr pair))))
      ((pair? expression)
       (let loop ((expression expression) (features '()))
        (let ((features (unique (append features (detect-features (car expression))))))
         (if (pair? (cdr expression))
          (loop (cdr expression) features)
          features))))
      (else
       '())))

    ; Metadata

    (define-record-type metadata
     (make-metadata symbols libraries macros optimizers dynamic-symbols)
     metadata?
     (symbols metadata-symbols)
     (libraries metadata-libraries)
     (macros metadata-macros)
     (optimizers metadata-optimizers)
     (dynamic-symbols metadata-dynamic-symbols))

    (define (find-symbols expression)
     (cond
      ((symbol? expression)
       (list expression))

      ((vector? expression)
       (find-symbols (vector->list expression)))

      ((pair? expression)
       (append (find-symbols (car expression)) (find-symbols (cdr expression))))

      (else
       '())))

    (define (find-quoted-symbols expression)
     (define (find expression)
      (cond
       ((not (pair? expression))
        '())

       ((eq? (car expression) '$$quote)
        (find-symbols (cadr expression)))

       (else
        (append (find (car expression)) (find (cdr expression))))))

     (unique (find expression)))

    (define (compile-metadata
             features
             raw-libraries
             raw-macros
             raw-optimizers
             raw-dynamic-symbols
             expression)
     (define libraries (if (memq 'libraries features) raw-libraries '()))
     (define macros (if (memq 'macros features) raw-macros '()))
     (define optimizers (if (memq 'optimizers features) raw-optimizers '()))
     (define dynamic-symbols (if (memq 'dynamic-symbols features) raw-dynamic-symbols '()))

     (make-metadata
      (filter
       (lambda (symbol)
        (not (library-symbol? symbol)))
       (unique
        (append
         (find-quoted-symbols expression)
         (find-symbols libraries)
         (find-symbols macros)
         (find-symbols optimizers)
         (find-symbols dynamic-symbols))))
      libraries
      macros
      optimizers
      dynamic-symbols))

    ; Compilation

    (define (compile metadata expression)
     (compile-expression (make-compilation-context '() metadata) expression '()))

    ; Marshalling

    (define-record-type constant-set
     (make-constant-set simple complex)
     constant-set?
     (simple constant-set-simple constant-set-set-simple!)
     (complex constant-set-complex constant-set-set-complex!))

    (define (constant-set-append-simple! constant-set pair)
     (constant-set-set-simple!
      constant-set
      (cons pair (constant-set-simple constant-set))))

    (define (constant-set-append-complex! constant-set pair)
     (constant-set-set-complex!
      constant-set
      (cons pair (constant-set-complex constant-set))))

    (define-record-type marshal-context
     (make-marshal-context symbols constants continuations)
     marshal-context?
     (symbols marshal-context-symbols)
     (constants marshal-context-constants)
     (continuations marshal-context-continuations marshal-context-set-continuations!))

    (define (nop-code? codes)
     (and
      (rib? codes)
      (= (rib-tag codes) nop-instruction)))

    (define vector-factor 64)

    (define (list->vector-nodes xs)
     (let ((xs
            (let loop ((xs xs))
             (if (null? xs)
              '()
              (let ((ys
                     (let loop ((xs xs) (length 0))
                      (if (and (pair? xs) (< length vector-factor))
                       (cons (car xs) (loop (cdr xs) (+ length 1)))
                       '()))))
               (cons ys (loop (list-tail xs (length ys)))))))))
      (case (length xs)
       ((0)
        '())
       ((1)
        (car xs))
       (else
        (list->vector-nodes xs)))))

    (define (marshal-constant context value)
     (define (marshal value)
      (marshal-rib context value #t))

     (cond
      ((null? value)
       (data-rib null-type 0 (cons-rib 0 0)))

      ((boolean? value)
       (if value
        (data-rib boolean-type 0 (marshal '()))
        (data-rib boolean-type (marshal '()) (marshal #t))))

      ((symbol? value)
       (data-rib
        symbol-type
        (marshal #f)
        (marshal
         (let ((symbols (marshal-context-symbols context)))
          (if (or (not symbols) (memq value symbols))
           (resolve-symbol-string value)
           "")))))

      ((char? value)
       (data-rib char-type (char->integer value) (marshal '())))

      ((string? value)
       (data-rib
        string-type
        (string-length value)
        (marshal (map char->integer (string->list value)))))

      ((pair? value)
       (cons-rib (marshal (car value)) (marshal (cdr value))))

      ((vector? value)
       (data-rib
        vector-type
        (vector-length value)
        (marshal (list->vector-nodes (vector->list value)))))

      ((bytevector? value)
       (data-rib
        bytevector-type
        (bytevector-length value)
        (marshal (list->vector-nodes (bytes->list value)))))

      (else
       (error "invalid type"))))

    (define (marshal-unique-constant context value)
     (define constant-set (marshal-context-constants context))

     (cond
      ((or
        (assq value (constant-set-simple constant-set))
        (assoc value (constant-set-complex constant-set)))
       =>
       cdr)
      (else
       (let ((marshalled (marshal-constant context value)))
        ((if (or
              (null? value)
              (boolean? value)
              (symbol? value))
          constant-set-append-simple!
          constant-set-append-complex!)
         constant-set
         (cons value marshalled))
        marshalled))))

    (define (marshal-rib context value data)
     (define (marshal value data)
      (marshal-rib context value data))

     (cond
      ((number? value)
       value)

      ((or data (null? value))
       (cond
        ((target-procedure? value)
         (unless (null? (rib-cdr value))
          (error "invalid environment"))
         (data-rib procedure-type (marshal (rib-car value) #f) (marshal '() #t)))

        ((or
          (null? value)
          (boolean? value)
          (char? value)
          (string? value)
          (symbol? value))
         (marshal-unique-constant context value))

        ((or (bytevector? value) (pair? value) (vector? value))
         (marshal-constant context value))

        (else
         (error "invalid type"))))

      ((nop-code? value)
       (cond
        ((assq value (marshal-context-continuations context)) =>
         cdr)

        (else
         (let ((continuation (nop-rib (marshal (rib-cdr value) #f))))
          (marshal-context-set-continuations!
           context
           (cons (cons value continuation) (marshal-context-continuations context)))
          continuation))))

      (else
       (rib
        (marshal (rib-car value) (not (= (rib-tag value) if-instruction)))
        (marshal (rib-cdr value) data)
        (rib-tag value)))))

    (define (marshal options metadata codes)
     (marshal-rib
      (make-marshal-context
       (and
        (not (memq 'debug options))
        (append
         (metadata-symbols metadata)
         (append-map
          (lambda (pair) (map cdr (cdr pair)))
          (metadata-libraries metadata))))
       (make-constant-set '() '())
       '())
      codes
      #f))

    ; Compression

    (define window-size 256)
    (define minimum-match 2) ; exclusive
    (define maximum-match 128) ; inclusive

    ;; Compressor

    (define-record-type compressor
     (make-compressor buffer current last back ahead)
     compressor?
     (buffer compressor-buffer compressor-set-buffer!)
     (current compressor-current compressor-set-current!)
     (last compressor-last compressor-set-last!)
     (back compressor-back compressor-set-back!)
     (ahead compressor-ahead compressor-set-ahead!))

    (define (compressor-push! compressor x)
     (let ((xs (list x)))
      (if (pair? (compressor-buffer compressor))
       (set-cdr! (compressor-last compressor) xs)
       (begin
        (compressor-set-buffer! compressor xs)
        (compressor-set-current! compressor xs)))
      (compressor-set-last! compressor xs)
      (compressor-set-ahead!
       compressor
       (+ (compressor-ahead compressor) 1))))

    (define (compressor-pop! compressor n)
     (let ((xs (compressor-current compressor)))
      (compressor-set-current! compressor (list-tail xs n))
      (compressor-set-ahead! compressor (- (compressor-ahead compressor) n))
      (compressor-set-back! compressor (+ (compressor-back compressor) n))

      (let ((d (- (compressor-back compressor) window-size)))
       (when (positive? d)
        (compressor-set-buffer!
         compressor
         (list-tail (compressor-buffer compressor) d))
        (compressor-set-back! compressor window-size)))

      (car xs)))

    (define (compressor-write-next compressor)
     (let ((current (compressor-current compressor))
           (back (compressor-back compressor)))
      (let* ((match
              (let loop ((xs (compressor-buffer compressor))
                         (i 0)
                         (match '(0 . 0)))
               (if (< i back)
                (let ((m
                       (do ((xs xs (cdr xs))
                            (ys current (cdr ys))
                            (n 0 (+ n 1)))
                        ((not
                          (and
                           (pair? ys)
                           (eq? (car xs) (car ys))
                           (< n maximum-match)))
                         n))))
                 (loop (cdr xs) (+ i 1) (if (< m (cdr match)) match (cons i m))))
                match)))
             (n (cdr match)))
       (if (> n minimum-match)
        (begin
         (write-u8 (+ 1 (* 2 (- n 1))))
         (write-u8 (- back (car match) 1))
         (compressor-pop! compressor n))
        (write-u8 (* 2 (compressor-pop! compressor 1)))))))

    (define (compressor-write compressor x)
     (compressor-push! compressor x)

     (when (> (compressor-ahead compressor) maximum-match)
      (compressor-write-next compressor)))

    (define (compressor-flush compressor)
     (do ()
      ((null? (compressor-current compressor)))
      (compressor-write-next compressor)))

    ; Encoding

    ;; Context

    (define-record-type encode-context
     (make-encode-context compressor dictionary counts null)
     encode-context?
     (compressor encode-context-compressor)
     (dictionary encode-context-dictionary encode-context-set-dictionary!)
     (counts encode-context-counts encode-context-set-counts!)
     (null encode-context-null))

    (define (encode-context-push! context value)
     (encode-context-set-dictionary!
      context
      (cons value (encode-context-dictionary context))))

    (define (encode-context-remove! context index)
     (let* ((dictionary (cons #f (encode-context-dictionary context)))
            (pair (list-tail dictionary index)))
      (set-cdr! pair (cddr pair))
      (encode-context-set-dictionary! context (cdr dictionary))))

    (define (encode-context-index context value)
     (memq-index value (encode-context-dictionary context)))

    (define (encode-context-find-count context value)
     (assq value (encode-context-counts context)))

    ;; Codes

    (define integer-base 64)
    (define number-base 16)
    (define tag-base 16)
    (define share-base 31)

    (define (shared-value? value)
     (and
      (rib? value)
      (memq
       (rib-tag value)
       (list
        nop-instruction ; for continuations
        boolean-type
        char-type
        null-type
        string-type
        symbol-type))))

    (define (strip-nop-instructions codes)
     (if (and (nop-code? codes))
      (strip-nop-instructions (rib-cdr codes))
      codes))

    (define (increment-count! context value)
     (cond
      ((encode-context-find-count context value) =>
       (lambda (pair)
        (set-cdr! pair (+ 1 (cdr pair)))))

      (else
       (encode-context-set-counts!
        context
        (cons (cons value 1) (encode-context-counts context))))))

    (define (decrement-count! pair)
     (set-cdr! pair (- (cdr pair) 1)))

    (define (count-ribs! context codes)
     (define (count-data! value)
      (when (rib? value)
       (unless (and (shared-value? value) (encode-context-find-count context value))
        ((if (target-procedure? value) count-code! count-data!) (rib-car value))
        (count-data! (rib-cdr value)))
       (when (shared-value? value)
        (increment-count! context value))))

     (define (count-code! codes)
      (cond
       ((number? codes)
        #f)

       ((eq? codes (encode-context-null context))
        (count-data! codes))

       ((nop-code? codes)
        (let* ((codes (strip-nop-instructions codes))
               (counted (encode-context-find-count context codes)))
         (increment-count! context codes)
         (unless counted
          (count-code! codes))))

       (else
        ((if (= (rib-tag codes) if-instruction) count-code! count-data!)
         (rib-car codes))
        (count-code! (rib-cdr codes)))))

     (count-code! codes))

    (define (fraction x)
     (- x (floor x)))

    ; We need to fit 64-bit floating-point numbers' sign, mantissa, and exponent into its mantissa...
    ; TODO Shouldn't this exponent be a higher number like 37 = 52 - 12 - 3?
    (define maximum-float-integer (expt 2 30))

    ; Lossy decomposition of floating-point numbers into a signed mantissa and an exponent.
    (define (decompose-float x)
     (define (mantissa y)
      (/ x (expt 2 y)))

     (do ((y (log x 2) (- y 1)))
      ((or
        (< (fraction (mantissa (floor y))) epsilon)
        (> (mantissa (+ y 1)) maximum-float-integer))
       (let ((y (floor y)))
        (values (exact (round (mantissa y))) (exact y))))))

    (define (encode-integer-part integer base bit)
     (+ (if bit 0 1) (* 2 (modulo integer base))))

    (define (encode-integer-parts integer base)
     (let ((rest (quotient integer base)))
      (values
       (encode-integer-part integer base (zero? rest))
       rest)))

    ; Unlike Ribbit Scheme, we use the forward encoding algorithm. So this integer encoding also proceeds forward.
    ; Therefore, we need to adopt little endianness like the `varint` in Protocol Buffer.
    (define (encode-integer-tail context x)
     (do ((x x (quotient x integer-base)))
      ((zero? x))
      (compressor-write
       (encode-context-compressor context)
       (encode-integer-part
        x
        integer-base
        (zero? (quotient x integer-base))))))

    (define (encode-number x)
     (cond
      ((and (integer? x) (negative? x))
       (+ 1 (* 4 (abs x))))

      ((integer? x)
       (* 2 x))

      (else
       (let-values (((m e) (decompose-float (abs x))))
        (+
         3
         (*
          4
          (+
           (if (negative? x) 1 0)
           (* 2 (+ e 1023))
           (* 4096 m))))))))

    (define (encode-rib context value)
     (define compressor (encode-context-compressor context))

     (if (rib? value)
      (let* ((value (strip-nop-instructions value))
             (entry (encode-context-find-count context value)))
       (cond
        ((and entry (encode-context-index context value)) =>
         (lambda (index)
          (decrement-count! entry)
          (let ((removed (zero? (cdr entry))))
           (encode-context-remove! context index)
           (unless removed
            (encode-context-push! context value))
           (let-values (((head tail)
                         (encode-integer-parts
                          (+ (* 2 index) (if removed 0 1))
                          share-base)))
            (compressor-write compressor (* 2 (+ 1 head)))
            (encode-integer-tail context tail)))))
        (else
         (encode-rib context (rib-car value))
         (encode-rib context (rib-cdr value))

         (let-values (((head tail) (encode-integer-parts (rib-tag value) tag-base)))
          (compressor-write compressor (+ 1 (* 4 head)))
          (encode-integer-tail context tail))

         (when entry
          (encode-context-push! context value)
          (decrement-count! entry)
          (compressor-write compressor 0)))))
      (let-values (((head tail) (encode-integer-parts (encode-number value) number-base)))
       (compressor-write compressor (+ 3 (* 4 head)))
       (encode-integer-tail context tail))))

    ;; Primitives

    (define (build-primitive primitive continuation)
     (code-rib
      constant-instruction
      (data-rib procedure-type (cadr primitive) '())
      (code-rib
       set-instruction
       (car primitive)
       continuation)))

    ; TODO Consider moving this logic to marshalling.
    (define (build-primitives primitives continuation)
     (if (null? primitives)
      continuation
      (build-primitive
       (car primitives)
       (build-primitives (cdr primitives) continuation))))

    ;; Main

    (define (encode codes)
     (let ((context
            (make-encode-context
             (make-compressor '() '() #f 0 0)
             '()
             '()
             (rib-car (rib-car codes)))))
      (count-ribs! context codes)
      (encode-context-set-counts!
       context
       (filter
        (lambda (pair) (> (cdr pair) 1))
        (encode-context-counts context)))
      (encode-rib context codes)
      (compressor-flush (encode-context-compressor context))

      (let ((size (length (encode-context-dictionary context))))
       (unless (zero? size)
        (error "dictionary not empty" size)))

      (do ((counts (encode-context-counts context) (cdr counts)))
       ((null? counts))
       (unless (zero? (cdar counts))
        (error "invalid constant count" (map cdr counts))))))

    ; Main

    (define (main options source)
     (define expression1 (include-files source))
     (define-values (expression2 libraries) (expand-libraries expression1))
     (define-values (expression3 macros dynamic-symbols) (expand-macros expression2))
     (define-values (expression4 optimizers) (optimize-custom expression3))
     (define features (detect-features expression4))
     (define expression5 (if (memq 'shake-tree options)
                          (shake-tree features expression4)
                          expression4))
     (define expression6 (optimize-begin expression5))
     (define expression7 (analyze-free-variables expression6))

     (define metadata
      (compile-metadata
       features
       libraries
       (shake-syntax-tree libraries macros)
       optimizers
       dynamic-symbols
       expression7))

     (encode
      (marshal
       options
       metadata
       (cons-rib
        #f
        (build-primitives
         primitives
         (compile metadata expression7))))))

    main))

(define compiler
  `(let ()
    (cond-expand
     (stak
      (define cons-rib cons)
      (define rib-car car)
      (define rib-cdr cdr)
      (define target-procedure? procedure?))

     (else
      (define-record-type *rib*
       (rib car cdr tag)
       rib?
       (car rib-car)
       (cdr rib-cdr)
       (tag rib-tag))

      (define (cons-rib car cdr)
       (rib car cdr pair-type))

      (define (target-procedure? value)
       (and (rib? value) (eq? (rib-tag value) procedure-type)))

      (define symbol-id 0)

      (define (string->uninterned-symbol name)
       (set! symbol-id (+ symbol-id 1))
       (string->symbol (string-append (number->string symbol-id 32) name)))))

    ,@frontend
    ,@backend))

; Source code reading

(define (read-all)
  (let ((x (read)))
    (if (eof-object? x)
      '()
      (cons x (read-all)))))

(define (read-source)
  (cons
    '$$begin
    ; Keep an invariant that a `begin` body must not be empty.
    (cons #f (read-all))))

; Inception

(define (incept expression)
  (cond
    ((not (pair? expression))
      expression)
    ((and
        (pair? (car expression))
        (null? (cdar expression))
        (eq? (caar expression) '$$compiler))
      (cons
        `(define-library (stak compile)
          (export compile)

          (import
           (scheme base)
           (scheme cxr)
           (scheme file)
           (scheme read)
           (only (stak base) rib string->uninterned-symbol))

          (begin
           (define compile
            (let ()
             (define cons-rib cons)
             (define rib-car car)
             (define rib-cdr cdr)

             ,@frontend

             ; Disable unused functionalities.
             (define dummy
              (let ((set-nothing (lambda xs #f)))
               (set! nop-rib (lambda (continuation) continuation))
               (set! macro-state-set-literals! set-nothing)
               (set! macro-state-set-static-symbols! set-nothing)
               (set! macro-state-set-dynamic-symbols! set-nothing)
               (set! optimization-context-set-literals! set-nothing)))

             ; Library system

             (define (append-imports old-imports new-imports)
              (fold-left
               (lambda (name names)
                (if (member name names)
                 names
                 (cons name names)))
               old-imports
               new-imports))

             (define expand-libraries
              (let ((context
                     (make-library-context
                      (map-values (lambda (exports) (make-library exports '() '())) ($$libraries))
                      '())))
               (lambda (imports symbol-table expression)
                (case (maybe-car expression)
                 ((define-library)
                  (add-library-definition! context expression)
                  (values #f imports))
                 ((import)
                  (let ((imports (append-imports imports (cdr expression))))
                   (values
                    (cons
                     '$$begin
                     (append
                      (expand-library-bodies
                       context
                       (map car (map parse-import-set imports)))
                      (list #f)))
                    imports)))
                 (else
                  (values
                   (resolve-environment-symbols
                    (let ((names (collect-imported-names
                                  context
                                  (map parse-import-set imports))))
                     (lambda (name)
                      (cond
                       ((assq name names) =>
                        cdr)
                       (else
                        (string->symbol (symbol->string name) symbol-table)))))
                    expression)
                   imports))))))

             ; Macro system

             (define expand-macros
              (let ((context
                     (make-macro-context
                      (make-macro-state '() '() '() '())
                      '())))
               (for-each
                (lambda (pair)
                 (macro-context-set-global!
                  context
                  (car pair)
                  (make-transformer context (cdr pair))))
                ($$macros))
               (lambda (expression)
                (expand-macro context expression))))

             ; Optimization

             (define optimize
              (let ((context
                     (make-optimization-context
                      (map-values make-optimizer ($$optimizers))
                      '())))
               (lambda (expression)
                (optimize-begin (optimize-custom-expression context expression)))))

             ; Compilation

             (define (compile expression)
              (compile-expression (make-compilation-context '() #f) expression '()))

             (lambda (imports symbol-table expression)
              (let-values (((expression imports)
                            (expand-libraries
                             imports
                             symbol-table
                             (include-files expression))))
               (values
                (make-procedure
                 (compile-arity 0 #f)
                 (compile
                  (analyze-free-variables
                   (optimize
                    (expand-macros expression))))
                 '())
                imports)))))))
        (cdr expression)))
    (else
      (cons
        (incept (car expression))
        (incept (cdr expression))))))

; Main

(define (main)
  (define compile
    (eval
      compiler
      (environment
        '(scheme base)
        '(scheme cxr)
        '(scheme file)
        '(scheme inexact)
        '(scheme read)
        '(scheme write))))

  (define arguments (command-line))

  (when (or
         (member "-h" arguments)
         (member "--help" arguments))
    (write-string "The Stak Scheme bytecode compiler.\n\n")
    (write-string "Usage: stak-compile < SOURCE_FILE > BYTECODE_FILE\n")
    (exit))

  (compile
    (list
      (and (member "--debug" arguments) 'debug)
      (and (member "--shake-tree" arguments) 'shake-tree))
    (incept (read-source))))

(main)
