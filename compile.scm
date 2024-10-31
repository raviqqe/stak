; Stak Scheme compiler.
;
; All compiler-internal variables contain at least one `$` or `%` character in their names.

(import
  (scheme base)
  (scheme cxr)
  (scheme inexact)
  (scheme lazy)
  (scheme read)
  (scheme write))

(cond-expand
  (stak
    (define cons-rib cons)
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
      (and (rib? value) (eqv? (rib-tag value) procedure-type)))

    (define string->uninterned-symbol string->symbol)))

; Constants

(define default-constants
  '((#f . $$false)
    (#t . $$true)
    (() . $$null)
    ; It is fine to have a key duplicate with `false`'s because it is never hit.
    (#f . $$rib)))

(define default-symbols (map cdr default-constants))

; Instructions

(define constant-instruction 0)
(define get-instruction 1)
(define set-instruction 2)
(define if-instruction 3)
(define nop-instruction 4)
(define call-instruction 5)

; Primitives

(define primitives
  '(($$rib 0)
    ($$cons 1)
    ($$close 2)
    ($$cdr 5)
    ($$< 11)
    ($$+ 12)
    ($$- 13)
    ($$* 14)
    ($$/ 15)
    ($$exp 17)
    ($$log 18)))

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

(define (code-rib tag car cdr)
  (rib car cdr tag))

(define (call-rib arity procedure continuation)
  (code-rib (+ call-instruction arity) procedure continuation))

(define (constant-rib constant continuation)
  (code-rib constant-instruction constant continuation))

(define (data-rib type car cdr)
  (rib car cdr type))

(define (make-procedure arity code environment)
  (data-rib procedure-type (cons-rib arity code) environment))

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
    (let ((x (car xs))
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

(define (list-head xs n)
  (if (zero? n)
    '()
    (cons
      (car xs)
      (list-head (cdr xs) (- n 1)))))

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

(define (flat-map f xs)
  (apply append (map f xs)))

(define (relaxed-length xs)
  (do ((xs xs (cdr xs)) (y 0 (+ y 1)))
    ((not (pair? xs))
      y)))

(define (relaxed-deep-map f xs)
  (if (pair? xs)
    (cons
      (relaxed-deep-map f (car xs))
      (relaxed-deep-map f (cdr xs)))
    (f xs)))

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

(define (map-values f xs)
  (map (lambda (pair) (cons (car pair) (f (cdr pair)))) xs))

(define (filter-values f xs)
  (filter (lambda (pair) (f (cdr pair))) xs))

; TODO Set a true machine epsilon.
(define epsilon
  (let ((x (/ 1 10000000 100000000)))
    (if (zero? x) 1 x)))

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

(define (symbol-append . xs)
  (string->symbol (apply string-append (map symbol->string xs))))

(define (id->string id)
  (number->string id 32))

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

; Library system

;; Types

(define-record-type library
  (make-library id name exports imports body symbols)
  library?
  (id library-id)
  (name library-name)
  (exports library-exports)
  (imports library-imports)
  (body library-body)
  (symbols library-symbols))

(define-record-type library-state
  (make-library-state library imported)
  library-state?
  (library library-state-library)
  (imported library-state-imported library-state-set-imported!))

(define-record-type library-context
  (make-library-context libraries name-maps)
  library-context?
  (libraries library-context-libraries library-context-set-libraries!)
  (name-maps library-context-name-maps library-context-set-name-maps!))

(define (library-context-assoc context name)
  (cond
    ((assoc name (library-context-libraries context)) =>
      cdr)

    (else
      (error "unknown library" name))))

(define (library-context-id context)
  (length (library-context-libraries context)))

(define (library-context-find context name)
  (library-state-library (library-context-assoc context name)))

(define (library-context-add! context library)
  (library-context-set-libraries!
    context
    (cons
      (cons
        (library-name library)
        (make-library-state library #f))
      (library-context-libraries context))))

(define (library-context-import! context name)
  (let* ((state (library-context-assoc context name))
         (imported (library-state-imported state)))
    (library-state-set-imported! state #t)
    imported))

;; Procedures

(define library-symbol-separator #\%)

(define (library-symbol? name)
  (memv-position
    library-symbol-separator
    (string->list (symbol->string name))))

(define (build-library-name id name)
  (string-append
    (id->string id)
    (list->string (list library-symbol-separator))
    (symbol->string name)))

(define (resolve-library-symbol name)
  (let* ((string (symbol->string name))
         (position (memv-position library-symbol-separator (string->list string))))
    (if position
      (string->symbol (string-copy string (+ position 1)))
      name)))

(define (rename-library-symbol context id name)
  (if (or
       (not id)
       (let ((name (symbol->string name)))
         (equal? (substring name 0 (min 2 (string-length name))) "$$")))
    name
    (let* ((maps (library-context-name-maps context))
           (pair (or (assq id maps) (cons id '())))
           (names (cdr pair)))
      (when (null? names)
        (library-context-set-name-maps! context (cons pair maps)))
      (let ((names (cdr pair)))
        (cond
          ((assq name names) =>
            cdr)

          (else
            (let ((renamed (string->uninterned-symbol (build-library-name id name))))
              (set-cdr! pair (cons (cons name renamed) names))
              renamed)))))))

(define (expand-import-set context importer-id importer-symbols qualify set)
  (define (expand qualify)
    (expand-import-set context importer-id importer-symbols qualify (cadr set)))

  (case (predicate set)
    ((except)
      (let ((names (cddr set)))
        (expand
          (lambda (name)
            (if (memq name names)
              #f
              (qualify name))))))

    ((only)
      (let ((names (cddr set)))
        (expand
          (lambda (name)
            (if (memq name names)
              (qualify name)
              #f)))))

    ((prefix)
      (expand
        (lambda (name)
          (qualify (symbol-append (caddr set) name)))))

    ((rename)
      (expand
        (lambda (name)
          (qualify
            (cond
              ((assq name (cddr set)) =>
                cadr)

              (else
                name))))))

    ((shake)
      (expand
        (lambda (name)
          (if (or
               (not importer-symbols)
               (memq name (force importer-symbols)))
            (qualify name)
            #f))))

    (else
      (let ((library (library-context-find context set)))
        (append
          (if (library-context-import! context set)
            '()
            (append
              (expand-import-sets
                context
                (library-id library)
                (library-symbols library)
                (library-imports library))
              (library-body library)))
          (flat-map
            (lambda (names)
              (let ((name (qualify (car names))))
                (if name
                  (list
                    (list
                      '$$alias
                      (rename-library-symbol context importer-id name)
                      (cdr names)))
                  '())))
            (library-exports library)))))))

(define (expand-import-sets context importer-id importer-symbols sets)
  (flat-map
    (lambda (set) (expand-import-set context importer-id importer-symbols (lambda (x) x) set))
    sets))

(define (expand-library-expression context body-symbols expression)
  (case (and (pair? expression) (car expression))
    ((define-library)
      (let* ((collect-bodies
               (lambda (predicate)
                 (flat-map
                   cdr
                   (filter
                     (lambda (body) (eq? (car body) predicate))
                     (cddr expression)))))
             (id (library-context-id context))
             (exports (collect-bodies 'export))
             (bodies (collect-bodies 'begin)))
        (library-context-add!
          context
          (make-library
            id
            (cadr expression)
            (map
              (lambda (name)
                (if (eq? (predicate name) 'rename)
                  (cons (caddr name) (rename-library-symbol context id (cadr name)))
                  (cons name (rename-library-symbol context id name))))
              exports)
            (collect-bodies 'import)
            (relaxed-deep-map
              (lambda (value)
                (if (symbol? value)
                  (rename-library-symbol context id value)
                  value))
              bodies)
            (delay (deep-unique (cons exports bodies)))))
        '()))

    ((import)
      (expand-import-sets context #f body-symbols (cdr expression)))

    (else
      (list expression))))

(define library-predicates '(define-library import))

(define (expand-libraries expression)
  (let* ((context (make-library-context '() '()))
         (body-symbols
           (delay
             (deep-unique
               (filter
                 (lambda (expression)
                   (not (and (pair? expression) (memq (car expression) library-predicates))))
                 (cdr expression)))))
         (expression
           (cons
             (car expression)
             (flat-map
               (lambda (expression)
                 (expand-library-expression context body-symbols expression))
               (cdr expression)))))
    (values expression context)))

; Macro system

;; Types

(define-record-type macro-state
  (make-macro-state id literals)
  macro-state?
  (id macro-state-id macro-state-set-id!)
  (literals macro-state-literals macro-state-set-literals!))

(define-record-type macro-context
  (make-macro-context state environment)
  macro-context?
  (state macro-context-state)
  (environment macro-context-environment macro-context-set-environment!))

(define (macro-context-append context pairs)
  (make-macro-context
    (macro-context-state context)
    (append pairs (macro-context-environment context))))

(define (macro-context-set! context name denotation)
  (let* ((environment (macro-context-environment context))
         (pair (assq name environment)))
    (when pair (set-cdr! pair denotation))
    pair))

(define (macro-context-set-last! context name denotation)
  (unless (macro-context-set! context name denotation)
    (let ((environment (macro-context-environment context))
          (tail (list (cons name denotation))))
      (if (null? environment)
        (macro-context-set-environment! context tail)
        (set-last-cdr! environment tail)))))

(define (macro-context-generate-id! context)
  (let* ((state (macro-context-state context))
         (id (macro-state-id state)))
    (macro-state-set-id! state (+ id 1))
    id))

(define (macro-context-append-literal! context name syntax)
  (define state (macro-context-state context))

  (macro-state-set-literals!
    state
    (cons
      (cons name syntax)
      (macro-state-literals state))))

(define-record-type rule-context
  (make-rule-context definition-context use-context ellipsis literals)
  rule-context?
  (definition-context rule-context-definition-context)
  (use-context rule-context-use-context)
  (ellipsis rule-context-ellipsis)
  (literals rule-context-literals))

;; Procedures

(define primitive-procedures
  (map
    (lambda (x)
      (cons
        ; `0` is always the library ID of `(stak base)`.
        (build-library-name 0 x)
        (symbol-append '$$ x)))
    '(+ - * / <)))

(define (optimize expression)
  (let ((predicate (predicate expression)))
    (cond
      ((eq? predicate '$$begin)
        ; Omit top-level constants.
        (cons '$$begin
          (let loop ((expressions (cdr expression)))
            (let ((expression (car expressions))
                  (expressions (cdr expressions)))
              (cond
                ((null? expressions)
                  (list expression))

                ((pair? expression)
                  (cons expression (loop expressions)))

                (else
                  (loop expressions)))))))

      ((and
          (list? expression)
          (= (length expression) 3)
          (symbol? predicate)
          (assoc (symbol->string predicate) primitive-procedures))
        =>
        (lambda (pair)
          (cons (cdr pair) (cdr expression))))

      (else
        expression))))

(define (resolve-denotation context expression)
  (cond
    ((assq expression (macro-context-environment context)) =>
      cdr)

    (else
      expression)))

(define (rename-variable context name)
  ; Share tails when appending strings.
  (string->uninterned-symbol
    (string-append
      (id->string (macro-context-generate-id! context))
      "$"
      (symbol->string name))))

(define (find-pattern-variables ellipsis bound-variables pattern)
  (define excluded-variables (cons ellipsis bound-variables))

  (let loop ((pattern pattern) (variables '()))
    (cond
      ((pair? pattern)
        (loop
          (car pattern)
          (loop
            (cdr pattern)
            variables)))

      ((and (symbol? pattern) (not (memq pattern excluded-variables)))
        (cons pattern variables))

      (else
        variables))))

(define-record-type ellipsis-match
  (make-ellipsis-match value)
  ellipsis-match?
  (value ellipsis-match-value))

(define-record-type ellipsis-pattern
  (make-ellipsis-pattern element variables)
  ellipsis-pattern?
  (element ellipsis-pattern-element)
  (variables ellipsis-pattern-variables))

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
        (make-ellipsis-pattern
          (compile (car pattern))
          (find-pattern-variables ellipsis literals (car pattern)))
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
        (memq pattern (rule-context-literals context)))
      (unless (eq?
               (resolve-denotation (rule-context-use-context context) expression)
               (resolve-denotation (rule-context-definition-context context) pattern))
        (raise #f))
      '())

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
  (let-values (((transformer definition-context) (expand-outer-macro definition-context transformer)))
    (case (resolve-denotation definition-context (predicate transformer))
      (($$syntax-rules)
        (let* ((ellipsis (resolve-denotation definition-context (cadr transformer)))
               (literals (caddr transformer))
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
                    (rule-context (make-rule-context definition-context use-context ellipsis literals)))
                (guard (value
                        ((not value)
                          (loop (cdr rules))))
                  (let* ((matches (match-pattern rule-context (car rule) expression))
                         (template (cadr rule))
                         (names
                           (map
                             (lambda (name) (cons name (rename-variable use-context name)))
                             (find-pattern-variables ellipsis (append literals (map car matches)) template))))
                    (values
                      (fill-template rule-context (append names matches) template)
                      (macro-context-append
                        use-context
                        (map
                          (lambda (pair)
                            (cons
                              (cdr pair)
                              (resolve-denotation definition-context (car pair))))
                          names))))))))))

      (else
        (error "unsupported macro transformer" transformer)))))

(define (expand-outer-macro context expression)
  (if (pair? expression)
    (let ((value (resolve-denotation context (car expression))))
      (if (procedure? value)
        (let-values (((expression context) (value context expression)))
          (expand-outer-macro context expression))
        (values expression context)))
    (values expression context)))

; https://www.researchgate.net/publication/220997237_Macros_That_Work
(define (expand-macro context expression)
  (define (expand expression)
    (expand-macro context expression))

  (define (resolve name)
    (resolve-denotation context name))

  (optimize
    (cond
      ((symbol? expression)
        (let ((value (resolve expression)))
          (when (procedure? value)
            (error "invalid syntax" expression))
          value))

      ((pair? expression)
        (case (resolve (car expression))
          (($$alias)
            (macro-context-set-last!
              context
              (cadr expression)
              (resolve (caddr expression)))
            (macro-context-append-literal!
              context
              (cadr expression)
              (caddr expression))
            #f)

          (($$define)
            (let ((name (cadr expression)))
              (macro-context-set! context name name)
              (expand (cons '$$set! (cdr expression)))))

          (($$define-syntax)
            (macro-context-set-last!
              context
              (cadr expression)
              (make-transformer context (caddr expression)))
            (macro-context-append-literal!
              context
              (cadr expression)
              (caddr expression))
            #f)

          (($$lambda)
            (let* ((parameters (cadr expression))
                   (context
                     (macro-context-append
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
                         (lambda (value) #f)
                         bindings))))
              (for-each
                (lambda (pair)
                  (macro-context-set!
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
                    (resolve-library-symbol value)
                    value))
                (cdr expression))))

          (else =>
            (lambda (value)
              (if (procedure? value)
                (let-values (((expression context) (value context expression)))
                  (expand-macro context expression))
                (map expand expression))))))

      (else
        expression))))

(define (expand-macros expression)
  (let* ((context (make-macro-context (make-macro-state 0 '()) '()))
         (expression (expand-macro context expression)))
    (values expression context)))

; Compilation

;; Context

(define-record-type compilation-context
  (make-compilation-context environment symbols libraries macros)
  compilation-context?
  (environment compilation-context-environment)
  (symbols compilation-context-symbols)
  (libraries compilation-context-libraries)
  (macros compilation-context-macros))

(define (compilation-context-append-locals context variables)
  (make-compilation-context
    (append variables (compilation-context-environment context))
    (compilation-context-symbols context)
    (compilation-context-libraries context)
    (compilation-context-macros context)))

(define (compilation-context-push-local context variable)
  (compilation-context-append-locals context (list variable)))

; If a variable is not in environment, it is considered to be global.
(define (compilation-context-resolve context variable)
  (or (memv-position variable (compilation-context-environment context)) variable))

;; Procedures

(define (find-quoted-symbols expression)
  (cond
    ((symbol? expression)
      (list expression))

    ((vector? expression)
      (find-quoted-symbols (vector->list expression)))

    ((pair? expression)
      (append (find-quoted-symbols (car expression)) (find-quoted-symbols (cdr expression))))

    (else
      '())))

(define (find-symbols expression)
  (define (find expression)
    (cond
      ((and
          (symbol? expression)
          (let ((string (symbol->string expression)))
            (and
              (> (string-length string) 1)
              (eqv? (string-ref string 0) #\$)
              (eqv? (string-ref string 1) #\$))))
        (list expression))

      ((not (pair? expression))
        '())

      ((eq? (car expression) '$$quote)
        (find-quoted-symbols (cadr expression)))

      (else
        (apply append (map find expression)))))

  (unique (find expression)))

(define (compile-arity argument-count variadic)
  (+
    (* 2 argument-count)
    (if variadic 1 0)))

(define (compile-primitive-call name continuation)
  (call-rib
    (compile-arity
      (case name
        (($$close $$cdr $$exp $$log)
          1)

        (($$cons $$- $$*)
          2)

        (($$rib)
          3)

        (else
          (error "unknown primitive" name)))
      #f)
    name
    continuation))

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
          (compile-unbind continuation))))))

(define (compile-unbind continuation)
  (if (null? continuation)
    continuation
    (code-rib set-instruction 1 continuation)))

(define (compile-expression context expression continuation)
  (cond
    ((symbol? expression)
      (code-rib
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
            (let ((continuation
                    (if (null? continuation)
                      '()
                      (code-rib nop-instruction 0 continuation))))
              (code-rib
                if-instruction
                (compile-expression
                  context
                  (caddr expression)
                  continuation)
                (compile-expression context (cadddr expression) continuation)))))

        (($$lambda)
          (let ((parameters (cadr expression)))
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
                  (cddr expression)
                  '())
                '())
              (compile-primitive-call '$$close continuation))))

        (($$libraries)
          (constant-rib (compilation-context-libraries context) continuation))

        (($$macros)
          (constant-rib (compilation-context-macros context) continuation))

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
          (constant-rib (compilation-context-symbols context) continuation))

        (else
          (compile-call context expression #f continuation))))

    (else
      (constant-rib expression continuation))))

(define (compile libraries macros expression)
  (compile-expression
    (make-compilation-context
      '()
      (unique
        (append
          (find-symbols expression)
          (find-quoted-symbols libraries)
          (find-quoted-symbols macros)))
      libraries
      macros)
    expression
    '()))

; Encoding

;; Context

(define-record-type encode-context
  (make-encode-context dictionary constants counts)
  encode-context?
  (dictionary encode-context-dictionary encode-context-set-dictionary!)
  (constants encode-context-constants encode-context-set-constants!)
  (counts encode-context-counts encode-context-set-counts!))

(define (encode-context-push! context value)
  (encode-context-set-dictionary!
    context
    (cons value (encode-context-dictionary context))))

(define (encode-context-remove! context index)
  (let* ((dictionary (cons #f (encode-context-dictionary context)))
         (cons (list-tail dictionary index))
         (value (cadr cons)))
    (set-cdr! cons (cddr cons))
    (encode-context-set-dictionary! context (cdr dictionary))
    value))

(define (encode-context-position context value)
  (memv-position value (encode-context-dictionary context)))

;; Codes

(define integer-base 128)
(define number-base 64)
(define tag-base 32)
(define share-base 31)

(define singletons '(#f #t ()))

(define (countable-shared-value? value)
  (or
    (char? value)
    ; TODO Include strings.
    ;(string? value)
    (symbol? value)))

(define (shared-value? value)
  (or
    (memq value singletons)
    (procedure? value)
    (countable-shared-value? value)))

(define (nop-codes? codes)
  (and
    (rib? codes)
    (eqv? (rib-tag codes) nop-instruction)))

(define (terminal-codes? codes)
  (or (null? codes) (nop-codes? codes)))

(define (decrement-count! counts value)
  (when (countable-shared-value? value)
    ; TODO Use `assoc`.
    (let ((pair (assv value counts)))
      (when (not pair)
        (error "missing count" value))
      (set-cdr! pair (- (cdr pair) 1)))))

(define (count-constants codes)
  (define counts '())
  (define continuations '())

  (define (increment! value)
    (when (countable-shared-value? value)
      (cond
        ; TODO Use `assoc`.
        ((assv value counts) =>
          (lambda (pair)
            (set-cdr! pair (+ 1 (cdr pair)))))

        (else
          (set! counts (cons (cons value 1) counts))))))

  (define (count-data! value)
    (when (rib? value)
      (let ((counted (assv value counts)))
        (increment! value)
        ; TODO Use `assoc`.
        (when (or (not (shared-value? value)) (not counted))
          (count-data! (rib-cdr value))

          (when (not (symbol? value))
            (let ((head (rib-car value)))
              ((if (and (procedure? value) (rib? head)) count-code! count-data!) head)))))))

  (define (count-code! codes)
    (cond
      ((nop-codes? codes)
        ; TODO Remove an element.
        (when (not (memq codes continuations))
          (set! continuations (cons codes continuations))
          (count-code! (rib-cdr codes))))

      ((not (terminal-codes? codes))
        (if (= (rib-tag codes) if-instruction)
          (count-code! (rib-car codes))
          (count-data! (rib-car codes)))

        (count-code! (rib-cdr codes)))))

  (count-code! codes)

  counts)

(define (build-constant context x)
  (let ((constants (encode-context-constants context)))
    (cond
      ((not (symbol? x))
        x)

      ((assq x constants) =>
        cdr)

      (else
        (let ((y (data-rib symbol-type #f (symbol->string (resolve-library-symbol x)))))
          (encode-context-set-constants! context (cons (cons x y) constants))
          y)))))

(define (encode-integer-part integer base bit)
  (+ bit (* 2 (modulo integer base))))

(define (encode-integer-parts integer base)
  (let ((rest (quotient integer base)))
    (values
      (encode-integer-part integer base (if (zero? rest) 0 1))
      rest)))

(define (encode-integer-tail x)
  (do ((x x (quotient x integer-base)))
    ((zero? x))
    (write-u8
      (encode-integer-part
        x
        integer-base
        (if (zero? (quotient x integer-base)) 0 1)))))

(define (encode-number x)
  (cond
    ((and (integer? x) (negative? x))
      (+ 1 (* 4 (abs x))))

    ((integer? x)
      (* 2 x))

    (else
      (error "float not supported"))))

(define (encode-node context value data)
  (let* ((counts (encode-context-counts context))
         (decrement!
           (lambda ()
             (when data
               (decrement-count! counts value))))
         (original-value value)
         (value (if data (build-constant context value) value)))
    (cond
      ((rib? value)
        (let* ((branch (and (not data) (nop-codes? value)))
               (shared
                 (or
                   branch
                   (null? value)
                   (and data (shared-value? value)))))
          (cond
            ((and shared (encode-context-position context value)) =>
              (lambda (index)
                (decrement!)
                (let ((removed
                        (or
                          branch
                          (and
                            (countable-shared-value? value)
                            (zero? (cdr (assv original-value counts))))))
                      (value (encode-context-remove! context index)))
                  (when (not removed)
                    (encode-context-push! context value))
                  (let-values (((head tail)
                                 (encode-integer-parts
                                   (+ (* 2 index) (if removed 0 1))
                                   share-base)))
                    (write-u8 (+ 3 (* 4 (+ 1 head))))
                    (encode-integer-tail tail)))))

            (else
              (let ((tag (rib-tag value)))
                (encode-node context (rib-cdr value) data)
                (encode-node
                  context
                  (rib-car value)
                  (not
                    (if data
                      (target-procedure? value)
                      (eq? tag if-instruction))))

                (let-values (((head tail) (encode-integer-parts tag tag-base)))
                  (write-u8 (+ 1 (* 4 head)))
                  (encode-integer-tail tail))

                (when shared
                  (encode-context-push! context value)
                  (decrement!)
                  (write-u8 3)))))))

      (else
        (let-values (((head tail) (encode-integer-parts (encode-number value) number-base)))
          (write-u8 (* 2 head))
          (encode-integer-tail tail))))))

;; Primitives

(define (build-primitive primitive continuation)
  (code-rib
    constant-instruction
    (data-rib procedure-type (cadr primitive) '())
    (code-rib
      set-instruction
      (car primitive)
      continuation)))

(define (build-primitives primitives continuation)
  (if (null? primitives)
    continuation
    (build-primitive
      (car primitives)
      (build-primitives (cdr primitives) continuation))))

;; Main

(define (encode codes)
  (let* ((codes (cons #f (build-primitives primitives codes)))
         (context (make-encode-context '() '() (count-constants codes))))
    (encode-node context codes #f)

    (do ((counts (encode-context-counts context) (cdr counts)))
      ((null? counts))
      (when (not (zero? (cdar counts)))
        (error "invalid constant count")))))

; Main

(define (main)
  (define-values (expression1 library-context) (expand-libraries (read-source)))
  (define-values (expression2 macro-context) (expand-macros expression1))

  (encode
    (compile
      (map-values
        library-exports
        (map-values library-state-library (library-context-libraries library-context)))
      (reverse
        (filter
          (lambda (pair) (library-symbol? (car pair)))
          (macro-state-literals (macro-context-state macro-context))))
      expression2)))

(main)
