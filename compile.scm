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
    (define target-pair? pair?)
    (define target-procedure? procedure?))

  (else
    (define-record-type *rib*
      (rib type car cdr tag)
      rib?
      (type rib-type)
      (car rib-car)
      (cdr rib-cdr)
      (tag rib-tag))

    (define (cons-rib car cdr)
      (rib pair-type car cdr 0))

    (define (instance? value type)
      (and (rib? value) (eq? (rib-type value) type)))

    (define (target-pair? value)
      (instance? value pair-type))

    (define (target-procedure? value)
      (instance? value procedure-type))

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
; Only for encoding
(define close-instruction 6)
(define skip-instruction 7)

; Primitives

(define primitives
  '(($$cons 1)
    ($$close 2)
    ($$car 4)
    ($$- 13)
    ($$* 14)
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
  (rib pair-type car cdr tag))

(define (call-rib arity procedure continuation)
  (code-rib (+ call-instruction arity) procedure continuation))

(define (constant-rib constant continuation)
  (code-rib constant-instruction constant continuation))

(define (data-rib type car cdr)
  (rib type car cdr 0))

(define (make-procedure arity code environment)
  (data-rib procedure-type environment (cons-rib arity code)))

(define (procedure-code procedure)
  (rib-cdr (rib-cdr procedure)))

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
  (cond
    ((null? xs)
      '())

    ((pair? xs)
      (cons
        (relaxed-deep-map f (car xs))
        (relaxed-deep-map f (cdr xs))))

    (else
      (f xs))))

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

; Target code writing

(define (write-target codes)
  (for-each write-u8 codes))

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

(define (resolve-library-symbol name)
  (let* ((string (symbol->string name))
         (position (memv-position library-symbol-separator (string->list string))))
    (if position
      (string->symbol (string-copy string (+ position 1)))
      name)))

(define (build-library-name id name)
  (string-append
    (id->string id)
    (list->string (list library-symbol-separator))
    (symbol->string name)))

(define (rename-library-symbol context id name)
  (if (or
       (not id)
       (eqv? (string-ref (symbol->string name) 0) #\$))
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
  (make-compilation-context environment libraries macros)
  compilation-context?
  (environment compilation-context-environment)
  (libraries compilation-context-libraries)
  (macros compilation-context-macros))

(define (compilation-context-append-locals context variables)
  (make-compilation-context
    (append variables (compilation-context-environment context))
    (compilation-context-libraries context)
    (compilation-context-macros context)))

(define (compilation-context-push-local context variable)
  (compilation-context-append-locals context (list variable)))

; If a variable is not in environment, it is considered to be global.
(define (compilation-context-resolve context variable)
  (or (memv-position variable (compilation-context-environment context)) variable))

;; Procedures

(define (compile-arity argument-count variadic)
  (+
    (* 2 argument-count)
    (if variadic 1 0)))

(define (compile-primitive-call name continuation)
  (call-rib
    (compile-arity
      (case name
        (($$close $$car $$exp $$log)
          1)

        (($$cons $$- $$*)
          2)

        (($$rib)
          4)

        (else
          (error "unknown primitive" name)))
      #f)
    name
    continuation))

(define (drop? codes)
  (and
    (target-pair? codes)
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
            (code-rib
              if-instruction
              (compile-expression
                context
                (caddr expression)
                (if (null? continuation) '() (code-rib nop-instruction 0 continuation)))
              (compile-expression context (cadddr expression) continuation))))

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

        (else
          (compile-call context expression #f continuation))))

    (else
      (constant-rib expression continuation))))

(define (compile libraries macros expression)
  (compile-expression
    (make-compilation-context '() libraries macros)
    expression
    '()))

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
    (string->uninterned-symbol (string-append "$" (id->string id)))))

;; Main

; We do not need to check boolean and null which are registered as default constants.
(define (constant-normal? constant)
  (or
    (symbol? constant)
    (and
      (integer? constant)
      (not (negative? constant)))
    (target-procedure? constant)))

(define (build-child-constants context car cdr continue)
  (define (build-child constant continue)
    (build-constant-codes context constant continue))

  (build-child
    car
    (lambda ()
      (build-child
        cdr
        continue))))

(define (fraction x)
  (- x (floor x)))

; TODO Why not 51 instead of 49?
(define maximum-float-integer (expt 2 49))

(define (decompose-float x)
  (define (mantissa y)
    (/ x (expt 2 y)))

  (do ((y (log x 2) (- y 1)))
    ((or
        (< (fraction (mantissa (floor y))) epsilon)
        (> (mantissa (+ y 1)) maximum-float-integer))
      (let ((y (floor y)))
        (values (exact (round (mantissa y))) (exact y))))))

(define (build-number-constant constant continue)
  (cond
    ((negative? constant)
      (constant-rib
        0
        (build-number-constant
          (abs constant)
          (lambda ()
            (compile-primitive-call '$$- (continue))))))

    ((not (integer? constant))
      (let-values (((x y) (decompose-float constant)))
        (constant-rib
          x
          (build-number-constant
            y
            (lambda ()
              (constant-rib
                2
                (compile-primitive-call
                  '$$log
                  (compile-primitive-call
                    '$$*
                    (compile-primitive-call
                      '$$exp
                      (compile-primitive-call
                        '$$*
                        (continue)))))))))))

    (else
      (constant-rib constant (continue)))))

(define (build-constant-codes context constant continue)
  (define (build-rib type car cdr)
    (constant-rib
      type
      (build-child-constants
        context
        car
        cdr
        (lambda ()
          (constant-rib
            0
            (compile-primitive-call '$$rib (continue)))))))

  (let ((symbol (constant-context-constant context constant)))
    (if symbol
      (code-rib get-instruction symbol (continue))
      (cond
        ((constant-normal? constant)
          (constant-rib constant (continue)))

        ((bytevector? constant)
          (build-rib
            bytevector-type
            (bytevector->list constant)
            (bytevector-length constant)))

        ((char? constant)
          (build-rib char-type '() (char->integer constant)))

        ((and
            (number? constant)
            (or
              (not (integer? constant))
              (negative? constant)))
          (build-number-constant constant continue))

        ((pair? constant)
          (build-child-constants
            context
            (car constant)
            (cdr constant)
            (lambda () (compile-primitive-call '$$cons (continue)))))

        ((string? constant)
          (constant-rib
            (string->symbol constant)
            (compile-primitive-call '$$car (continue))))

        ((vector? constant)
          (build-rib
            vector-type
            (vector->list constant)
            (vector-length constant)))

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
          (code-rib set-instruction id (continue)))))))

(define (build-constants context codes)
  (let loop ((codes codes) (continue (lambda () codes)))
    (if (terminal-codes? codes)
      (continue)
      (let* ((instruction (rib-tag codes))
             (operand (rib-car codes))
             (codes (rib-cdr codes))
             (continue (lambda () (loop codes continue))))
        (cond
          ((eq? instruction constant-instruction)
            (build-constant
              context
              operand
              (if (target-procedure? operand)
                (lambda () (loop (procedure-code operand) continue))
                continue)))

          ((eq? instruction if-instruction)
            (loop operand continue))

          (else
            (continue)))))))

; Encoding

;; Utility

(define (find-symbols constant-symbols codes)
  (let loop ((codes codes) (symbols '()))
    (if (terminal-codes? codes)
      symbols
      (let ((instruction (rib-tag codes))
            (operand (rib-car codes)))
        (loop
          (rib-cdr codes)
          (cond
            ((and
                (eq? instruction constant-instruction)
                (target-procedure? operand))
              (loop (procedure-code operand) symbols))

            ((eq? instruction if-instruction)
              (loop operand symbols))

            ((and
                (symbol? operand)
                (not (memq operand default-symbols))
                (not (memq operand constant-symbols))
                (not (memq operand symbols)))
              (cons operand symbols))

            (else
              symbols)))))))

(define (nop-codes? codes)
  (and
    (target-pair? codes)
    (eq? (rib-tag codes) nop-instruction)))

(define (terminal-codes? codes)
  (or (null? codes) (nop-codes? codes)))

(define (find-continuation codes)
  (cond
    ((null? codes)
      '())

    ((nop-codes? codes)
      (rib-cdr codes))

    (else
      (find-continuation (rib-cdr codes)))))

(define (count-skips codes continuation)
  (do ((codes codes (rib-cdr codes)) (count 0 (+ 1 count)))
    ((eq? codes continuation)
      count)))

;; Context

(define-record-type encode-context
  (make-encode-context symbols constant-context)
  encode-context?
  (symbols encode-context-symbols encode-context-set-symbols!)
  (constant-context encode-context-constant-context))

(define (encode-context-constant context constant)
  (constant-context-constant (encode-context-constant-context context) constant))

;; Symbols

(define symbol-separator (- 256 2))
(define symbol-terminator (- 256 1))

(define (encode-string string target)
  (if (null? string)
    target
    (encode-string (cdr string) (cons (char->integer (car string)) target))))

(define (encode-symbol symbol target)
  (encode-string (string->list (symbol->string symbol)) target))

(define (encode-symbols symbols constant-symbols target)
  (let ((target (cons symbol-terminator target)))
    (encode-integer
      (length constant-symbols)
      (cdr
        (fold-left
          (lambda (target symbol)
            (cons
              symbol-separator
              (encode-symbol symbol target)))
          target
          symbols)))))

;; Codes

(define integer-base 128)
(define short-integer-base 8)

(define (encode-integer-part integer base bit)
  (+ bit (* 2 (modulo integer base))))

(define (encode-integer-with-base integer base target)
  (do ((x (quotient integer base) (quotient x integer-base))
       (bit 0 1)
       (target target (cons (encode-integer-part x integer-base bit) target)))
    ((zero? x)
      (values (encode-integer-part integer base bit) target))))

(define (encode-short-integer integer target)
  (encode-integer-with-base integer short-integer-base target))

(define (encode-integer integer target)
  (let-values (((byte target) (encode-integer-with-base integer integer-base target)))
    (cons byte target)))

(define (encode-instruction instruction integer return target)
  (let-values (((integer target) (encode-short-integer integer target)))
    (cons (+ (if return 1 0) (* 2 instruction) (* 16 integer)) target)))

(define (encode-procedure context procedure return target)
  (let ((code (rib-cdr procedure)))
    (encode-codes
      context
      (rib-cdr code)
      (encode-instruction
        close-instruction
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

(define (encode-codes context codes target)
  (if (terminal-codes? codes)
    target
    (let* ((instruction (rib-tag codes))
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
        (cond
          ((and
              (eq? instruction constant-instruction)
              (target-procedure? operand))
            (encode-procedure context operand return target))

          ((memq instruction (list get-instruction set-instruction))
            (encode-simple instruction))

          ((eq? instruction constant-instruction)
            (let ((symbol (encode-context-constant context operand)))
              (if symbol
                (encode-instruction
                  get-instruction
                  (encode-operand context symbol)
                  return
                  target)
                (encode-simple constant-instruction))))

          ((eq? instruction if-instruction)
            (let ((continuation (find-continuation operand))
                  (target
                    (encode-codes
                      context
                      operand
                      (encode-instruction if-instruction 0 #f target))))
              (if (null? continuation)
                target
                (encode-instruction skip-instruction (count-skips codes continuation) #t target))))

          ((eq? instruction nop-instruction)
            (error "unexpected nop instruction"))

          (else
            (encode-instruction
              call-instruction
              (- instruction call-instruction)
              return
              (encode-integer (encode-operand context operand) target))))))))

;; Primitives

(define (build-primitive primitive continuation)
  (constant-rib
    procedure-type
    (constant-rib
      '()
      (constant-rib
        (cadr primitive)
        (constant-rib
          0
          (compile-primitive-call
            '$$rib
            (code-rib set-instruction (car primitive) continuation)))))))

(define (build-primitives primitives continuation)
  (if (null? primitives)
    continuation
    (build-primitive
      (car primitives)
      (build-primitives (cdr primitives) continuation))))

;; Main

(define (encode codes)
  (let* ((constant-context (make-constant-context '() 0))
         (codes
           (build-primitives
             primitives
             (build-constants constant-context codes)))
         (constant-symbols (map cdr (constant-context-constants constant-context)))
         (symbols (append default-symbols (find-symbols constant-symbols codes))))
    (encode-symbols
      symbols
      constant-symbols
      (encode-codes
        (make-encode-context
          (append symbols constant-symbols)
          constant-context)
        codes
        '()))))

; Main

(define (marshall-library library)
  (let* ((id (library-id library))
         (exports
           (map
             (lambda (pair)
               (let ((name (car pair))
                     (denotation (cdr pair)))
                 (cons
                   name
                   (and
                     (not
                       (equal?
                         (build-library-name id name)
                         (symbol->string denotation)))
                     denotation))))
             (library-exports library))))
    (list
      id
      (map car (filter-values not exports))
      (filter-values (lambda (denotation) denotation) exports))))

(define (main)
  (define-values (expression1 library-context) (expand-libraries (read-source)))
  (define-values (expression2 macro-context) (expand-macros expression1))

  (write-target
    (encode
      (compile
        (map-values
          marshall-library
          (map-values library-state-library (library-context-libraries library-context)))
        ; TODO Exclude unqualified symbols.
        (reverse (macro-state-literals (macro-context-state macro-context)))
        expression2))))

(main)
