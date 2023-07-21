(define false-index 0)
(define true-index 1)
(define null-index 2)
(define rib-index 3)

(define call-instruction 0)
(define set-instruction 1)
(define get-instruction 2)
(define constant-instruction 3)
(define if-instruction 4)

(define (todo) (error "not implemented"))

(define (read-all)
  (let ((x (read)))
    (if (eof-object? x)
      '()
      (cons x (read-all)))))

; (block . symbols)
(define (make-context)
  (cons '() '()))

(define (set-tail! values tail)
  (let ((next (cdr values)))
    (if (null? next)
      (set-cdr! values tail)
      (set-tail! next tail))))

(define (add-symbol! context symbol)
  (set-tail! context (list symbol)))

(define (compile-expression context expression)
  (display expression)
  expression)

(define (compile-program context expression continuation)
  (cond
    ((symbol? expr)
      (let ((v (lookup expr (ctx-cte ctx) 0)))
        (if (eqv? v expr) ;; global?
          (let ((g (live? expr (ctx-live ctx))))
            (if (and g (constant? g)) ;; constant propagated?
              (c-rib const-op (cadr (cadr g)) cont)
              (c-rib get-op v cont)))
          (c-rib get-op v cont))))

    ((pair? expr)
      (let ((first (car expr)))

        (cond ((eqv? first 'quote)
            (c-rib const-op (cadr expr) cont))

          ((eqv? first 'set!)
            (let ((var (cadr expr)))
              (let ((val (caddr expr)))
                (let ((v (lookup var (ctx-cte ctx) 1)))
                  (if (eqv? v var) ;; global?
                    (let ((g (live? var (ctx-live ctx))))
                      (if g
                        (if (and (constant? g)
                            (not (assoc var (ctx-exports ctx))))
                          (begin
                            ;;                                        (pp `(*** constant propagation of ,var = ,(cadr g))
                            ;;                                             (current-error-port))
                            (gen-noop ctx cont))
                          (comp ctx val (gen-assign ctx v cont)))
                        (begin
                          ;;                                    (pp `(*** removed dead assignment to ,var)
                          ;;                                         (current-error-port))
                          (gen-noop ctx cont))))
                    (comp ctx val (gen-assign ctx v cont)))))))

          ((eqv? first 'if)
            (let ((cont-false (comp ctx (cadddr expr) cont)))
              (let ((cont-true (comp ctx (caddr expr) cont)))
                (let ((cont-test (c-rib if-op cont-true cont-false)))
                  (comp ctx (cadr expr) cont-test)))))

          ((eqv? first 'lambda)
            (let* ((params (cadr expr))
                (variadic (or (symbol? params) (not (eq? (last-item params) '()))))
                (nb-params
                  (if variadic
                    (improper-length params)
                    (length params)))
                (params
                  (if variadic
                    (improper-list->list params '())
                    params)))
              (c-rib const-op
                (make-procedure
                  (c-rib (+ (* 2 nb-params) (if variadic 1 0))
                    0
                    (comp-begin (ctx-cte-set
                        ctx
                        (extend params
                          (cons #f
                            (cons #f
                              (ctx-cte ctx)))))
                      (cddr expr)
                      tail))
                  '())
                (if (null? (ctx-cte ctx))
                  cont
                  (add-nb-args
                    ctx
                    1
                    (gen-call (use-symbol ctx 'close)
                      cont))))))

          ((eqv? first 'begin)
            (comp-begin ctx (cdr expr) cont))

          ((eqv? first 'let)
            (let ((bindings (cadr expr)))
              (let ((body (cddr expr)))
                (comp-bind ctx
                  (map car bindings)
                  (map cadr bindings)
                  body
                  cont))))

          (else
            (let ((args (cdr expr)))
              (if (symbol? first)
                (comp-call ctx
                  args
                  (lambda (ctx)
                    (let ((v (lookup first (ctx-cte ctx) 0)))
                      (add-nb-args ctx
                        (length args)
                        (gen-call
                          (if (and (number? v)
                              (memq 'arity-check (ctx-live-features ctx)))
                            (+ v 1)
                            v)
                          cont)))))
                (comp-bind ctx
                  '(_)
                  (cons first '())
                  (cons (cons '_ args) '())
                  cont)))))))

    (else
      (c-rib const-op expr cont))))

(define (compile source)
  (compile-program (make-context) source))

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

          (else
            expression))))

    (else
      expression)))

(write (compile (expand (read-all))))
