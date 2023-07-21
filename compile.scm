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

(define (lookup variable variables index)
  (if (pair? variables)
    (if (eqv? (car variables) variable)
      index
      (lookup variable (cdr variables) (+ index 1)))
    variable))

(define (set-tail! values tail)
  (let ((next (cdr values)))
    (if (null? next)
      (set-cdr! values tail)
      (set-tail! next tail))))

(define (add-symbol! context symbol)
  (set-tail! context (list symbol)))

(define (compile-expression context expression continuation)
  (cond
    ((symbol? expression)
      (todo))

    ((pair? expression)
      (let ((first (car expr)))
        (cond
          ((eqv? first 'set!)
            (let* (
                (variable (cadr expression))
                (value (caddr expression))
                (v (lookup variable (context-variables context) 1)))
              (if (eqv? v var) ; global?
                (let ((g (live? var (ctx-live ctx))))
                  (if g
                    (if (and (constant? g) (not (assoc var (ctx-exports context))))
                      (gen-noop context continuation)
                      (compile-expression context value (gen-assign context v continuation)))
                    (gen-noop context continuatiokn)))
                (compile ctx val (gen-assign ctx v cont)))))

          (else
            (todo)))))

    (else
      (todo))))

(define (compile expression)
  (compile-expression (make-context) expression '()))

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
