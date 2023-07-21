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

(define (compile-program context source)
  (let ((continue (lambda (context) (compile-program context (cdr source)))))
    (cond
      ((null? source)
        '())
      ((pair? (car source))
        (let ((expression (car source)))
          (case (car expression)
            ((define)
              (cons
                (compile-expression context (caddr expression))
                (continue context)))
            (else (todo)))))
      (else (continue context)))))

(define (compile source)
  (compile-program (make-context) source))

(define (expand source)
  source)

(write (compile (expand (read-all))))
