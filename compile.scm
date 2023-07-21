; Constants

(define false-index 0)
(define true-index 1)
(define null-index 2)
(define rib-index 3)

; Instructions

(define call-instruction 0)
(define set-instruction 1)
(define get-instruction 2)
(define constant-instruction 3)
(define if-instruction 4)

; Utility

(define (todo value) (error "not implemented" value))

(define (rib tag car cdr)
  (cons (cons tag car) cdr))

; Source code reading

(define (read-all)
  (let ((x (read)))
    (if (eof-object? x)
      '()
      (cons x (read-all)))))

(define (read-source)
  (cons 'begin (read-all)))

; Non-primitive expansion

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

          ((eqv? first 'begin)
            (cons 'begin (map expand (cdr expression))))

          (else
            expression))))

    (else
      expression)))

; Context

; (block . symbols)
(define (make-context)
  (cons '() '()))

(define (resolve-variable* variables variable index)
  (cond
    ((null? variables)
      variable)

    ((eqv? (car variables) variable)
      index)

    (else
      (resolve-variable (cdr variables) variable (+ index 1)))))

(define (resolve-variable context variable index)
  (resolve-variable* (car context) variable index))

; Compilation

(define (compile-set variable continuation)
  (rib set-instruction variable continuation))

(define (compile-begin context expressions continuation)
  (compile-expression context
    (car expressions)
    (if (pair? (cdr expressions))
      (compile-begin context (cdr expressions) continuation)
      continuation)))

(define (compile-expression context expression continuation)
  (cond
    ((symbol? expression)
      (todo variable))

    ((pair? expression)
      (let ((first (car expression)))
        (cond
          ((eqv? first 'set!)
            (compile-expression context (caddr expression)
              (compile-set
                (resolve-variable context (cadr expression) 1)
                continuation)))

          ((eqv? first 'begin)
            (compile-begin context (cdr expression) continuation))

          (else
            (todo first)))))

    (else
      (rib constant-instruction expression continuation))))

(define (compile expression)
  (compile-expression (make-context) expression '()))

; Main

(write (compile (expand (read-source))))
