#!/usr/bin/env gsi

(define false-index 0)
(define true-index 1)
(define null-index 2)
(define rib-index 3)

(define (todo) (error "not implemented"))

(define (read-all)
  (let ((x (read)))
    (if (eof-object? x)
      '()
      (cons x (read-all)))))

(define (compile-expression block expression)
  (display expression)
  #f)

(define (compile-program block source)
  (let ((continue (lambda (block) (compile-program block (cdr source)))))
    (cond
      ((null? source)
        '())
      ((pair? (car source))
        (let ((expression (car source)))
          (case (car expression)
            ((define) (cons
                (compile-expression block (caddr expression))
                (continue block)))
            (else (todo)))))
      (else (continue block)))))

(define (compile source)
  (compile-program '() source))

(write (compile (read-all)))
