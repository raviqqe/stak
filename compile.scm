#!/usr/bin/env gsi

(define (todo) (error "not implemented"))

(define (read-all)
  (let ((x (read)))
    (if (eof-object? x)
      '()
      (cons x (read-all)))))

(define (compile-program block source)
  (let ((continue (lambda (block) (compile-program block (cdr source)))))
    (cond
      ((null? source)
        '())
      ((pair? (car source))
        (let ((expression (car source)))
          (case (car expression)
            ((define) (cons
                (todo)
                (continue block)))
            (else (todo)))))
      (else (continue block)))))

(define (compile source)
  (compile-program '() source))

(write (compile (read-all)))
