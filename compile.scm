#!/usr/bin/env gsi

(define (read-all)
  (let ((x (read)))
    (if (eof-object? x)
      '()
      (cons x (read-all)))))

(define (compile-program block source)
  (let ((continue (lambda () (compile-program block (cdr source)))))
    (cond
      ((null? source)
        '())
      ((pair? (car source))
        (let ((expression (car source)))
          (cons "tomato" (continue))))
      (else (continue)))))

(define (compile source)
  (compile-program '() source))

(write (compile (read-all)))
