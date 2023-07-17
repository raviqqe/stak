#!/usr/bin/env gsi

(define (read-all)
  (let ((x (read)))
    (if (eof-object? x)
      '()
      (cons x (read-all)))))

(define (compile source)
  (cond
    ((null? source) (display "compile end"))
    ((list? (car source)) (display #f))
    (else (compile (cdr source)))))

(compile (read-all))

(write-u8 65)
