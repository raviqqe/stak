(import (scheme base) (scheme read) (scheme write))

(define (read-all)
  (let ((x (read)))
    (if (eof-object? x)
      '()
      (cons x (read-all)))))

(define (write-all xs)
  (for-each
    (lambda (x)
      (begin
        (write x)
        (newline)))
    xs))

(write-all (read-all))
