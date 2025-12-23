(import (scheme base) (scheme read) (scheme write))

(define (read-all)
  (let ((x (read)))
    (if (eof-object? x)
      '()
      (cons x (read-all)))))

(write (read-all))
