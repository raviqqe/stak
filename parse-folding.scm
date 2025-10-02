(import
  (scheme base)
  (scheme read)
  (scheme write))

(define (read-all)
  (map
    (lambda (line) line)
    (let loop ()
      (let ((line (read-line)))
        (cond
          ((eof-object? line)
            '())
          ((or (equal? line "") (eqv? (string-ref line 0) #\#))
            (loop))
          (else
            (cons line (loop))))))))

(write (read-all))
