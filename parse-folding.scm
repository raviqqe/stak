(import
  (scheme base)
  (scheme read)
  (scheme write))

(define (parse-line line)
  line)

(define (parse)
  (map
    parse-line
    (let loop ()
      (let ((line (read-line)))
        (write line)
        (cond
          ((eof-object? line)
            '())
          ((or (equal? line "") (eqv? (string-ref line 0) #\#))
            (loop))
          (else
            (cons line (loop))))))))

(write (parse))
