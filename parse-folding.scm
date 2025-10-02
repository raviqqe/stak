(import
  (scheme base)
  (scheme read)
  (scheme write))

(define (parse-tokens)
  (do ((character (peek-char)))
    ((memv character '(#\; #\space)))
    (read-char))
  (let ((token (read)))
    (if (eof-object? token)
      '()
      (cons token (loop)))))

(define (parse)
  (map
    (lambda (line)
      (parameterize ((current-input-port (open-input-string line)))
        (parse-tokens)))
    (let loop ()
      (let ((line (read-line)))
        (cond
          ((eof-object? line)
            '())
          ((or (equal? line "") (eqv? (string-ref line 0) #\#))
            (loop))
          (else
            (cons line (loop))))))))

(write (parse))
