(import
  (scheme base)
  (scheme read)
  (scheme write)
  (scheme eval)
  (scheme repl))

(define (main)
  (display "> " (current-error-port))

  (let loop ()
    (let ((char (peek-char)))
      (if (char-whitespace? char)
        (begin
          (read-char)
          (loop))
        (if (or
             (eof-object? char)
             (eqv? char (integer->char 4)))
          #f
          (begin
            (write (eval (read) (interaction-environment)))
            (newline)
            (main)))))))

(main)
