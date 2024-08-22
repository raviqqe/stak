(import
  (scheme base)
  (scheme char)
  (scheme eval)
  (scheme read)
  (scheme repl)
  (scheme write))

(define (main)
  (display "> " (current-error-port))

  (let loop ()
    (let ((char (peek-char)))
      (cond
        ((char-whitespace? char)
          (read-char)
          (loop))

        ((or (eof-object? char) (eqv? char (integer->char 4)))
          #f)

        (else
          (write (eval (read) (interaction-environment)))
          (newline)
          (main))))))

(main)
