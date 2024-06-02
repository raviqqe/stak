(import
  (scheme base)
  (scheme char)
  (scheme read)
  (scheme write)
  (scheme eval)
  (scheme repl))

(define (main)
  (let loop ()
    (let ((char (peek-char)))
      (cond
        ((char-whitespace? char)
          (read-char)
          (loop))

        ((eof-object? char)
          #f)

        (else
          (write (eval (read) (interaction-environment)))
          (newline)
          (main))))))

(main)
