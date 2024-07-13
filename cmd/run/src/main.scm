(import
  (scheme base)
  (scheme char)
  (scheme read)
  (scheme write)
  (scheme eval)
  (scheme repl))

(define (main)
  (do ()
    ((eof-object? (peek-char))
      #f)
    (if (char-whitespace? (peek-char))
      (read-char)
      (eval (read) (interaction-environment)))))

(main)
