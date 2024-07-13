(import
  (scheme base)
  (scheme char)
  (scheme lazy)
  (scheme read)
  (scheme write)
  (scheme eval)
  (scheme repl)
  (scheme file)
  (scheme process-context))

(define (main)
  (define program (open-input-file (linux-ref (command-line) 1)))

  (do ()
    ((eof-object? (peek-char program))
      #f)
    (if (char-whitespace? (peek-char program))
      (read-char program)
      (eval (read program) (interaction-environment)))))

(main)
