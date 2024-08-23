(import
  (scheme base)
  (scheme char)
  (scheme cxr)
  (scheme eval)
  (scheme file)
  (scheme inexact)
  (scheme lazy)
  (scheme process-context)
  (scheme read)
  (scheme repl)
  (scheme write))

(define (main)
  (define program (open-input-file (list-ref (command-line) 1)))

  (do ()
    ((eof-object? (peek-char program))
      #f)
    (if (char-whitespace? (peek-char program))
      (read-char program)
      (eval (read program) (interaction-environment)))))

(main)
