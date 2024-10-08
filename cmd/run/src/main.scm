; We import all libraries here because the compiler strips any unimported libraries.
(import
  (shake (scheme base))
  (shake (scheme char))
  (only (scheme cxr))
  (only (scheme eval) eval)
  (shake (scheme file))
  (only (scheme inexact))
  (only (scheme lazy))
  (only (scheme process-context) command-line)
  (scheme read)
  (scheme repl)
  (only (scheme time))
  (only (scheme write)))

(define (main)
  (define program (open-input-file (list-ref (command-line) 1)))

  (do ()
    ((eof-object? (peek-char program))
      #f)
    (if (char-whitespace? (peek-char program))
      (read-char program)
      (eval (read program) (interaction-environment)))))

(main)
