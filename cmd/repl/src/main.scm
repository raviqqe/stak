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
