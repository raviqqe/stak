(import
  (shake (scheme base))
  (shake (scheme char))
  (only (scheme cxr))
  (shake (scheme eval))
  (only (scheme file))
  (only (scheme inexact))
  (only (scheme lazy))
  (only (scheme process-context))
  (shake (scheme read))
  (shake (scheme repl))
  (only (scheme time))
  (shake (scheme write)))

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
