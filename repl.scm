(import
  (scheme base)
  (scheme char)
  (only (scheme cxr))
  (scheme eval)
  (only (scheme file))
  (only (scheme inexact))
  (only (scheme lazy))
  (scheme process-context)
  (scheme read)
  (scheme repl)
  (only (scheme time))
  (scheme write))

(define (write-value value)
  (if (error-object? value)
    (begin
      (display "ERROR ")
      (display (error-object-message value)))
    (write value)))

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
          (write-value
            (guard (error (#t error))
              (eval (read) (interaction-environment))))
          (newline)
          (main))))))

(let ((arguments (command-line)))
  (when (or
         (member "-h" arguments)
         (member "--help" arguments))
    (write-string "The Stak Scheme REPL interpreter.\n\n")
    (write-string "Usage: stak-repl\n")
    (exit)))

(main)
