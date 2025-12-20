(import
  (scheme base)
  (scheme char)
  (only (scheme complex))
  (only (scheme cxr))
  (scheme eval)
  (only (scheme file))
  (only (scheme inexact))
  (only (scheme lazy))
  (scheme process-context)
  (scheme read)
  (scheme repl)
  (only (scheme time))
  (scheme write)
  (only (srfi 1))
  (only (stak backtrace))
  (only (stak mapping))
  (only (stak radix-vector)))

(define (write-value value)
  (if (error-object? value)
    (begin
      (display "ERROR: ")
      (display (error-object-message value))
      (for-each
        (lambda (value)
          (write-char #\space)
          (write value))
        (error-object-irritants value)))
    (write value)))

(define (prompt)
  (display "> " (current-error-port))

  (guard
    (error
      (else
        ; Skip an erroneous line.
        (read-line)
        error))
    (read)))

(define (main)
  (do ((expression (prompt) (prompt)))
    ((eof-object? expression))
    (write-value
      (if (error-object? expression)
        expression
        (guard (error (else error))
          (eval expression (interaction-environment)))))
    (newline)))

(let ((arguments (command-line)))
  (when (or
         (member "-h" arguments)
         (member "--help" arguments))
    (write-string "The Stak Scheme REPL interpreter.\n\n")
    (write-string "Usage: stak-repl\n")
    (exit)))

(main)
