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
  (only (stak mapping)))

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

  (do ((char (peek-char) (peek-char)))
    ((not (and (char? char) (char-whitespace? char)))
      char)
    (read-char)))

(define (main)
  (do ((char (prompt) (prompt)))
    ((or (eof-object? char) (eqv? char (integer->char 4))))
    (write-value
      (let ((expression
              (guard (error (else error))
                (read))))
        (if (error-object? expression)
          (begin
            ; Skip an erroneous line.
            (read-line)
            expression)
          (guard (error (else error))
            (eval expression (interaction-environment))))))
    (newline)
    (main)))

(let ((arguments (command-line)))
  (when (or
         (member "-h" arguments)
         (member "--help" arguments))
    (write-string "The Stak Scheme REPL interpreter.\n\n")
    (write-string "Usage: stak-repl\n")
    (exit)))

(main)
