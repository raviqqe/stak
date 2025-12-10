; We import all libraries here because the compiler strips any unimported libraries.
(import
  (scheme base)
  (only (scheme case-lambda))
  (scheme char)
  (only (scheme complex))
  (only (scheme cxr))
  (only (scheme eval) eval)
  (scheme file)
  (only (scheme inexact))
  (only (scheme lazy))
  (only (scheme load))
  (scheme process-context)
  (only (scheme r5rs))
  (scheme read)
  (scheme repl)
  (only (scheme time))
  (only (scheme write))
  (only (srfi 1))
  (only (stak backtrace))
  (only (stak mapping)))

(define (run environment path)
  (parameterize ((current-input-port (open-input-file path)))
    (do ()
      ((eof-object? (peek-char))
        #f)
      (if (char-whitespace? (peek-char))
        (read-char)
        (eval (read) environment)))))

(define (main)
  (define environment (interaction-environment))

  (let loop ((arguments (cdr (command-line))))
    (cond
      ((equal? (car arguments) "-l")
        (run environment (cadr arguments))
        (loop (cddr arguments)))
      (else
        (set! command-line (lambda () arguments))
        (run environment (car arguments))))))

(let ((arguments (command-line)))
  (when (or
         (member "-h" arguments)
         (member "--help" arguments))
    (write-string "The Stak Scheme interpreter.\n\n")
    (write-string "Usage: stak [-l LIBRARY_FILE] SCRIPT_FILE ARGUMENT...\n")
    (exit)))

(main)
