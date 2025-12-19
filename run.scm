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
  (only (stak mapping))
  (only (stak radix-vector)))

(define (run environment path)
  (define file (open-input-file path))

  (do ()
    ((eof-object? (peek-char file))
      (close-port file))
    (if (char-whitespace? (peek-char file))
      (read-char file)
      (eval (read file) environment))))

(define (main)
  (define environment (interaction-environment))

  (let loop ((arguments (cdr (command-line))))
    (cond
      ((null? arguments)
        (error "script file missing"))
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
