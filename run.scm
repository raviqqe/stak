; We import all libraries here because the compiler strips any unimported libraries.
(import
  (scheme base)
  (only (scheme case-lambda))
  (scheme char)
  (only (scheme cxr))
  (only (scheme eval) eval)
  (scheme file)
  (only (scheme inexact))
  (only (scheme lazy))
  (scheme process-context)
  (only (scheme r5rs))
  (scheme read)
  (scheme repl)
  (only (scheme time))
  (only (scheme write)))

(define (run path)
  (define file (open-input-file path))

  (do ()
    ((eof-object? (peek-char file))
      #f)
    (if (char-whitespace? (peek-char file))
      (read-char file)
      (eval (read file) (interaction-environment)))))

(define (main)
  (do ((arguments (cdr (command-line)) (cddr arguments)))
    ((not (equal? (car arguments) "-l"))
      (run (car arguments)))
    (run (cadr arguments))))

(let ((arguments (command-line)))
  (when (or
         (member "-h" arguments)
         (member "--help" arguments))
    (write-string "The Stak Scheme interpreter.\n\n")
    (write-string "Usage: stak SOURCE_FILE [ARGUMENTS]...\n")
    (exit)))

(main)
