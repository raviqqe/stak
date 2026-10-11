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
  (only (stak compile) library-paths)
  (only (stak mapping)))

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
      ((equal? (substring (car arguments) 0 1) "-")
        (let ((option (substring (car arguments) 1)))
          (cond
            ((equal? option "A")
              (library-paths (append (library-paths) (list (cadr arguments)))))
            ((equal? option "I")
              (library-paths (cons (cadr arguments) (library-paths))))
            ((equal? option "l")
              (run environment (cadr arguments)))
            (else
              (error "unknown option" (car arguments)))))
        (loop (cddr arguments)))
      (else
        (set! command-line (lambda () arguments))
        (run environment (car arguments))))))

(let ((arguments (command-line)))
  (when (or
         (member "-h" arguments)
         (member "--help" arguments))
    (write-string "The Stak Scheme interpreter.\n\n")
    (write-string "Usage: stak [-A DIRECTORY] [-I DIRECTORY] [-l LIBRARY_FILE] SCRIPT_FILE ARGUMENT...\n")
    (exit)))

(main)
