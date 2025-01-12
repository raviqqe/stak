; We import all libraries here because the compiler strips any unimported libraries.
(import
  (scheme base)
  (scheme char)
  (only (scheme cxr))
  (only (scheme eval) eval)
  (scheme file)
  (only (scheme inexact))
  (only (scheme lazy))
  (only (scheme process-context) command-line)
  (scheme read)
  (scheme repl)
  (only (scheme time))
  (only (scheme write)))

(define (usage)
  (write-string "stak-compile, the Scheme-to-bytecode compiler for Stak Scheme.\n\n")
  (write-string "Usage: stak-compile < SOURCE_FILE > BYTECODE_FILE\n"))

(define (main)
  (define program (open-input-file (list-ref (command-line) 1)))

  (do ()
    ((eof-object? (peek-char program))
      #f)
    (if (char-whitespace? (peek-char program))
      (read-char program)
      (eval (read program) (interaction-environment)))))

(let ((arguments (command-line)))
  (when (> (length arguments) 2)
    (usage)
    (error "Too many script files"))
  (when (or
         (member "-h" arguments)
         (member "--help" arguments))
    (usage)
    (exit)))

(main)
