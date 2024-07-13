(import
  (scheme base)
  (scheme char)
  (scheme read)
  (scheme write)
  (scheme eval)
  (scheme repl)
  (scheme file)
  (scheme process-context))

(define (main)
  (define program
    (open-input-file
      (car
        (member
          #f
          (command-line)
          (lambda (x y)
            (let ((length (string-length y)))
              (equal? (substring y (- length 4) length) ".scm")))))))

  (do ()
    ((eof-object? (peek-char program))
      #f)
    (if (char-whitespace? (peek-char program))
      (read-char program)
      (eval (read program) (interaction-environment)))))

(main)
