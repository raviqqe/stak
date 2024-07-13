(import
  (scheme base)
  (scheme char)
  (scheme read)
  (scheme write)
  (scheme eval)
  (scheme repl))

(define (main)
  (do ()
    ((eof-object? (peek-char))
      #f)
    (set! y (+ y (vector-ref xs i))))
  (do ()
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
