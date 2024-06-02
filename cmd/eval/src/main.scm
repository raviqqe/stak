(import
  (scheme base)
  (scheme char)
  (scheme read)
  (scheme write)
  (scheme eval)
  (scheme repl))

(let loop ()
  (let ((char (peek-char)))
    (cond
      ((char-whitespace? char)
        (read-char)
        (loop))

      ((eof-object? char)
        #f)

      (else
        (eval (read) (interaction-environment))
        (loop)))))
