(import
  (scheme base)
  (scheme read)
  (scheme write)
  (scheme eval)
  (scheme repl))

(let loop ()
  (display "> " (current-error-port))

  (let ((char (peek-char)))
    (if (or
         (eof-object? char)
         (= (char->integer char) 4))
      #f
      (begin
        (write (eval (read) (interaction-environment)))
        (newline)
        (read-char)
        (loop)))))
