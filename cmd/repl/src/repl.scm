(import
  (scheme base)
  (scheme read)
  (scheme write)
  (scheme eval)
  (scheme repl))

(let loop ()
  (display "> " (current-error-port))
  (write (eval (read) (interaction-environment)))
  (newline)
  (loop))
