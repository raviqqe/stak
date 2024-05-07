(import
  (scheme base)
  (scheme read)
  (scheme write)
  (scheme eval)
  (scheme repl))

(let loop ()
  (write (eval (read) (interaction-environment)))
  (newline)
  (loop))
