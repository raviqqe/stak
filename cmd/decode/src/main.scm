(import (scheme base) (scheme write))

(define (decode-ribs data)
  (do ((byte (read-u8) (read-u8)))
    ((eof-object? byte))
    (write-string "- ")
    (write byte)
    (newline)))

(decode-ribs #f)
