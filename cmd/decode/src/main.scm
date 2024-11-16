(import (scheme base) (scheme write))

(define (decode-ribs)
  (do ((byte (read-u8) (read-u8)))
    ((eof-object? byte))
    (write byte)
    (newline)))

(decode-ribs)
