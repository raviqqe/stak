(import (scheme base) (scheme write))

(define constant-instruction 0)
(define get-instruction 1)
(define set-instruction 2)
(define if-instruction 3)
(define nop-instruction 4)
(define call-instruction 5)

(define stack '())

(define (decode-number byte)
  (set! stack (cons byte stack)))

(define (decode-ribs data)
  (do ((byte (read-u8) (read-u8)))
    ((eof-object? byte))
    (cond
      ((even? byte)
        (set! stack (cons byte stack)))

      (else
        (write-string "- ")
        (write byte)
        (newline)))))

(decode-ribs #f)
